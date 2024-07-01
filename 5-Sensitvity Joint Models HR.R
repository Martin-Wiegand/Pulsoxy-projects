library("JM")
library("nlme")
library("survival")

merged_data_set = readRDS(rds_file("merged_data_set"))

merged_data_set_oxygen = merged_data_set %>%
  ungroup %>% rowwise %>%
  mutate(hr_adjusted_quantile = case_when(!is.na(hrpaed_) & !is.na(age) ~ hr_age_interpol(hrpaed_,age),
                              T ~ NA_real_)) %>%
  mutate(hr_adjusted = case_when(!is.na(hr_adjusted_quantile) ~ quantile(merged_data_set$hrpaed_,probs = hr_adjusted_quantile,na.rm = T),
                                 T ~ NA_real_)) %>%
  group_by(studysubjectid) %>%
  arrange(studysubjectid,daysfromrand) %>%
  fill(hb_,.direction = "down") %>%
  mutate(oxygen_delivery_quantile = spo2_ * hr_adjusted_quantile * hb_,
         oxygen_delivery_adjusted = spo2_ * hr_adjusted * hb_,
         oxygen_delivery = spo2_ * hrpaed_ * hb_) 

merged_data_set_oxygen = merged_data_set_oxygen %>%
  ungroup %>%
  mutate(oxygen_delivery_zscore = scale(oxygen_delivery_zscore))

saveRDS(merged_data_set_oxygen,rds_file("merged_data_set_oxygen_adj"))
merged_data_set_oxygen_adj = readRDS(rds_file("merged_data_set_oxygen_adj"))

#
joint_data_mort_hr = merged_data_set_oxygen %>%
  filter(!is.na(hrzscore),
         !is.na(daysfromrand),
         !is.na(pims3_death),
         !is.na(baseSpO2),
         !is.na(baseFiO2),
         !is.na(marp),
         !is.na(age)) %>%
  mutate(death30 = case_when(is.na(death30) ~ "Alive",
                             TRUE ~ death30)) %>%
  mutate(died = (death30 == "Died")) %>%
  group_by(studysubjectid) %>%
  mutate(time_to_death = abs(difftime(dod,dtrand,units = "days"))) %>%
  mutate(time_to_death = case_when(time_to_death < 30 ~ as.numeric(time_to_death),
                                   TRUE ~ 30)) %>%
  group_by(studysubjectid) %>%
  arrange(studysubjectid,daysfromrand) %>%
  filter(time_to_death > 0) %>%
  filter(daysfromrand < time_to_death) %>% 
  ungroup %>%
  mutate(age = case_when(age <= 1 ~ "Under 1",
                         age > 1 & age < 5 ~ "1-5",
                         age >= 5 ~ "5+")) %>%
  mutate(age = relevel(as.factor(age),ref = "Under 1"))

saveRDS(joint_data_mort_hr,rds_file("joint_data_mort_hr"))
joint_data_mort_hr = readRDS(rds_file("joint_data_mort_hr"))

############################### MORTALITY
# ---------------- Mixed model

lmeFit.mort.hr <- lme(hrzscore ~ daysfromrand, #+ trt + pims3_death + sites, 
                   data = joint_data_mort_hr ,
                   random =  ~  1 + daysfromrand | studysubjectid) 

summary(lmeFit.mort.hr)
saveRDS(lmeFit.mort.hr,rds_file("lmeFit.mort.hr"))

## LM plot
joint_data_mort_hr = joint_data_mort_hr %>% ungroup %>% mutate(pred_hr = lmeFit.mort.hr %>% predict) 

ggplot(data = joint_data_mort_hr) +
  geom_line(aes(x = daysfromrand,y = pred_hr,group = studysubjectid,linetype = death30),colour = "red") +
  geom_point(aes(x = daysfromrand,y = hrzscore),alpha = 0.25) +
  geom_abline(slope = -0.27829,intercept = 0.5311,colour = "darkblue",linewidth = 1) +
  theme_bw() +
  labs(x = "Days since randomisation",
       y = "Age-adjusted Heart Rate (z score)")

ggsave(plot_file("LinePlot_mort_hr.jpg"),width = cm(3),height = cm(2))


ggplot(data = joint_data_mort_hr[1:200,]) +
  geom_line(aes(x = daysfromrand,y = pred_hr,colour = studysubjectid,linetype = death30)) +
  geom_point(aes(x = daysfromrand,y = hrzscore),alpha = 0.25) +
  geom_abline(slope = -0.27829,intercept = 0.5311,colour = "darkblue",linewidth = 1) +
  theme_bw() +
  labs(x = "Days since randomisation",
       y = "Age-adjusted Heart Rate (z score)") +
  theme(legend.position = "bottom")
ggsave(plot_file("LinePlotSelection_mort_hr.jpg"),width = cm(3),height = cm(2))

# --------------- Survival
survival_data_mort_hr <- joint_data_mort_hr %>%
  group_by(studysubjectid) %>%
  reframe(time_to_event = max(daysfromrand,na.rm = T),
          time_to_death = max(time_to_death,na.rm = T),
          died = as.numeric(any(died)),
          pims3_death = mean(pims3_death),
          baseSpO2 = mean(baseSpO2),
          basemarp = mean(marp),
          baseFiO2 = mean(baseFiO2),
          trt = unique(trt),
          age = unique(age))

# Build survival model
survFit.mort.hr <- coxph(Surv(time_to_death, died) ~ trt + scale(baseSpO2) + scale(basemarp) + scale(baseFiO2) + age + scale(pims3_death), 
                      data = survival_data_mort_hr,
                      x = TRUE,
                      model = TRUE)  

saveRDS(survFit.mort.hr,rds_file("survFit.mort.hr"))

survFit.mort.hr = readRDS(rds_file("survFit.mort.hr"))

# Joint model
lmeFit.mort.hr <- lme(hrzscore ~ daysfromrand, #+ trt + pims3_death + sites, 
                      data = joint_data_mort_hr ,
                      random =  ~  1 + daysfromrand | studysubjectid) 

survFit.mort.hr <- coxph(Surv(time_to_death, died) ~ trt + scale(baseSpO2) + scale(basemarp) + scale(baseFiO2) + age + scale(pims3_death), 
                         data = survival_data_mort_hr,
                         x = TRUE,
                         model = TRUE) 

dForm <- list(fixed = ~ 1, indFixed = c(2, 4), random = ~ 1, indRandom = 2)

jointFit.mort.hr.pw <- jointModel(lmeFit.mort.hr, 
                                  survFit.mort.hr, 
                                  parameterization = "both",
                                  derivForm = dForm,
                                  timeVar = "daysfromrand",
                                  method = "piecewise-PH-aGH")

jointFit.mort.hr.wb <- jointModel(lmeFit.mort.hr, 
                                  survFit.mort.hr, 
                               timeVar = "daysfromrand",
                               method = "weibull-PH-GH")

jointFit.mort.hr.spl <- jointModel(lmeFit.mort.hr, 
                                   survFit.mort.hr, parameterization = 
                                timeVar = "daysfromrand",
                                method = "spline-PH-aGH")

summary(jointFit.mort.hr.pw)
summary(jointFit.mort.hr.wb)
summary(jointFit.mort.hr.spl)

saveRDS(jointFit.mort.hr.pw,rds_file("jointFit.mort.hr.pw"))
saveRDS(jointFit.mort.hr.wb,rds_file("jointFit.mort.hr.wb"))
saveRDS(jointFit.mort.hr.spl,rds_file("jointFit.mort.hr.spl"))

############################### ORGAN SUPPORT LIBERATION

# Create joint data - patients + obs data,
# omitting:
# Missing covariates
# Missing outcomes
# Liberation time negative
# Measurement time after liberation time

# No deaths < 30 days
# No missing liberation time
# No liberation time > 30
# No measurement time > 30
# No Measurement time after liberation
joint_data_lib_hr = merged_data_set_oxygen %>%
  filter(!is.na(hrzscore),
         !is.na(daysfromrand),
         !is.na(pims3_death),
         !is.na(baseSpO2),
         !is.na(baseFiO2),
         !is.na(marp),
         !is.na(age)) %>%
  filter(!is.na(lib_event)) %>%
  mutate(daystolib = hrstolib/24) %>%
  group_by(studysubjectid) %>%
  arrange(studysubjectid,daysfromrand) %>%
  filter(daystolib > 0) %>%
  filter(daysfromrand < daystolib) %>% ungroup %>%
  filter(death30 == "Alive") %>%
  filter(daystolib < 30) %>%
  mutate(age = case_when(age <= 1 ~ "Under 1",
                         age > 1 & age < 5 ~ "1-5",
                         age >= 5 ~ "5+")) %>%
  mutate(age = relevel(as.factor(age),ref = "Under 1"))

saveRDS(joint_data_lib_hr,rds_file("joint_data_lib_hr"))
joint_data_lib_hr = readRDS(rds_file("joint_data_lib_hr"))


# ---------------- Mixed model

lmeFit.lib.hr <- lme((hrzscore) ~ daysfromrand, #+ trt + pims3_death + sites, 
                  data = joint_data_lib_hr,
                  random =  ~  1 + daysfromrand | studysubjectid) 

summary(lmeFit.lib.hr)
saveRDS(lmeFit.lib.hr,rds_file("lmeFit.lib.hr"))

## LM plot
joint_data_lib_hr <- joint_data_lib_hr %>% ungroup %>% mutate(pred_hr = lmeFit.lib.hr %>% predict) 

ggplot(data = joint_data_lib_hr) +
  geom_line(aes(x = daysfromrand,y = pred_hr,group = studysubjectid),colour = "red") +
  geom_point(aes(x = daysfromrand,y = hrzscore),alpha = 0.25) +
  geom_abline(slope = -0.3293783,intercept = 0.4585613 ,colour = "darkblue",linewidth = 1) +
  theme_bw() +
  labs(x = "Days since randomisation",
       y = "Age-adjusted Heart Rate (z score)",
       linetype = "") +
  theme(legend.position = "bottom")

ggsave(plot_file("LinePlot_lib_hr.jpg"),width = cm(3),height = cm(2))


ggplot(data = joint_data_lib_hr[1:200,]) +
  geom_line(aes(x = daysfromrand,y = pred_hr,colour = studysubjectid)) +
  geom_point(aes(x = daysfromrand,y = hrzscore),alpha = 0.25) +
  geom_abline(slope = -0.3293783,intercept = 0.4585613 ,colour = "darkblue",linewidth = 1) +
  theme_bw() +
  labs(x = "Days since randomisation",
       y = "Age-adjusted Heart Rate (z score)",
       colour = "") 
ggsave(plot_file("LinePlotSelection_lib_hr.jpg"),width = cm(3),height = cm(2))




# --------------- Survival
survival_data_lib_hr <- joint_data_lib_hr %>%
  # mutate(lib_event = case_when(lib_event == 1 & death30 == "Alive" ~ 1,
  #                              TRUE ~ 0)) %>%
  group_by(studysubjectid) %>%
  reframe(time_to_lib = max(daystolib,na.rm = T),
          death = any(death == "Died"),
          ethnic = unique(ethnic),
          lib_event = any(lib_event == 1),
          pims3_death = mean(pims3_death),
          baseSpO2 = mean(baseSpO2),
          basemarp = mean(marp),
          baseFiO2 = mean(baseFiO2),
          supportdeath = unique(supportdeath),
          trt = unique(trt),
          age = unique(age))

# Build survival model
survFit.lib.hr <- coxph(Surv(time_to_lib, lib_event) ~ trt + scale(baseSpO2) + scale(basemarp) + scale(baseFiO2) + age + scale(pims3_death), 
                     data = survival_data_lib_hr,
                     x = TRUE,model = TRUE)  

saveRDS(survFit.lib.hr,rds_file("survFit.lib.hr"))

# Joint model
jointFit.lib.hr.p1 <- jointModel(lmeFit.lib.hr, 
                              survFit.lib.hr, 
                              timeVar = "daysfromrand",
                              method = "piecewise-PH-aGH")

jointFit.lib.hr.wb <- jointModel(lmeFit.lib.hr, 
                                 survFit.lib.hr, 
                              timeVar = "daysfromrand",
                              method = "weibull-PH-GH")

jointFit.lib.hr.spl <- jointModel(lmeFit.lib.hr, 
                                  survFit.lib.hr, 
                               timeVar = "daysfromrand",
                               method = "spline-PH-aGH")

summary(jointFit.lib.hr.p1)
summary(jointFit.lib.hr.wb)
summary(jointFit.lib.hr.spl)

saveRDS(jointFit.lib.hr.p1,rds_file("jointFit.lib.hr.p1"))
saveRDS(jointFit.lib.hr.wb,rds_file("jointFit.lib.hr.wb"))
saveRDS(jointFit.lib.hr.spl,rds_file("jointFit.lib.hr.spl"))




####### Print results
# Mort
lm.mort.hr.table = (lmeFit.mort.hr %>% intervals)$fixed %>% as.tibble %>% mutate(coef = c("Intercept","Days since randomisation"),
                                                                                pval = 0) %>%
  rename(est = est.) %>%
  dplyr::select(coef,est,lower,upper,pval)

surv.mort.hr.table = summary(survFit.mort.hr)$coefficients %>% 
  as.tibble %>%
  rename(se = 'se(coef)',
         pval = 'Pr(>|z|)',
         est = coef) %>%
  mutate(lower = est - 1.96*se) %>%
  mutate(upper = est + 1.96*se) %>%
  mutate(coef = survFit.mort.hr$coefficients %>% names) %>%
  dplyr::select(coef,est,lower,upper,pval)
  
jf.mort.hr.table = summary(jointFit.mort.hr.pw)$`CoefTable-Event`

jf.mort.hr.table = (jf.mort.hr.table %>%
  as.tibble %>%
  mutate(lower = Value - Std.Err*1.96) %>%
  mutate(upper = Value + Std.Err*1.96) %>%
  mutate(coef = rownames(jf.mort.hr.table)) %>%
  rename(pval = 'p-value',
         est = Value) %>%
  dplyr::select(coef,est,lower,upper,pval))[1:8,]


lm.mort.hr.table
surv.mort.hr.table
jf.mort.hr.table


# Liberation
lm.lib.hr.table = (lmeFit.lib.hr %>% intervals)$fixed %>% as.tibble %>% mutate(coef = c("Intercept","Days since randomisation"),
                                                                                pval = 0) %>%
  rename(est = est.) %>%
  dplyr::select(coef,est,lower,upper,pval)

surv.lib.hr.table = summary(survFit.lib.hr)$coefficients %>% 
  as.tibble %>%
  rename(se = 'se(coef)',
         pval = 'Pr(>|z|)',
         est = coef) %>%
  mutate(lower = est - 1.96*se) %>%
  mutate(upper = est + 1.96*se) %>%
  mutate(coef = survFit.lib.hr$coefficients %>% names) %>%
  dplyr::select(coef,est,lower,upper,pval)

jf.lib.hr.table = summary(jointFit.lib.hr.p1)$`CoefTable-Event`

jf.lib.hr.table = (jf.lib.hr.table %>%
                     as.tibble %>%
                     mutate(lower = Value - Std.Err*1.96) %>%
                     mutate(upper = Value + Std.Err*1.96) %>%
                     mutate(coef = rownames(lib.hr.table)) %>%
                     rename(pval = 'p-value',
                            est = Value) %>%
                     dplyr::select(coef,est,lower,upper,pval))[1:8,]


lm.lib.hr.table
surv.lib.hr.table
jf.lib.hr.table




##### First measurement
first_oxygen_delivery =
  merged_data_set_oxygen_adj %>%
  group_by(studysubjectid) %>%
  arrange(daysfromrand) %>%
  filter(!is.na(oxygen_delivery_adjusted),
         daysfromrand <= 1) %>%
  mutate(trt = case_when(trt == "L" ~ "Liberal",
                         trt == "C" ~ "Conservative")) %>%
  mutate(oxygen_delivery_adjusted = oxygen_delivery_adjusted/10000) %>%
  dplyr::slice(1)

first_bars = 
first_oxygen_delivery %>%
  group_by(trt) %>%
  mutate(oxygen_delivery_adjusted = oxygen_delivery_adjusted %>% plyr::round_any(10)) %>% 
  mutate(groups = n()) %>%
  group_by(trt,oxygen_delivery_adjusted) %>%
  reframe(perc = n()/groups*100) %>% unique 

first_oxygen_delivery %>%
  group_by(trt) %>% 
  summarise(print_continuous(oxygen_delivery_adjusted))

t.test(first_oxygen_delivery %>% filter(trt == "Liberal") %>% pull(oxygen_delivery_adjusted),
       first_oxygen_delivery %>% filter(trt == "Conservative") %>% pull(oxygen_delivery_adjusted),
       alternative = "two.sided")

png(plot_file("ODS_first.png"),width = 700,height = 500)
ggplot(first_bars) +
  geom_rect(aes(xmin = oxygen_delivery_adjusted-5,xmax = oxygen_delivery_adjusted+5,ymin = 0,ymax = perc,fill = trt),alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,350,by = 10),minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0,14,by = 2),minor_breaks = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 270)) +
  labs(fill = "",
       x = "Oxygen delivery (HB*adj HR*SpO2, in 10K)",
       y = "Percentage (%)")
dev.off()



# CS by start value

lm_cs_ods <-  ordinal::clmm2(location = supportdeath ~ scale(pims3_death) + scale(age) + (trt=="Conservative") + scale(oxygen_delivery_adjusted) + (trt=="Conservative")*scale(oxygen_delivery_adjusted) + scale(baseSpO2) ,
                                 random = sites,
                                 data = first_oxygen_delivery,
                                 nAGQ = 9,
                                 Hess = TRUE)


lm_cs_ods_C <-  ordinal::clmm2(location = supportdeath ~ scale(pims3_death) + scale(age) + scale(oxygen_delivery_adjusted) + scale(baseSpO2) ,
                             random = sites,
                             data = first_oxygen_delivery %>% filter(trt == "Conservative"),
                             nAGQ = 9,
                             Hess = TRUE)

lm_cs_ods_L <-  ordinal::clmm2(location = supportdeath ~ scale(pims3_death) + scale(age) + scale(oxygen_delivery_adjusted) + scale(baseSpO2) ,
                             random = sites,
                             data = first_oxygen_delivery %>% filter(trt == "Liberal"),
                             nAGQ = 9,
                             Hess = TRUE)

sC = summary(lm_cs_ods_C)$coefficients[-(1:27),]
sL = summary(lm_cs_ods_L)$coefficients[-(1:30),]

sC %>% 
  as.tibble() %>% 
  rename(se = "Std. Error") %>%
  mutate(Name = rownames(sC)) %>% 
  mutate(lower = Estimate - 1.96*se) %>%
  mutate(upper = Estimate + 1.96*se) %>%
  mutate(upper = exp(upper),
         lower = exp(lower),
         Estimate = exp(Estimate)) %>%
  select(Name,Estimate,lower,upper,`Pr(>|z|)`)

sL  %>% 
  as.tibble() %>% 
  rename(se = "Std. Error") %>%
  mutate(Name = rownames(sC)) %>% 
  mutate(lower = Estimate - 1.96*se) %>%
  mutate(upper = Estimate + 1.96*se) %>%
  mutate(upper = exp(upper),
         lower = exp(lower),
         Estimate = exp(Estimate)) %>%
  select(Name,Estimate,lower,upper,`Pr(>|z|)`)

lm_cs_ods_sum = summary(lm_cs_ods)
coefs = lm_cs_ods_sum$coefficients[-(1:30),] %>% as_tibble()
coefs = coefs %>%
  rename(se = 'Std. Error') %>%
  mutate(lower = Estimate - 1.96*se) %>%
  mutate(upper = Estimate + 1.96*se) %>%
  mutate(upper = exp(upper),
         lower = exp(lower),
         Estimate = exp(Estimate),
         Coef = names(lm_cs_ods$coefficients)[31:36])

saveRDS(coefs,rds_file("lm_cs_primary_ods_coefs"))
coefs = readRDS(rds_file("lm_cs_primary_ods_coefs"))
coefs = coefs %>%
  ungroup %>%
  mutate(Estimate = (Estimate),
         lower = (lower),
         upper = (upper))

# CS plot


base_data_cs <- tibble::tibble(mean  = round(coefs$Estimate,2),
                               lower = round(coefs$lower,2),
                               upper = round(coefs$upper,2),
                               study = c("PIM-3 score (standardised)","Age (standardised)","Conservative treatment",
                                         "Oxygen delivery (scaled)","Baseline SpO2 (standardised)",
                                         "Conservative treatment * Oxygen delivery (scaled)"),
                               deaths_steroid = as.character(round(coefs$Estimate,2)),
                               deaths_placebo = as.character(round(coefs$lower,2)),
                               OR = as.character(round(coefs$upper,2)))

# base_data_cs = base_data_cs[c(4,5,3,6,),]
saveRDS(base_data_cs,rds_file("base_data_cs"))

png(plot_file("CS_scaled_ODS.png"),width = 1500,height = 1000,res = 150)

base_data_cs[c(2,5,1,3,4,6),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(0,1.5,by = 0.5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Odds ratio",
             zero = 1,
             title = "Ordinal regression coefficients for combined mortality and organ support",
             # xlog = T,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_5 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               # v_5 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_8 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper")) 

dev.off()


#### Excluding treatment effects
lm_cs_ods_min <-  ordinal::clmm2(location = supportdeath ~ scale(pims3_death) + scale(age) + scale(oxygen_delivery_adjusted) + scale(baseSpO2) ,
                             random = sites,
                             data = first_oxygen_delivery,
                             nAGQ = 9,
                             Hess = TRUE)

lm_cs_odsmin_sum = summary(lm_cs_ods_min)
coefs = lm_cs_odsmin_sum$coefficients[-(1:30),] %>% as_tibble()
coefs = coefs %>%
  rename(se = 'Std. Error') %>%
  mutate(lower = Estimate - 1.96*se) %>%
  mutate(upper = Estimate + 1.96*se) %>%
  mutate(upper = exp(upper),
         lower = exp(lower),
         Estimate = exp(Estimate),
         Coef = names(lm_cs_odsmin_sum$coefficients)[31:36])

saveRDS(coefs,rds_file("lm_cs_primary_ods_notreat_coefs"))
coefs = readRDS(rds_file("lm_cs_primary_ods_notreat_coefs"))
coefs = coefs %>%
  ungroup %>%
  mutate(Estimate = (Estimate),
         lower = (lower),
         upper = (upper))

# CS plot


base_data_cs_notreat <- tibble::tibble(mean  = round(coefs$Estimate,2),
                               lower = round(coefs$lower,2),
                               upper = round(coefs$upper,2),
                               study = c("PIM-3 score (standardised)","Age (standardised)",
                                         "Oxygen delivery (scaled)","Baseline SpO2 (standardised)"),
                               deaths_steroid = as.character(round(coefs$Estimate,2)),
                               deaths_placebo = as.character(round(coefs$lower,2)),
                               OR = as.character(round(coefs$upper,2)))

# base_data_cs = base_data_cs[c(4,5,3,6,),]
saveRDS(base_data_cs_notreat,rds_file("base_data_cs_notreat"))

png(plot_file("CS_scaled_ODS_notreat.png"),width = 1500,height = 600,res = 150)

base_data_cs_notreat[c(2,4,1,3),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(0,1.5,by = 0.5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Odds ratio",
             zero = 1,
             title = "Ordinal regression coefficients for combined mortality and organ support",
             # xlog = T,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               # v_5 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_6 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper")) 

dev.off()
