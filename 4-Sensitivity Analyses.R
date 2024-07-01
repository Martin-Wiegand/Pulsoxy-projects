library("lme4")

analysis_data = readRDS(rds_file("analysis_data1202"))
merged_data_set = readRDS(rds_file("merged_data_set")) %>%
  mutate(fio2_ = fio2_*100,
         baseFiO2 = baseFiO2*100)

# First 7 days
# Make data set
analysis_data %>% ungroup %>%
  reframe(mean(daysfromrand <= 7,na.rm = T)) 

ggplot(data = analysis_data %>% ungroup %>%
  mutate(day = ceiling(daysfromrand)) %>%
  group_by(studysubjectid,day) %>%
  reframe(n_per_day_per_p = n()) %>%
  group_by(day) %>%
  reframe(n_per_day_avg =  mean(n_per_day_per_p,na.rm = T)) %>%
    filter(day > 0)) +
  geom_line(aes(x = day,y = n_per_day_avg),colour = "cornflowerblue") +
  geom_point(aes(x = day,y = n_per_day_avg)) +
  coord_cartesian(xlim = c(1,30),
                  ylim = c(0,25)) +
  theme_bw() +
  labs(x = "Days since randomisation",
       y = "Average measurements per patient per day") +
  scale_x_continuous(breaks = c(1:7,14,21,28),
                     minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL)

ggsave(plot_file("MeasurementsPerDay.jpg"),width = cm(3),height = cm(2))


# Inlcuding other categories
merged_data_set %>% group_by(ethnic) %>% tally

all_ethnicities_data = merged_data_set %>%
  mutate(ethnic = ifelse(is.na(ethnic),"Not known",as.character(ethnic))) %>%
  mutate(ethnic = relevel(factor(ethnic,levels = c("White","Asian","Black","Mixed","Not known","Other")),ref = "White"))  %>%
  ungroup %>%
  mutate(spo2_scaled = scale(spo2_),
         fio2_scaled = scale(fio2_),
         marp_scaled = scale(marp_))

# Baseline version
all_ethnicities_baseline = all_ethnicities_data %>%
  group_by(studysubjectid) %>%
  dplyr::slice(1) %>%
  ungroup %>%
  mutate(baseSpO2_scaled = scale(baseSpO2),
         baseFiO2_scaled = scale(baseFiO2),
         baseMarp_scaled = scale(marp),
         pims3_death_scaled = scale(pims3_death))

all_ethnicities_data = all_ethnicities_data %>%
  left_join(all_ethnicities_baseline %>% 
              select(studysubjectid,baseSpO2_scaled,baseFiO2_scaled,baseMarp_scaled),
            by = "studysubjectid")

# 7 days only
first_7_days = analysis_data %>%
  filter(daysfromrand <= 7)


saveRDS(first_7_days,rds_file("first_7_days"))
saveRDS(all_ethnicities_data,rds_file("all_ethnicities_data"))
saveRDS(all_ethnicities_baseline,rds_file("all_ethnicities_baseline"))

first_7_days = readRDS(rds_file("first_7_days"))
all_ethnicities_data = readRDS(rds_file("all_ethnicities_data"))
all_ethnicities_baseline = readRDS(rds_file("all_ethnicities_baseline"))

########### Models
# First 7 days only
# SPO2 analysis
lm_spo2_sensitivity <- lmer(spo2_ ~ ethnic + trt + trt*ethnic + baseSpO2_scaled + (1|sites) + (1|sites:studysubjectid),
                        data = first_7_days,
                        REML = T)

lm_spo2_7d_sum = summary(lm_spo2_sensitivity)
lm_spo2_7d_coef = lm_spo2_7d_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(spo2_sum$coefficients))

saveRDS(lm_spo2_7d_coef,rds_file("lm_spo2_7d_coef"))
saveRDS(lm_spo2_sensitivity,rds_file("lm_spo2_sensitivity"))


lm_spo2_7d_coef = readRDS(rds_file("lm_spo2_7d_coef"))
lm_spo2_sensitivity = readRDS(rds_file("lm_spo2_sensitivity"))

### Plot
base_data_spo2_7days <- tibble::tibble(mean  = lm_spo2_7d_coef$Estimate[-1],
                                       lower = lm_spo2_7d_coef$lower[-1],
                                       upper = lm_spo2_7d_coef$upper[-1],
                                       study = c("Asian","Black","Conservative treatment","Baseline SpO2 (%)",
                                                 "Conservative treatment * Asian","Conservative treatment * Black"),
                                       deaths_steroid = as.character(signif(lm_spo2_7d_coef$Estimate[-1],2)),
                                       deaths_placebo = as.character(signif(lm_spo2_7d_coef$lower[-1],3)),
                                       OR = as.character(signif(lm_spo2_7d_coef$upper[-1],3)))

jpeg(plot_file("FP_SPO2_7days.jpg"),width = 1500,height = 800,res = 200)

base_data_spo2_7days |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-3,1,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",title = "A) Regression coefficients for SpO2 outcome within 7 days of admission",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_6 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_8 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()


# FiO2
# formula: FiO2 ~ Ethnicity + SpO2 + SpO2*Ethnicity + MAP + SpO2_baseline +  FiO2_baseline + MAP_baseline + treatment + site(RE) + studyid(RE)
lm_fio2_sensitivity <- lmer(fio2_ ~ ethnic + spo2_scaled + spo2_scaled*ethnic + marp_scaled + baseSpO2_scaled + baseFiO2_scaled + baseMarp_scaled + trt + (1|sites) + (1|sites:studysubjectid),
                        data = first_7_days,
                        REML = T)

lm_fio2_7d_sum = summary(lm_fio2_sensitivity)
lm_fio2_7d_coef = lm_fio2_7d_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(lm_fio2_7d_sum$coefficients))

saveRDS(lm_fio2_7d_coef,rds_file("lm_fio2_7d_coef"))
saveRDS(lm_fio2_7d_sum,rds_file("lm_fio2_7d_sum"))

lm_fio2_7d_coef = readRDS(rds_file("lm_fio2_7d_coef"))
lm_fio2_7d_sum = readRDS(rds_file("lm_fio2_7d_sum"))

### Plot
base_data_fio2_7days <- tibble::tibble(mean  = lm_fio2_7d_coef$Estimate[-1],
                                       lower = lm_fio2_7d_coef$lower[-1],
                                       upper = lm_fio2_7d_coef$upper[-1],
                                       study = c("Asian","Black","SpO2 (standardised)","Pmean (standardised)","Baseline SpO2 (standardised)","Baseline FiO2 (standardised)","Baseline Pmean (standardised)","Conservative treatment",
                                                 "SpO2 (standardised) * Asian","SpO2 (standardised) * Black"),
                                       deaths_steroid = as.character(signif(lm_fio2_7d_coef$Estimate[-1],2)),
                                       deaths_placebo = as.character(signif(lm_fio2_7d_coef$lower[-1],2)),
                                       OR = as.character(signif(lm_fio2_7d_coef$upper[-1],2)))

jpeg(plot_file("FP_FIO2_7days.jpg"),width = 1500,height = 1200,res = 200)

base_data_fio2_7days[c(1,2,8,5,6,7,3,4,9,10),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-20,10,by = 5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             title = "B) Regression coefficients for FiO2 outcome within 7 days of admission",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_10 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_8 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_12 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()



# #### Combined plot
# 
# # spo2_plot <-   
# jpeg(filename = plot_file("SpO2_aligned_7D.jpg"),width = 500,height = 1000)
# 
# (base_data_spo2_7days %>% 
#     mutate(study = NA) %>%
#     add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>%
#     add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>% 
#     add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>%
#     add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA))[c(1,2,4,7,8,9,10,3,5,6),]  |>
#   forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
#              boxsize = 0.1,
#              xticks = seq(-3,1,by = 1),
#              txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
#              xlab = "Regression Coefficient",
#              title = "B) SpO2",
#              vertices = TRUE,
#              graphwidth = unit(8,"cm"),
#              colgap = unit(0.5,"cm")) |>
#   fp_set_style(box = "royalblue",
#                line = "darkblue") |> 
#   fp_add_lines(h_4 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
#                h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
#                h_9 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
#                h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
#                h_12 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
#   fp_add_header(deaths_steroid = c("Estimate"),
#                 deaths_placebo = c("Lower"),
#                 OR = c("Upper"))
# 
# dev.off()
# 
# # fio2_plot <-  
# jpeg(filename = plot_file("FiO2_aligned_7D.jpg"),width = 700,height = 1000)
# base_data_fio2_7days[c(1,2,5,6,7,3,4,8,9,10),] |>
#   forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
#              boxsize = 0.1,
#              xticks = seq(-20,10,by = 5),
#              txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
#              xlab = "Regression Coefficient",
#              title = "A) Fio2",
#              vertices = TRUE) |>
#   fp_set_style(box = "royalblue",
#                line = "darkblue") |>
#   fp_add_lines(h_4 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
#                h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
#                h_9 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
#                h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
#                h_12 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
#   fp_add_header(study = c("Covariate"),
#                 deaths_steroid = c("Estimate"),
#                 deaths_placebo = c("Lower"),
#                 OR = c("Upper"))
# 
# dev.off()

























######################## Including all categories
# SpO2
# formula: SPO2 ~ Ethnicity + treatment + tretment*Ethnicity + Spo2_baseline + site(RE) + studyid(RE)
lm_spo2_ethnic_sens <- lmer(spo2_ ~ ethnic + trt + trt*ethnic + baseSpO2_scaled + (1|sites) + (1|sites:studysubjectid),
                            data = all_ethnicities_data,
                            REML = T)

spo2_eth_sum = summary(lm_spo2_ethnic_sens)
spo2_eth_coef = spo2_eth_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(spo2_eth_sum$coefficients))

saveRDS(lm_spo2_ethnic_sens,rds_file("lm_spo2_ethnic_sens"))
saveRDS(spo2_eth_coef,rds_file("spo2_eth_coef"))

lm_spo2_ethnic_sens = readRDS(rds_file("lm_spo2_ethnic_sens"))
spo2_eth_coef = readRDS(rds_file("spo2_eth_coef"))



# Plot
base_data_spo2_eth <- tibble::tibble(mean  = spo2_eth_coef$Estimate[-1],
                                 lower = spo2_eth_coef$lower[-1],
                                 upper = spo2_eth_coef$upper[-1],
                                 study = c("Asian","Black","Mixed","Not known","Other","Conservative treatment","Baseline SpO2 (standardised)","Conservative treatment * Asian",
                                           "Conservative treatment * Black","Conservative treatment * Mixed","Conservative treatment * Not known","Conservative treatment * Other"),
                                 deaths_steroid = as.character(signif(spo2_eth_coef$Estimate[-1],2)),
                                 deaths_placebo = as.character(signif(spo2_eth_coef$lower[-1],2)),
                                 OR = as.character(signif(spo2_eth_coef$upper[-1],2)))

saveRDS(base_data_spo2_eth,rds_file("base_data_spo2_eth"))

jpeg(plot_file("FP_SPO2_eth.jpg"),width = 1500,height = 800,res = 200)

base_data_spo2_eth |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-3,2,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             title = "A) Regression coefficients for SpO2 outcome including all ethnicity groups",
             xlab = "Effect size",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_9 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_14 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()




# FiO2
# formula: FiO2 ~ Ethnicity + SpO2 + SpO2*Ethnicity + MAP + SpO2_baseline +  FiO2_baseline + MAP_baseline + treatment + site(RE) + studyid(RE)
lm_fio2_ethnic_sens <- lmer(fio2_ ~ ethnic + spo2_scaled + spo2_scaled*ethnic + marp_scaled + baseSpO2_scaled + baseFiO2_scaled + baseMarp_scaled + trt + (1|sites) + (1|sites:studysubjectid),
                        data = all_ethnicities_data,
                        REML = T)

fio2_eth_sum = summary(lm_fio2_ethnic_sens)
fio2_eth_coef = fio2_eth_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(fio2_eth_sum$coefficients))

saveRDS(lm_fio2_ethnic_sens,rds_file("lm_fio2_ethnic_sens"))
saveRDS(fio2_eth_coef,rds_file("fio2_eth_coef"))

# Plot
base_data_fio2_eth <- tibble::tibble(mean  = fio2_eth_coef$Estimate[-1],
                                     lower = fio2_eth_coef$lower[-1],
                                     upper = fio2_eth_coef$upper[-1],
                                     study = c("Asian","Black","Mixed","Not known","Other","SpO2 (standardised)","Pmean (standardised)","Baseline SpO2 (standardised)","Baseline FiO2 (standardised)","Baseline Pmean (standardised)","Conservative treatment",
                                               "SpO2 (standardised) * Asian","SpO2 (standardised) * Black","SpO2 (standardised) * Mixed","SpO2 (standardised)* Not known","SpO2 (standardised) * Other"),
                                     deaths_steroid = as.character(signif(fio2_eth_coef$Estimate[-1],2)),
                                     deaths_placebo = as.character(signif(fio2_eth_coef$lower[-1],2)),
                                     OR = as.character(signif(fio2_eth_coef$upper[-1],2)))

saveRDS(base_data_fio2_eth,rds_file("base_data_fio2_eth"))

jpeg(plot_file("FP_FIO2_eth.jpg"),width = 1500,height = 1200,res = 200)

base_data_fio2_eth[c(1:5,11,8:10,6,7,12:16),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-8,6,by = 2),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             title = "B) Regression coefficients for FiO2 outcome including all ethnicity groups",
             xlab = "Effect size",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_13 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_11 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_18 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()



# Clinical outcome score
# formula: CS ~ pim3 + age + treatment + ethnicity + treatment*ethnicity + SpO2_baseline + site(RE) + studyid(RE)

all_ethnicities_baseline = all_ethnicities_baseline %>%
  filter(!is.na(supportdeath)) %>%
  mutate(supportdeath = factor(supportdeath,ordered = TRUE,levels = 1:31)) %>%
  mutate(ethnic = relevel(as.factor(ethnic),ref = "White"))


saveRDS(all_ethnicities_baseline,rds_file("all_ethnicities_baseline"))
# all_ethnicities_baseline$ethnic = relevel(as.factor(all_ethnicities_baseline$ethnic),ref = "White")  
# all_ethnicities_baseline$supportdeath = factor(all_ethnicities_baseline$supportdeath,ordered = T)

# original_data = chimera_patients
# original_data$ethnic = relevel(as.factor(original_data$ethnic),ref = "White")  
# original_data$supportdeath = factor(original_data$supportdeath,ordered = T)

# lm_cs_ethnic_sens <- lmer(supportdeath ~ pims3_death + age + trt + ethnic + trt*ethnic + baseSpO2 + (1|sites),
#                           data = all_ethnicities_baseline,
#                           REML = T)

lm_cs_ethnic_sens <-  ordinal::clmm2(location = supportdeath ~ pims3_death_scaled + age_scaled + trt + ethnic + trt*ethnic + baseSpO2_scaled,
                                 random = sites,
                                 data = all_ethnicities_baseline %>%
                                    ungroup %>% mutate(age_scaled = scale(age)),
                                 Hess = TRUE)


# brms_eth =brm(supportdeath ~ pims3_death + age + trt + ethnic + trt*ethnic + baseSpO2 + (1|sites),
#               data = all_ethnicities_baseline,
#               family = cumulative(),
#               prior = set_prior("normal(0,5"))
  
brms_eth_sum = summary(lm_cs_ethnic_sens)
CS_coefs = brms_eth_sum$coefficients[-(1:30),] %>%
  as_tibble() %>%
  rename(sterr = 'Std. Error') %>%
  mutate(lower = Estimate - 1.96*sterr) %>%
  mutate(upper = Estimate + 1.96*sterr)

summary(lm_cs_ethnic_sens)

saveRDS(CS_coefs,rds_file("CS_coefs"))
saveRDS(lm_cs_ethnic_sens,rds_file("lm_cs_ethnic_sens"))

# Plot
base_data_cs_eth <- tibble::tibble(mean  = CS_coefs$Estimate %>% exp,
                                     lower = CS_coefs$lower %>% exp,
                                     upper = CS_coefs$upper %>% exp,
                                     study = c("PIM-3 risk (standardised)","Age (standardised)","Conservative treatment","Asian","Black","Mixed","Not known","Other","Baseline SpO2 (standardised)",
                                               "Conservative treatment * Asian","Conservative treatment * Black","Conservative treatment * Mixed","Conservative treatment * Not known","Conservative treatment * Other"),
                                     deaths_steroid = as.character(signif(CS_coefs$Estimate %>% exp,3)),
                                     deaths_placebo = as.character(signif(CS_coefs$lower %>% exp,3)),
                                     OR = as.character(signif(CS_coefs$upper %>% exp %>% exp,3)))

saveRDS(base_data_cs_eth,rds_file("base_data_cs_eth"))

jpeg(plot_file("FP_CS_eth.jpg"),width = 1500,height = 1000,res = 200)

base_data_cs_eth[c(4:8,3,1,2,9,10:14),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(0,4,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Odds ratio",
             title = "C) Regression coefficients for clinical outcome including all ethnicity groups",
             zero = 1,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_11 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_16 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()









#### Combined plot

# spo2_plot <-   
jpeg(filename = plot_file("SpO2_aligned_ETH.jpg"),width = 500,height = 1000)

(base_data_spo2_eth %>% 
    mutate(study = NA) %>%
    add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>%
    add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>% 
    add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>%
    add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA))[c(1,2,3,4,5,7,13,14,15,16,6,8:12),]  |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-3,1,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Regression Coefficient",
             title = "B) SpO2",
             vertices = TRUE,
             graphwidth = unit(8,"cm"),
             colgap = unit(0.5,"cm")) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_10 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_12 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_18 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()

# fio2_plot <-  
jpeg(filename = plot_file("FiO2_aligned_ETH.jpg"),width = 700,height = 1000)

base_data_fio2_eth[c(1,2,3,4,5,8,9,10,6,7,11:16),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-20,10,by = 5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Regression Coefficient",
             title = "A) Fio2",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_10 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_12 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_18 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()


jpeg(filename = plot_file("CS_aligned_MI.jpg"),width = 1000,height = 1000)

base_data_cs_eth[c(4,5,6,7,8,1,2,9,3,10:14),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(0,3,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Hazard Ratio",
             zero = 1,
             title = "C) Rank-Based Composite of Days of Organ Failure or Death",
             graphwidth = unit(14,"cm"),
             colgap = unit(1.3,"cm"),
             # xlog = T,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_10 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               # v_5 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_16 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()

