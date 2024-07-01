library("lme4")
library("lmerTest")
library("forestploter")
library("forestplot")
library("ordinal")

################## PRIMARY ANALYSIS

chimera_patients = readRDS(rds_file("chimera_patients"))
chimera_obs = readRDS(rds_file("chimera_obs"))

merged_data_set = chimera_obs %>%
  left_join(chimera_patients,by = "studysubjectid")

merged_data_set = merged_data_set  %>%
  mutate(supportdeath = factor(supportdeath,ordered = TRUE))%>%
  mutate(ethnic = relevel(as.factor(ethnic),ref = "White"))


# Filter values
# check missigness of repeated measurements
merged_data_set %>% select(spo2_,baseSpO2,fio2_,baseFiO2,marp_,marp,pao2paed_,syspaed) %>%
  mutate_all(is.na) %>%
  reframe(spo2 = mean(spo2_),
          basespo2 = mean(baseSpO2),
          fio2 = mean(fio2_),
          basefio2 = mean(baseFiO2),
          marp = mean(marp_),
          basemarp = mean(marp),
          pao2 = mean(pao2paed_),
          bp = mean(syspaed))

merged_data_set <- merged_data_set  %>%
  mutate(spo2_ = case_when(spo2_ > 100 | spo2_ <= 0 ~ NA_real_,
                           TRUE ~ spo2_)) %>%
  mutate(baseSpO2 = case_when(baseSpO2 > 100 | baseSpO2 <= 0 ~ NA_real_,
                           TRUE ~ baseSpO2)) %>%
  mutate(marp_ = case_when(marp_ > 30 | marp_ < 6  ~ NA_real_,
                           TRUE ~ marp_)) %>%
  mutate(marp = case_when(marp > 30 | marp < 6 ~ NA_real_,
                           TRUE ~ marp)) %>%
  mutate(syspaed = case_when(syspaed < 5 ~ NA_real_,
                               TRUE ~ syspaed))

# Check how the removal of missing values changes overall missingness
merged_data_set %>%
select(spo2_,baseSpO2,fio2_,baseFiO2,marp_,marp,pao2paed_,syspaed) %>%
  mutate_all(is.na) %>%
  reframe(spo2 = mean(spo2_),
          basespo2 = mean(baseSpO2),
          fio2 = mean(fio2_),
          basefio2 = mean(baseFiO2),
          marp = mean(marp_),
          basemarp = mean(marp),
          pao2 = mean(pao2paed_),
          bp = mean(syspaed))

# Get replacement baseline vals
replacement_spo2 = merged_data_set %>%
  group_by(studysubjectid) %>%
  arrange(daysfromrand) %>%
  filter(!is.na(spo2_)) %>%
  dplyr::slice(1) %>%
  mutate(spo2_replacement = case_when(daysfromrand < 0.25 ~ spo2_,
                                      TRUE ~ NA_real_))%>%
  select(studysubjectid,spo2_replacement)

replacement_fio2 = merged_data_set %>%
  group_by(studysubjectid) %>%
  arrange(daysfromrand) %>%
  filter(!is.na(fio2_)) %>%
  dplyr::slice(1) %>%
  mutate(fio2_replacement = case_when(daysfromrand < 0.25 ~ fio2_,
                                      TRUE ~ NA_real_)) %>%
  select(studysubjectid,fio2_replacement)

replacement_marp = merged_data_set %>%
  group_by(studysubjectid) %>%
  arrange(daysfromrand) %>%
  filter(!is.na(marp_)) %>%
  dplyr::slice(1) %>%
  mutate(marp_replacement = case_when(daysfromrand < 0.25 ~ marp_,
                                      TRUE ~ NA_real_)) %>%
  select(studysubjectid,marp_replacement)

replacement_pao2 = merged_data_set %>%
  group_by(studysubjectid) %>%
  arrange(daysfromrand) %>%
  filter(!is.na(basepao2)) %>%
  dplyr::slice(1) %>%
  mutate(pao2_replacement = case_when(daysfromrand < 0.25 ~ basepao2,
                                      TRUE ~ NA_real_)) %>%
  select(studysubjectid,pao2_replacement)

##
merged_data_set = merged_data_set %>%
  left_join(replacement_spo2,by = "studysubjectid") %>%
  left_join(replacement_fio2,by = "studysubjectid") %>%
  left_join(replacement_marp,by = "studysubjectid") %>%
  left_join(replacement_pao2,by = "studysubjectid") %>%
  mutate(marp = case_when(is.na(marp) ~ marp_replacement,
                          TRUE ~ marp)) %>%
  mutate(baseSpO2 = case_when(is.na(baseSpO2) ~ spo2_replacement,
                          TRUE ~ baseSpO2)) %>%
  mutate(baseFiO2 = case_when(is.na(baseFiO2) ~ fio2_replacement,
                          TRUE ~ baseFiO2)) %>%
  mutate(basepao2 = case_when(is.na(basepao2) ~ pao2_replacement,
                              TRUE ~ basepao2))

merged_data_set %>% group_by(studysubjectid) %>% dplyr::slice(1) %>% ungroup %>% reframe(spo2 = mean(is.na(baseSpO2)),
                                                                             fio2 = mean(is.na(baseFiO2)),
                                                                             marp = mean(is.na(marp)),
                                                                             pao2 = mean(is.na(basepao2)))

merged_data_set = merged_data_set %>%
  ungroup %>%
  rowwise() %>%
  mutate(hr_age_adjusted = case_when(!is.na(hrpaed_) ~ hr_age_adjustment(hr = hrpaed_,age = age),
                                     TRUE ~ NA_character_))


merged_data_set = merged_data_set %>% 
  mutate(hr_age_adjusted = factor(hr_age_adjusted,levels = c("<1","1-5","5-10","10-50","50-90","90-95","95-99",">99"),ordered = T))

merged_data_set$hr_age_adjusted

saveRDS(merged_data_set,rds_file("merged_data_set"))

# Export values
ggplot(merged_data_set,aes(x = spo2_)) +
  geom_density(aes(x= spo2_),bw = 0.5) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth = 1,alpha = 0.5) +
  theme_bw() +
  labs(x = paste0("SpO2; ",print_continuous(merged_data_set$spo2_)),
       y = "Frequency")
ggsave(plot_file("SpO2.jpg"),width = cm(3),height = cm(2))

ggplot(merged_data_set,aes(x = marp_)) +
  geom_density(aes(x= marp_),bw = 0.75) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth = 1,alpha = 0.5) +
  theme_bw() +
  labs(x = paste0("MArP; ",print_continuous(merged_data_set$marp_)),
       y = "Frequency")
ggsave(plot_file("MArP.jpg"),width = cm(3),height = cm(2))

ggplot(merged_data_set,aes(x = pao2paed_)) +
  geom_density(aes(x= pao2paed_),bw = 0.5) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth = 1,alpha = 0.5) +
  theme_bw() +
  labs(x = paste0("PaO2; ",print_continuous(merged_data_set$pao2paed_)),
       y = "Frequency")
ggsave(plot_file("PaO2.jpg"),width = cm(3),height = cm(2))

ggplot(merged_data_set,aes(x = fio2_)) +
  geom_density(aes(x= fio2_),bw = 0.7) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth = 0.05,alpha = 0.5) +
  theme_bw() +
  labs(x = paste0("FiO2; ",print_continuous(merged_data_set$fio2_)),
       y = "Frequency")
ggsave(plot_file("FIO2.jpg"),width = cm(3),height = cm(2))

ggplot(merged_data_set,aes(x = syspaed)) +
  geom_density(aes(x= syspaed),bw = 2) +
  geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth =1,alpha = 0.5) +
  theme_bw() +
  labs(x = paste0("Syst. BP; ",print_continuous(merged_data_set$syspaed)),
       y = "Frequency")
ggsave(plot_file("Syst.jpg"),width = cm(3),height = cm(2))

# check merger - should be no NAs
merged_data_set %>% filter(is.na(day))
merged_data_set %>%
  arrange(studysubjectid,daysfromrand)

# Only the chosen ethnicities
analysis_data = merged_data_set %>%
  filter(ethnic %in% c("White","Asian","Black")) %>%
  mutate(fio2_ = fio2_*100,
         baseFiO2 = baseFiO2*100)

# rename
analysis_data = analysis_data %>% rename(basemarp = marp) 

analysis_data = analysis_data %>%
  ungroup %>%
  mutate(spo2_scaled = scale(spo2_),
         fio2_scaled = scale(fio2_),
         marp_scaled = scale(marp_))


analysis_data %>%
  group_by(trt) %>%
  tally

# Baseline data for model 3, CS
analysis_data_baseline = chimera_patients %>%
  filter(ethnic %in% c("White","Asian","Black"))

analysis_data_baseline = analysis_data_baseline %>%
  mutate(supportdeath = factor(supportdeath,ordered = TRUE))%>%
  mutate(ethnic = relevel(as.factor(ethnic),ref = "White"))

analysis_data_baseline = analysis_data_baseline %>%
  left_join(replacement_spo2,by = "studysubjectid") %>%
  left_join(replacement_fio2,by = "studysubjectid") %>%
  left_join(replacement_marp,by = "studysubjectid") %>%
  left_join(replacement_pao2,by = "studysubjectid") %>%
  mutate(marp = case_when(is.na(marp) ~ marp_replacement,
                          TRUE ~ marp)) %>%
  mutate(baseSpO2 = case_when(is.na(baseSpO2) ~ spo2_replacement,
                              TRUE ~ baseSpO2)) %>%
  mutate(baseFiO2 = case_when(is.na(baseFiO2) ~ fio2_replacement,
                              TRUE ~ baseFiO2)) %>%
  mutate(basepao2 = case_when(is.na(basepao2) ~ pao2_replacement,
                              TRUE ~ basepao2))

analysis_data_baseline = analysis_data_baseline %>%
  mutate(baseFiO2 = baseFiO2*100)

analysis_data_baseline = analysis_data_baseline %>%
  mutate(baseSpO2_scaled = scale(baseSpO2),
         baseFiO2_scaled = scale(baseFiO2),
         baseMarp_scaled = scale(marp),
         pims3_death_scaled = scale(pims3_death))


saveRDS(analysis_data_baseline,rds_file("analysis_data_baseline1202"))
analysis_data_baseline = readRDS(rds_file("analysis_data_baseline1202"))


analysis_data <- analysis_data %>%
  left_join(analysis_data_baseline %>% 
              select(studysubjectid,baseSpO2_scaled,baseFiO2_scaled,baseMarp_scaled),
            by = "studysubjectid")


saveRDS(analysis_data,rds_file("analysis_data1202"))
saveRDS(analysis_data_baseline,rds_file("analysis_data_baseline1202"))

## ---------------------------------------  SpO2
# formula: SPO2 ~ Ethnicity + treatment + tretment*Ethnicity + Spo2_baseline + site(RE) + studyid(RE)
# Missingness 32%
analysis_data %>%
  summarise(na = mean(is.na(ethnic) | is.na(baseSpO2) | is.na(spo2_)))

analysis_data %>% filter(!is.na(spo2_) & !is.na(baseSpO2)) %>% group_by(studysubjectid) %>% n_groups()

lm_spo2_primary <- lmer(spo2_ ~ ethnic + trt + trt*(ethnic) + baseSpO2_scaled + (1|sites) + (1|sites:studysubjectid),
                        data = analysis_data,
                        REML = T)


spo2_sum = summary(lm_spo2_primary)
spo2_coef = spo2_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(spo2_sum$coefficients))

lm_spo2_primary_ci = confint(lm_spo2_primary)

saveRDS(lm_spo2_primary,rds_file("lm_spo2_primary"))
saveRDS(lm_spo2_primary_ci,rds_file("lm_spo2_primary_ci"))

lm_spo2_primary = readRDS(rds_file("lm_spo2_primary"))
lm_spo2_primary_ci = readRDS(rds_file("lm_spo2_primary_ci"))


base_data_spo2 <- tibble::tibble(mean  = (spo2_coef$Estimate)[-1],
                            lower = spo2_coef$lower[-1],
                            upper = spo2_coef$upper[-1],
                            study = c("Asian","Black","Conservative treatment","Baseline SpO2 (standardised)",
                                      "Conservative treatment * Asian","Conservative treatment * Black"),
                            deaths_steroid = as.character(signif((spo2_coef$Estimate)[-1],2)),
                            deaths_placebo = as.character(signif(spo2_coef$lower[-1],2)),
                            OR = as.character(signif(spo2_coef$upper[-1],2)))

png(plot_file("FP_SPO2_scaled.png"),width = 1500,height = 800,res = 200)

base_data_spo2 |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-3,1,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             vertices = TRUE,
             title = "A) Hierarchical model coefficients for SpO2 outcome") |>
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

# --------------------------------------- FiO2
# formula: FiO2 ~ Ethnicity + SpO2 + SpO2*Ethnicity + MAP + SpO2_baseline +  FiO2_baseline + MAP_baseline + treatment + site(RE) + studyid(RE)
analysis_data %>% filter(!is.na(spo2_) & !is.na(baseSpO2) & !is.na(baseFiO2) & !is.na(fio2_) & !is.na(basemarp) & !is.na(marp_)) %>% 
  group_by(studysubjectid) %>% n_groups()

lm_fio2_primary <- lmer(fio2_ ~ ethnic + spo2_scaled + spo2_scaled*ethnic + marp_scaled + baseSpO2_scaled + baseFiO2_scaled + baseMarp_scaled + trt + (1|sites) + (1|sites:studysubjectid),
                        data = analysis_data,
                        REML = T)


fio2_sum = summary(lm_fio2_primary)
fio2_sum_old = summary(lm_fio2_primary_old)

fio2_coefs = fio2_sum$coefficients %>% as.tibble() %>% rename(se = 'Std. Error') %>% mutate(lower = Estimate - 1.96*se) %>% mutate(upper = Estimate + 1.96*se) %>% mutate(Cov = rownames(fio2_sum$coefficients))
fio2_sum_old$coefficients %>% as.tibble() %>% rename(se = 'Std. Error') %>% mutate(lower = Estimate - 1.96*se) %>% mutate(upper = Estimate + 1.96*se)%>% mutate(Cov = rownames(fio2_sum_old$coefficients))

summary(lm_fio2_primary)
ranova(lm_fio2_primary)
lm_fio2_primary_ci = confint(lm_fio2_primary)

saveRDS(lm_fio2_primary_ci,rds_file("lm_fio2_primary_ci"))
saveRDS(lm_fio2_primary,rds_file("lm_fio2_primary"))

lm_fio2_primary = readRDS(rds_file("lm_fio2_primary"))
lm_fio2_primary_ci = readRDS(rds_file("lm_fio2_primary_ci"))

# FIO2 plot
base_data_fio2 <- tibble::tibble(mean  = fio2_coefs$Estimate[-1],
                                 lower = fio2_coefs$lower[-1],
                                 upper = fio2_coefs$upper[-1],
                                 study = c("Asian","Black","SpO2 (standardised)","Pmean (standardised)","Baseline SpO2 (standardised)","Baseline FiO2 (standardised)","Baseline Pmean (standardised)","Conservative treatment",
                                           "SpO2 (standardised) * Asian","SpO2 (standardised) * Black"),
                                 deaths_steroid = as.character(signif(fio2_coefs$Estimate[-1],2)),
                                 deaths_placebo = as.character(signif(fio2_coefs$lower[-1],2)),
                                 OR = as.character(signif(fio2_coefs$upper[-1],2)))

png(plot_file("FP_FIO2_scaled.png"),width = 1500,height = 1200,res = 200)

base_data_fio2[c(1,2,8,5,6,7,3,4,9,10),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-10,5,by = 5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             title = "B) Hierarchical model coefficients for FiO2 outcome",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_8 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_10 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_12 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()











# ---------------------------------------  Clinical outcome score
# formula: CS ~ pim3 + age + treatment + ethnicity + treatment*ethnicity + SpO2_baseline + site(RE) + studyid(RE)


analysis_data_baseline %>% filter(!is.na(pims3_death) & !is.na(age) & !is.na(baseSpO2) & !is.na(supportdeath))

lm_cs_primary <-  ordinal::clmm2(location = supportdeath ~ pims3_death_scaled + age_scaled + trt + ethnic + trt*ethnic + baseSpO2_scaled,
                       random = sites,
                       # data = analysis_data_baseline %>%
                       #   ungroup %>%
                       #   mutate(baseSpO2 = (baseSpO2 - mean(baseSpO2))/sd(baseSpO2,na.rm = T)) ,
                       data = analysis_data_baseline %>% mutate(age_scaled = scale(age)),
                       nAGQ = 9,
                       Hess = TRUE)

# cs_clmm = clmm(formula = supportdeath ~ pims3_death + age + trt + ethnic + trt*ethnic + baseSpO2 + (1|sites),
#      data = analysis_data_baseline)

# lm_cs_primary_simple <- lmer(as.numeric(supportdeath) ~ pims3_death + age + trt + ethnic + trt*ethnic + baseSpO2 + (1|sites),
#                         data = analysis_data_baseline)
# 
# lm_cs_primary_simple_scaled <- lmer(as.numeric(supportdeath) ~ pims3_death + age + trt + ethnic + trt*ethnic + baseSpO2 + (1|sites),
#                              data = analysis_data_baseline %>%
#                                mutate(baseSpO2 = (baseSpO2-mean(baseSpO2))/sd(baseSpO2)))



# I don't feel like coding this - SE for baseSpO2 comes out NaN, due to scaling issues.
# Unscaled Est; -0.0416
# Scaled Est; -0.1850 SE 0.0507
# --> Unscaled SE 0.01140065

lm_cs_sum = summary(lm_cs_primary)
coefs = lm_cs_sum$coefficients[-(1:30),] %>% as_tibble()
coefs = coefs %>%
  rename(se = 'Std. Error') %>%
  mutate(se = ifelse(is.na(se),0.01140065,se)) %>%
  mutate(lower = Estimate - 1.96*se) %>%
  mutate(upper = Estimate + 1.96*se) %>%
  mutate(upper = exp(upper),
         lower = exp(lower),
         Estimate = exp(Estimate))

saveRDS(coefs,rds_file("lm_cs_primary_coefs"))
coefs = readRDS(rds_file("lm_cs_primary_coefs"))
coefs = coefs %>%
  ungroup %>%
  mutate(Estimate = log(Estimate),
         lower = log(lower),
         upper = log(upper))

# CS plot

  
base_data_cs <- tibble::tibble(mean  = round(coefs$Estimate,3),
                                 lower = round(coefs$lower,3),
                                 upper = round(coefs$upper,3),
                                 study = c("PIM-3 score (standardised)","Age (standardised)","Conservative treatment","Asian","Black","Baseline SpO2 (standardised)",
                                           "Conservative treatment * Asian","Conservative treatment * Black"),
                                 deaths_steroid = as.character(round(coefs$Estimate %>% exp,3)),
                                 deaths_placebo = as.character(round(coefs$lower %>% exp,3)),
                                 OR = as.character(round(coefs$upper %>% exp,3)))

# base_data_cs = base_data_cs[c(4,5,3,6,),]
saveRDS(base_data_cs,rds_file("base_data_cs"))

png(plot_file("CS_scaled.png"),width = 1500,height = 1000,res = 200)

base_data_cs[c(4,5,3,6,1,2,7,8),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-1,1,by = 0.5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Odds ratio (log)",
             zero = 0,
             title = "C) Ordinal regression coefficients for combined mortality and organ support",
             # xlog = T,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_8 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               # v_5 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_10 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper")) 

dev.off()




 #### Combined plot

# spo2_plot <-   
jpeg(filename = plot_file("SpO2_aligned.jpg"),width = 500,height = 1000)

  (base_data_spo2 %>% 
      mutate(study = NA) %>%
    add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>%
    add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>% 
    add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA) %>%
    add_row(mean = NA,lower = NA,upper = NA,study=NA,deaths_steroid=NA,deaths_placebo=NA,OR=NA))[c(1,2,4,7,8,9,10,3,5,6),]  |>
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
  fp_add_lines(h_4 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_9 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_12 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()

# fio2_plot <-  
jpeg(filename = plot_file("FiO2_aligned.jpg"),width = 700,height = 1000)
  base_data_fio2[c(1,2,5,6,7,3,4,8,9,10),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-20,10,by = 5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Regression Coefficient",
             title = "A) Fio2",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_4 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_9 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_12 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

  dev.off()
  

jpeg(filename = plot_file("FiO2_aligned.jpg"),width = 1000,height = 1000)

  base_data_cs[c(4,5,1,2,6,3,7,8),] |>
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
    fp_add_lines(h_4 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
                 h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
                 h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
                 # v_5 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
                 h_10 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
    fp_add_header(study = c("Covariate"),
                  deaths_steroid = c("Estimate"),
                  deaths_placebo = c("Lower"),
                  OR = c("Upper"))

dev.off()
