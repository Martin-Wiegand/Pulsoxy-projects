library("lme4")
library("ordinal")
library("forestplot")


analysis_data_sev = readRDS(rds_file("analysis_data_sev"))
analysis_data_sev_baseline = readRDS(rds_file("analysis_data_sev_baseline"))

analysis_data_sev_97 = analysis_data_sev %>%
  filter(baseSpO2 <= 97 & spo2_ <= 97)

analysis_data_sev_baseline_97 = analysis_data_sev_baseline %>%
  filter(baseSpO2 <= 97)


saveRDS(analysis_data_sev_97,rds_file("analysis_data_sev_97"))
saveRDS(analysis_data_sev_baseline_97,rds_file("analysis_data_sev_baseline_97"))

## ---------------------------------------  SpO2
# formula: SPO2 ~ Ethnicity + treatment + tretment*Ethnicity + Spo2_baseline + site(RE) + studyid(RE)
# Missingness 32%
analysis_data_sev_97 %>%
  summarise(na = mean(is.na(ethnic) | is.na(baseSpO2) | is.na(spo2_)))

analysis_data_sev_97 %>% filter(!is.na(spo2_) & !is.na(baseSpO2) & !is.na(OSI)) %>% group_by(studysubjectid) %>% n_groups()

lm_spo2_primary_sev_97 <- lmer(spo2_ ~ OSI_exceeded + trt + trt*(OSI_exceeded) + scale(baseSpO2) + (1|sites) + (1|sites:studysubjectid),
                            data = analysis_data_sev_97,
                            REML = T)


spo2_sev_97_sum = summary(lm_spo2_primary_sev_97)

spo2_97_coef = spo2_sev_97_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(spo2_sev_97_sum$coefficients))

saveRDS(spo2_97_coef,rds_file("spo2_97_coef"))
spo2_97_coef = readRDS(rds_file("spo2_97_coef"))


base_data_sev_spo2_97 <- tibble::tibble(mean  = spo2_97_coef$Estimate[-1],
                                     lower = spo2_97_coef$lower[-1],
                                     upper = spo2_97_coef$upper[-1],
                                     study = c("OSI >= 12","Conservative treatment","Baseline SpO2 (standardised)",
                                               "OSI >= 12 * Conservative Treatment"),
                                     deaths_steroid = as.character(signif(spo2_97_coef$Estimate[-1],2)),
                                     deaths_placebo = as.character(signif(spo2_97_coef$lower[-1],3)),
                                     OR = as.character(signif(spo2_97_coef$upper[-1],3)))

jpeg(plot_file("FP_SPO2_sev_97.jpg"),width = 900,height = 300)

base_data_sev_spo2_97[c(3,1,2,4),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-3,1,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_3 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_6 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()

# --------------------------------------- FiO2
# formula: FiO2 ~ Ethnicity + SpO2 + SpO2*Ethnicity + MAP + SpO2_baseline +  FiO2_baseline + MAP_baseline + treatment + site(RE) + studyid(RE)
analysis_data_sev %>% filter(!is.na(spo2_) & !is.na(baseSpO2) & !is.na(baseFiO2) & !is.na(fio2_) & !is.na(basemarp) & !is.na(marp_) &!is.na(OSI_exceeded)) %>% 
  group_by(studysubjectid) %>% n_groups()

lm_fio2_primary_sev_97 <- lmer(fio2_ ~ OSI_exceeded + scale(spo2_) + scale(spo2_)*OSI_exceeded + scale(marp_) + scale(baseSpO2) + scale(baseFiO2) + scale(basemarp) + trt + (1|sites) + (1|sites:studysubjectid),
                            data = analysis_data_sev_97,
                            REML = T)


fio2_sev_97_sum = summary(lm_fio2_primary_sev_97)

fio2_97_coef = fio2_sev_97_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(fio2_sev_97_sum$coefficients))

saveRDS(fio2_97_coef,rds_file("fio2_97_coef"))
fio2_97_coef = readRDS(rds_file("fio2_97_coef"))


# FIO2 plot
base_data_fio2_sev_97 <- tibble(mean  = fio2_97_coef$Estimate[-1],
                                     lower = fio2_97_coef$lower[-1],
                                     upper = fio2_97_coef$upper[-1],
                                     study = c("OSI >= 12","SpO2 (standardised)","Pmean (standardised)","Baseline SpO2 (standardised)","Baseline FiO2 (standardised)","Baseline Pmean (standardised)","Conservative treatment",
                                               "OSI >= 12 * Conservative treatment"),
                                     deaths_steroid = as.character(signif((fio2_97_coef$Estimate)[-1],2)),
                                     deaths_placebo = as.character(signif(fio2_97_coef$lower[-1],2)),
                                     OR = as.character(signif(fio2_97_coef$upper[-1],2)))

jpeg(plot_file("FP_FIO2_sev_97.jpg"),width = 900,height = 500)

base_data_fio2_sev_97[c(4,5,6,2,3,1,7,8),]  |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-8,6,by = 2),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             # xlog = T,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_5 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_10 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()






# ---------------------------------------  Clinical outcome score
# formula: CS ~ pim3 + age + treatment + ethnicity + treatment*ethnicity + SpO2_baseline + site(RE) + studyid(RE)

library("ordinal")

analysis_data_sev_baseline %>% filter(!is.na(pims3_death) & !is.na(age) & !is.na(trt) & !is.na(baseSpO2) & !is.na(supportdeath) &!is.na(OSI_exceeded)) %>%
  nrow()


lm_cs_primary_clmm <-  clmm(formula = supportdeath ~ scale(pims3_death) + scale(age) + trt + OSI_exceeded+ trt*OSI_exceeded + (1|sites),
                            data = analysis_data_sev_baseline_97)

# sm_scaled = summary(lm_cs_primary_scaled)
# sm_clmm = summary(lm_cs_primary_clmm)


# sm_scaled$coefficients[31:36,]
sm_clmm$coefficients[31:35,]

sm_primary_97 = summary(lm_cs_primary_clmm)


# CS setup for forestplot
-+lm_cs_primary_coefs_sev_97 = sm_primary_97$coefficients[31:35,] %>% as_tibble()
# lm_cs_primary_coefs_sev = lm_cs_sum$coefficients[-(1:30),] %>% as_tibble()
lm_cs_primary_coefs_sev_97 = lm_cs_primary_coefs_sev_97 %>%
  rename(se = 'Std. Error') %>%
  mutate(lower = Estimate - 1.96*se) %>%
  mutate(upper = Estimate + 1.96*se) 

saveRDS(lm_cs_primary_coefs_sev_97,rds_file("lm_cs_primary_coefs_sev_97"))
lm_cs_primary_coefs_sev_97 = readRDS(rds_file("lm_cs_primary_coefs_sev_97"))

# CS plot
base_data_cs_sev_97 <- tibble::tibble(mean  = round(lm_cs_primary_coefs_sev_97$Estimate,3),
                                   lower = round(lm_cs_primary_coefs_sev_97$lower,3),
                                   upper = round(lm_cs_primary_coefs_sev_97$upper,3),
                                   study = c("PIM-3 score (standardised)","Age (standardised)","Conservative treatment","OSI >= 12",
                                             "OSI >= 12*Conservative treatment"),
                                   deaths_steroid = as.character(round(lm_cs_primary_coefs_sev_97$Estimate,3)),
                                   deaths_placebo = as.character(round(lm_cs_primary_coefs_sev_97$lower,3)),
                                   OR = as.character(round(lm_cs_primary_coefs_sev_97$upper,3)))

jpeg(plot_file("CS_sev_97.jpg"),width = 900,height = 350)

base_data_cs_sev_97 |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-1,2,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Odds ratio (log)",
             zero = 0,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_4= gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_7 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()



#### Subgroups

lm_cs_primary_clmm_PARDS <-  clmm(formula = supportdeath ~ scale(pims3_death) + scale(age) + trt + (1|sites),
                                  data = analysis_data_sev_baseline %>% filter(OSI_exceeded == 1))
lm_cs_primary_clmm_nonPARDS <-  clmm(formula = supportdeath ~ scale(pims3_death) + scale(age) + trt + (1|sites),
                                  data = analysis_data_sev_baseline %>% filter(OSI_exceeded == 0))


lm_cs_primary_clmm_PARDS$coefficients[31:35,]
lm_cs_primary_clmm_nonPARDS$coefficients[31:35,]


sm_primary_PARDS = summary(lm_cs_primary_clmm_PARDS)
sm_primary_nonPARDS = summary(lm_cs_primary_clmm_nonPARDS)

# CS setup for forestplot
lm_cs_primary_coefs_sev_PARDS = sm_primary_PARDS$coefficients[25:27,] %>% as_tibble()
lm_cs_primary_coefs_sev_nonPARDS = sm_primary_nonPARDS$coefficients[25:27,] %>% as_tibble()

lm_cs_primary_coefs_sev_PARDS = lm_cs_primary_coefs_sev_PARDS %>%
  rename(se = 'Std. Error') %>%
  mutate(lower = Estimate - 1.96*se) %>%
  mutate(upper = Estimate + 1.96*se) 
lm_cs_primary_coefs_sev_nonPARDS = lm_cs_primary_coefs_sev_nonPARDS %>%
  rename(se = 'Std. Error') %>%
  mutate(lower = Estimate - 1.96*se) %>%
  mutate(upper = Estimate + 1.96*se) 

saveRDS(lm_cs_primary_coefs_sev_97,rds_file("lm_cs_primary_coefs_sev_97"))
lm_cs_primary_coefs_sev_97 = readRDS(rds_file("lm_cs_primary_coefs_sev_97"))

# CS plot
base_data_cs_sev_97 <- tibble::tibble(mean  = round(lm_cs_primary_coefs_sev_97$Estimate,3),
                                      lower = round(lm_cs_primary_coefs_sev_97$lower,3),
                                      upper = round(lm_cs_primary_coefs_sev_97$upper,3),
                                      study = c("PIM-3 score (standardised)","Age (standardised)","Conservative treatment",
                                      deaths_steroid = as.character(round(lm_cs_primary_coefs_sev_97$Estimate,3)),
                                      deaths_placebo = as.character(round(lm_cs_primary_coefs_sev_97$lower,3)),
                                      OR = as.character(round(lm_cs_primary_coefs_sev_97$upper,3)))

jpeg(plot_file("CS_sev_97.jpg"),width = 900,height = 350)

base_data_cs_sev_97 |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-1,2,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Odds ratio (log)",
             zero = 0,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_4= gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_7 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()
