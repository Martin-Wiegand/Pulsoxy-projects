library("lme4")
library("ordinal")
library("forestplot")

#-----------------------------------------
merged_data_set = readRDS(rds_file("merged_data_set"))

analysis_data_sev = merged_data_set %>%
  mutate(baseFiO2 = baseFiO2*100,
         fio2_ = fio2_*100) %>%
  rename(basemarp = marp) %>%
  mutate(SF_ratio = baseSpO2/baseFiO2*100) %>%
  mutate(OI = basemarp*baseFiO2/basepao2/7.5) %>%
  mutate(OSI = basemarp*baseFiO2/baseSpO2)

analysis_data_sev = analysis_data_sev %>%
  mutate(OI_exceeded = (OI >= 16) & !is.na(OI)) %>%
  mutate(OSI_exceeded = case_when(!is.na(OSI) ~ (OSI >= 12),
                                  TRUE ~ NA_real_)) %>%
  mutate(SF_exceeded = (SF_ratio >= 264) & !is.na(SF_ratio))  

analysis_data_sev %>% group_by(studysubjectid) %>% slice(1) %>% group_by(OSI > 12) %>% tally
analysis_data_sev$SF_ratio %>% summary

analysis_data_sev %>% group_by(studysubjectid) %>% slice(1) %>% group_by(is.na(OSI),is.na(OI)) %>% tally



analysis_data_sev %>% group_by(studysubjectid) %>% slice(1) %>% group_by(OSI_exceeded,OI_exceeded,SF_exceeded) %>% tally

saveRDS(analysis_data_sev,rds_file("analysis_data_sev"))

analysis_data_sev = readRDS(rds_file("analysis_data_sev"))
## ---------------------------------------  SpO2
# formula: SPO2 ~ Ethnicity + treatment + tretment*Ethnicity + Spo2_baseline + site(RE) + studyid(RE)
# Missingness 32%
analysis_data_sev %>%
  summarise(na = mean(is.na(ethnic) | is.na(baseSpO2) | is.na(spo2_)))

analysis_data_sev %>% filter(!is.na(spo2_) & !is.na(baseSpO2) & !is.na(OSI)) %>% group_by(studysubjectid) %>% n_groups()

lm_spo2_primary_sev <- lmer(spo2_ ~ OSI_exceeded + trt + trt*(OSI_exceeded) + scale(baseSpO2) + (1|sites) + (1|sites:studysubjectid),
                            data = analysis_data_sev,
                            REML = T)



spo2_sev_sum = summary(lm_spo2_primary_sev)
spo2_sev_coef = spo2_sev_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(spo2_sev_sum$coefficients))

saveRDS(spo2_sev_coef,rds_file("spo2_sev_coef"))
spo2_sev_coef = readRDS(rds_file("spo2_sev_coef"))


base_data_sev_spo2 <- tibble::tibble(mean  = spo2_sev_coef$Estimate[-1],
                                 lower = spo2_sev_coef$lower[-1],
                                 upper = spo2_sev_coef$upper[-1],
                                 study = c("OSI >= 12","Conservative treatment","Baseline SpO2 (standardised)",
                                           "OSI >= 12 * Conservative Treatment"),
                                 deaths_steroid = as.character(signif(spo2_sev_coef$Estimate[-1],2)),
                                 deaths_placebo = as.character(signif(spo2_sev_coef$lower[-1],2)),
                                 OR = as.character(signif(spo2_sev_coef$upper[-1],2)))

jpeg(plot_file("FP_SPO2_sev.jpg"),width = 900,height = 300)

base_data_sev_spo2[c(3,1,2,4),] |>
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

lm_fio2_primary_sev <- lmer(fio2_ ~ OSI_exceeded + scale(spo2_) + scale(spo2_)*OSI_exceeded + scale(marp_) + scale(baseSpO2) + scale(baseFiO2) + scale(basemarp) + trt + (1|sites) + (1|sites:studysubjectid),
                        data = analysis_data_sev,
                        REML = T)


fio2_sev_sum = summary(lm_fio2_primary_sev)
fio2_sev_coef = fio2_sev_sum$coefficients %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(fio2_sev_sum$coefficients))
# summary(lm_fio2_primary_sev)
# lm_fio2_primary_sev_ci = confint(lm_fio2_primary_sev)



saveRDS(fio2_sev_coef,rds_file("fio2_sev_coef"))
fio2_sev_coef = readRDS(rds_file("fio2_sev_coef"))


# FIO2 plot
base_data_fio2_sev <- tibble::tibble(mean  = (fio2_sev_coef$Estimate)[-1],
                                 lower = fio2_sev_coef$lower[-1],
                                 upper = fio2_sev_coef$upper[-1],
                                 study = c("OSI >= 12","SpO2 (standardised)","Pmean (standardised)","Baseline SpO2 (standardised)","Baseline FiO2 (standardised)","Baseline Pmean (standardised)","Conservative treatment",
                                           "OSI >= 12 * Conservative treatment"),
                                 deaths_steroid = as.character(signif((fio2_sev_coef$Estimate)[-1],2)),
                                 deaths_placebo = as.character(signif(fio2_sev_coef$lower[-1],2)),
                                 OR = as.character(signif(fio2_sev_coef$upper[-1],2)))

jpeg(plot_file("FP_FIO2_sev.jpg"),width = 900,height = 500)

base_data_fio2_sev[c(4,5,6,2,3,1,7,8),] |>
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
# analysis_data_baseline = analysis_data_baseline %>%
#   mutate(supportdeath = factor(supportdeath,ordered = TRUE))%>%
#   mutate(ethnic = relevel(as.factor(ethnic),ref = "White"))
library("ordinal")
analysis_data_sev_baseline = analysis_data_sev %>%
  group_by(studysubjectid) %>% dplyr::slice(1)

saveRDS(analysis_data_sev_baseline,rds_file("analysis_data_sev_baseline"))
analysis_data_sev_baseline = readRDS(rds_file("analysis_data_sev_baseline"))
analysis_data_baseline = readRDS(rds_file("analysis_data_baseline"))

analysis_data_sev_baseline %>% filter(!is.na(pims3_death) & !is.na(age) & !is.na(trt)  & !is.na(supportdeath) &!is.na(OSI_exceeded)) %>%
  group_by(OSI_exceeded,supportdeath == 31) %>%
  tally

analysis_data_sev_baseline %>% filter(!is.na(OSI_exceeded)) %>% group_by(!is.na(pims3_death),!is.na(age),!is.na(supportdeath)) %>%
  tally()

lm_cs_primary_clmm <-  clmm(formula = supportdeath ~ scale(pims3_death) + scale(age) + trt + OSI_exceeded + trt*OSI_exceeded + (1|sites),
                       data = analysis_data_sev_baseline)

cs_sev_sum = summary(lm_cs_primary_clmm)
cs_sev_coef = cs_sev_sum$coefficients[31:35,] %>% 
  as.tibble() %>% rename(se = 'Std. Error') %>% 
  mutate(lower = Estimate - 1.96*se) %>% 
  mutate(upper = Estimate + 1.96*se) %>% 
  mutate(Cov = rownames(cs_sev_sum$coefficients[31:35,]))

# CS setup for forestplot
# lm_cs_primary_coefs_sev = sm_clmm$coefficients[31:36,] %>% as_tibble()
# lm_cs_primary_coefs_sev = lm_cs_primary_coefs_sev %>%
#   rename(se = 'Std. Error') %>%
#   mutate(lower = Estimate - 1.96*se) %>%
#   mutate(upper = Estimate + 1.96*se) %>%
#   mutate(upper = exp(upper),
#          lower = exp(lower),
#          Estimate = exp(Estimate))

# saveRDS(lm_cs_primary_coefs_sev,rds_file("lm_cs_primary_coefs_sev"))
# lm_cs_primary_coefs_sev = readRDS(rds_file("lm_cs_primary_coefs_sev"))

# CS plot
base_data_cs_sev <- tibble::tibble(mean  = round(cs_sev_coef$Estimate,2),
                               lower = round(cs_sev_coef$lower,2),
                               upper = round(cs_sev_coef$upper,2),
                               study = c("PIM-3 score (standardised)","Age (standardised)","Conservative treatment","OSI >= 12",
                                         "OSI >= 12*Conservative treatment"),
                               deaths_steroid = as.character(round(cs_sev_coef$Estimate,2)),
                               deaths_placebo = as.character(round(cs_sev_coef$lower,2)),
                               OR = as.character(round(cs_sev_coef$upper,2)))

jpeg(plot_file("CS_sev.jpg"),width = 900,height = 350)

base_data_cs_sev[c(1,2,3,4,5),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-1,2,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Odds ratio (log)",
             zero = 0,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_4 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_7 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()


base_data_cs_sp <- tibble::tibble(mean  = c(-0.168,-0.056) %>% exp,
                                   lower = c(-0.341,-0.721616) %>% exp,
                                   upper = c(0.005932,0.609616) %>% exp,
                                   study = c("OSI < 12","OSI >= 12"),
                                   deaths_steroid = c("0.85","0.95"),
                                   deaths_placebo = c("0.71","0.49"),
                                   OR = c("1.01","1.84"))

jpeg(plot_file("CS_sev_simplified.png"),width = 600,height = 200)

base_data_cs_sp |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(0,2,by = 0.5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Favours Conservative        Favours Liberal         ",
             zero = 1,
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_4 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Subgroup"),
                deaths_steroid = c("Odds Ratio"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()
