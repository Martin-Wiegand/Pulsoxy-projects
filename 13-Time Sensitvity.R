library("lme4")
library("lmerTest")
library("forestploter")
library("forestplot")
library("ordinal")

################## PRIMARY ANALYSIS
analysis_data = readRDS(rds_file("analysis_data"))
analysis_data_baseline = readRDS(rds_file("analysis_data_baseline"))

## ---------------------------------------  SpO2
# formula: SPO2 ~ Ethnicity + treatment + tretment*Ethnicity + Spo2_baseline + site(RE) + studyid(RE)
# Missingness 32%
analysis_data %>%
  summarise(na = mean(is.na(ethnic) | is.na(baseSpO2) | is.na(spo2_)))

analysis_data %>% filter(!is.na(spo2_) & !is.na(baseSpO2)) %>% group_by(studysubjectid) %>% n_groups()

lm_spo2_time <- lmer(spo2_ ~ ethnic + trt + trt*(ethnic) + baseSpO2 + (1|sites) + (1|sites:studysubjectid) + daysfromrand + daysfromrand*ethnic,
                        data = analysis_data,
                        REML = T)



summary(lm_spo2_time)
lm_spo2_time_ci = confint(lm_spo2_time)

saveRDS(lm_spo2_time,rds_file("lm_spo2_time"))
saveRDS(lm_spo2_time_ci,rds_file("lm_spo2_time_ci"))

lm_spo2_time = readRDS(rds_file("lm_spo2_time"))
lm_spo2_time_ci = readRDS(rds_file("lm_spo2_time_ci"))


base_data_spo2 <- tibble::tibble(mean  = (lm_spo2_time %>% summary %>% coef)[-1,1],
                                 lower = lm_spo2_time_ci[-(1:4),1],
                                 upper = lm_spo2_time_ci[-(1:4),2],
                                 study = c("Asian","Black","Conservative treatment","Baseline SpO2 (%)","Days since randomisation",
                                           "Conservative treatment * Asian","Conservative treatment * Black","Days since randomisation * Asian","Days since randomisation * Black"),
                                 deaths_steroid = as.character(signif((lm_spo2_time %>% summary %>% coef)[-1,1],2)),
                                 deaths_placebo = as.character(signif(lm_spo2_time_ci[-(1:4),1],2)),
                                 OR = as.character(signif(lm_spo2_time_ci[-(1:4),2],2)))

base_data_spo2 = base_data_spo2[c(1,2,4,3,5,6:9),]

jpeg(plot_file("FP_SPO2_time.jpg"),width = 900,height = 500)

base_data_spo2 |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-3,1,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_5 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_7 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_11 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()

# --------------------------------------- FiO2
# formula: FiO2 ~ Ethnicity + SpO2 + SpO2*Ethnicity + MAP + SpO2_baseline +  FiO2_baseline + MAP_baseline + treatment + site(RE) + studyid(RE)
analysis_data %>% filter(!is.na(spo2_) & !is.na(baseSpO2) & !is.na(baseFiO2) & !is.na(fio2_) & !is.na(basemarp) & !is.na(marp_)) %>% 
  group_by(studysubjectid) %>% n_groups()

lm_fio2_time <- lmer(fio2_ ~ ethnic + spo2_ + spo2_*ethnic + marp_ + baseSpO2 + baseFiO2 + basemarp + trt + (1|sites) + (1|sites:studysubjectid) + daysfromrand + daysfromrand*ethnic,
                        data = analysis_data,
                        REML = T)




summary(lm_fio2_time)
lm_fio2_time_ci = confint(lm_fio2_time)

saveRDS(lm_fio2_time_ci,rds_file("lm_fio2_primary_ci"))
saveRDS(lm_fio2_time,rds_file("lm_fio2_time"))

lm_fio2_time = readRDS(rds_file("lm_fio2_time"))
lm_fio2_time_ci = readRDS(rds_file("lm_fio2_time_ci"))

# FIO2 plot
base_data_fio2 <- tibble::tibble(mean  = (lm_fio2_time %>% summary %>% coef)[-1,1],
                                 lower = lm_fio2_time_ci[-(1:4),1],
                                 upper = lm_fio2_time_ci[-(1:4),2],
                                 study = c("Asian","Black","SpO2 (%)","MArP","Baseline SpO2 (%)","Baseline FiO2 (%)","Baseline MArP","Conservative treatment","Days since randomisation",
                                           "Conservative treatment * Asian","Conservative treatment * Black","Days since randomisation * Asian","Days since randomisation * Black"),
                                 deaths_steroid = as.character(signif((lm_fio2_time %>% summary %>% coef)[-1,1],2)),
                                 deaths_placebo = as.character(signif(lm_fio2_time_ci[-(1:4),1],2)),
                                 OR = as.character(signif(lm_fio2_time_ci[-(1:4),2],2)))

jpeg(plot_file("FP_FIO2_time.jpg"),width = 900,height = 600)

base_data_fio2 |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-20,10,by = 5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |>
  fp_add_lines(h_6 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_11 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_15 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |>
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()

