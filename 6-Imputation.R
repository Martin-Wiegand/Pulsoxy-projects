library("mice")
library("VIM")
library("ggmice")
library("broom.mixed")
library("forestplot")

analysis_data = readRDS(rds_file("analysis_data"))

## Investigate missingness
analysis_data %>%
  dplyr::select(spo2_,baseSpO2,fio2_,baseFiO2,basemarp,marp_,age,pims3_death,trt) %>%
  ungroup %>%
  reframe(spo2 = mean(is.na(spo2_),na.rm = T)*100,
          spo2sd = sd(spo2_,na.rm = T),
          baseSpO2 = mean(is.na(baseSpO2),na.rm = T)*100,
          fio2 = mean(is.na(fio2_),na.rm = T)*100,
          fio2sd = sd(fio2_,na.rm = T),
          baseFiO2 = mean(is.na(baseFiO2),na.rm = T)*100,
          marp_ = mean(is.na(marp_),na.rm = T)*100,
          basemarp = mean(is.na(basemarp),na.rm = T)*100,
          basemarp_sd = sd(basemarp,na.rm = T),
          age = mean(is.na(age),na.rm = T)*100,
          pims3_death = mean(is.na(pims3_death),na.rm = T)*100)

#
md.pattern(analysis_data %>%
             dplyr::select(spo2_,baseSpO2,fio2_,baseFiO2,basemarp,marp_,age,pims3_death,trt))

aggr_plot <- aggr(analysis_data %>%
                    dplyr::select(spo2_,baseSpO2,fio2_,baseFiO2,basemarp,marp_,age,pims3_death),
                  col = c("blue","red"),numbers = T,sortVars = T)


# Create Data sets
imp_data = analysis_data %>%
  dplyr::select(studysubjectid,sites,ethnic,trt,spo2_,baseSpO2,fio2_,baseFiO2,basemarp,marp_,age,pims3_death) %>%
  group_by(studysubjectid) %>% mutate(studysubjectid = cur_group_id()) %>%
  group_by(sites) %>% mutate(sites = cur_group_id())
  # ungroup %>%
  # mutate_at(vars(spo2_:pims3_death),scale)

md.pattern(imp_data)
aggr(imp_data %>% ungroup %>% select(marp_,spo2_,fio2_,basemarp) %>%
       rename(MArP = marp_,
              SpO2 = spo2_,
              FiO2 = fio2_,
              'MArP (bl.)' = basemarp),
     col = c("blue","red"),
     numbers = T,
     sortVars = T) 
ggsave(plot_file("Missingness_Plot.jpg"),height = cm(2),width = cm(3))

imp0 <- mice((imp_data),
             maxit = 0)

predM <- imp0$predictorMatrix
impM <- imp0$method

impM <- c("","","","","2l.pan","","2l.pan","","pmm","2l.pan","","")
predM[,1] <- -2
predM[,2] <- 2
predM[,4] <- 0
predM[,-(1:2)] <- 1

mi_data = mice(imp_data,
               method = impM,
               predictorMatrix = predM,
               maxit = 1)

## Do full scale imputation
imp40 <- mice(imp_data,
              method = impM,
              predictorMatrix = predM,
              m = 40,
              maxit = 10)

saveRDS(imp40,rds_file("imp40"))
imp40 = readRDS(rds_file("imp40"))

# Check imputation values
plot(imp40)

densityplot(imp40)

ggmice(imp40,aes(spo2_,fio2_))
plot_trace(imp40) +
  theme(legend.position = "NULL")  +
  labs(x = "Iteration") +
  scale_x_continuous(breaks = 1:10)
ggsave(plot_file("TracePlots.jpg"),width = cm(3),height = cm(4))

### Analysis
# SpO2
reg.imp.spo2 <- with(imp40,
                     lme4::lmer(spo2_ ~ ethnic + trt + trt*(ethnic) + scale(baseSpO2) + (1|sites) + (1|sites:studysubjectid),
                          REML = T))


saveRDS(reg.imp.spo2,rds_file("reg.imp.spo2"))
reg.imp.spo2 = readRDS(rds_file("reg.imp.spo2"))



pool_fct <- function(x){
  
  internal <- function(y){
    return(summary(y)$coefficients)
  }
  
  summaries = lapply(X = x,FUN = internal)
  RR_pool = Reduce('+',summaries)/length(summaries)
  
  RR_pool %>% 
    as_tibble() %>%
    rename(sterr = 'Std. Error') %>%
    mutate(lower = Estimate - 1.96*sterr) %>%
    mutate(upper = Estimate + 1.96*sterr) %>%
    select(- 't value') %>%
    return
  
}

spo2_pooled = pool_fct(x = reg.imp.spo2$analyses)


base_data_spo2 <- tibble::tibble(mean  = spo2_pooled$Estimate[-1],
                                 lower = spo2_pooled$lower[-1],
                                 upper = spo2_pooled$upper[-1],
                                 study = c("Asian","Black","Conservative treatment","Baseline SpO2 (standardised)",
                                           "Conservative treatment * Asian","Conservative treatment * Black"),
                                 deaths_steroid = as.character(signif(spo2_pooled$Estimate[-1],2)),
                                 deaths_placebo = as.character(signif(spo2_pooled$lower[-1],2)),
                                 OR = as.character(signif(spo2_pooled$upper[-1],2)))

base_data_spo2 = base_data_spo2

png(plot_file("FP_SPO2_IMP_scaled.png"),width = 1500,height = 800,res = 200)

base_data_spo2 |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-3,1,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             title = "A) Pooled model coefficients for SpO2 outcome",
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


#FiO2
reg.imp.fio2 <- with(imp40,
                     lme4::lmer(fio2_ ~ ethnic + scale(spo2_) + scale(spo2_)*ethnic +scale(marp_) + scale(baseSpO2) + scale(baseFiO2) + scale(basemarp) + trt + (1|sites) + (1|sites:studysubjectid),
                                REML = T))

saveRDS(reg.imp.fio2,rds_file("reg.imp.fio2"))
reg.imp.fio2 = readRDS(rds_file("reg.imp.fio2"))

fio2_pooled = pool_fct(reg.imp.fio2$analyses)

base_data_fio2 <- tibble::tibble(mean  = fio2_pooled$Estimate[-1],
                                 lower = fio2_pooled$lower[-1],
                                 upper = fio2_pooled$upper[-1],
                                 study = c("Asian","Black","SpO2 (standardised)","Pmean (standardised)","Baseline SpO2 (standardised)","Baseline FiO2 (standardised)","Baseline Pmean (standardised)","Conservative treatment",
                                           "SpO2 (standardised) * Asian","SpO2 (standardised) * Black"),
                                 deaths_steroid = as.character(signif(fio2_pooled$Estimate[-1],2)),
                                 deaths_placebo = as.character(signif(fio2_pooled$lower[-1],2)),
                                 OR = as.character(signif(fio2_pooled$upper[-1],2)))

png(plot_file("FP_FIO2_IMP_scaled.png"),width = 1500,height = 1000,res = 200)

base_data_fio2[c(1,2,8,5,6,7,3,4,9,10),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-15,5,by = 5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             title = "B) Pooled model coefficients for FiO2 outcome",
             # xlog = T,
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

# CS
analysis_data_baseline = readRDS(rds_file("analysis_data_baseline"))

baseline_imp = analysis_data_baseline %>%
  select(sites,pims3_death,age,trt,ethnic,baseSpO2,supportdeath) %>%
  group_by(sites) %>%
  mutate(sites = cur_group_id()) %>%
  mutate(age = as.numeric(age))

aggr(baseline_imp)

imp0 <- mice((baseline_imp),
             maxit = 0)

predM <- imp0$predictorMatrix
impM <- imp0$method

impM <- c("","","","","","","2l.pan")
predM[,1] <- -2
predM[,-(1)] <- 1

mi_data = mice(baseline_imp,
               method = impM,
               predictorMatrix = predM,
               maxit = 1)

## Do full scale imputation
imp40_bl <- mice(baseline_imp,
              method = impM,
              predictorMatrix = predM,
              m = 40,
              maxit = 10)

saveRDS(imp40_bl,rds_file("imp40_bl"))
imp40_bl = readRDS(rds_file("imp40_bl"))


# reg.imp.cs <- with(imp40_bl,
#                      ordinal::clmm2(supportdeath ~ scale(pims3_death) + scale(age) + trt + ethnic + trt*ethnic + scale(baseSpO2),
#                                     random = sites,
#                                     nAGQ = 9,
#                                     Hess = TRUE))

# reg.imp.cs <- with(imp40_bl,
#                    ordinal::clmm2(supportdeath ~ (pims3_death) + (age) + trt + ethnic + trt*ethnic + (baseSpO2),
#                                   random = sites,
#                                   nAGQ = 9,
#                                   Hess = TRUE))

reg.imp.cs <- with(imp40_bl,
                   MASS::polr(supportdeath ~ scale(pims3_death) + scale(age) + trt + ethnic + trt*ethnic + scale(baseSpO2) + (1|sites),Hess = T))

saveRDS(reg.imp.cs,rds_file("reg.imp.cs"))
reg.imp.cs = readRDS(rds_file("reg.imp.cs"))

cs_pooled = pool(reg.imp.cs)
cs_sum = summary(cs_pooled,conf.int = T)


# Plot
base_data_cs <- tibble::tibble(mean  = round(cs_sum$estimate[1:8] %>% exp,3),
                               lower = round(cs_sum$`2.5 %`[1:8]%>% exp,3),
                               upper = round(cs_sum$`97.5 %`[1:8]%>% exp,3),
                               study = c("PIM-3 score (standardised)","Age (standardised)","Conservative treatment","Asian","Black","Baseline SpO2 (standardised)",
                                         "Conservative treatment * Asian","Conservative treatment * Black"),
                               deaths_steroid = as.character(round(cs_sum$estimate[1:8]%>% exp,3)),
                               deaths_placebo = as.character(round(cs_sum$`2.5 %`[1:8]%>% exp,3)),
                               OR = as.character(round(cs_sum$`97.5 %`[1:8]%>% exp,3)))

png(plot_file("CS_IMP_scaled.png"),width = 1500,height = 1000,res = 200)

base_data_cs[c(4,5,3,6,1,2,7,8),] |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(0,3,by = 1),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Odds ratio",
             zero = 1,
             title = "C) Pooled model coefficients for clinical outcome",
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
jpeg(filename = plot_file("SpO2_aligned_MI.jpg"),width = 500,height = 1000)

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
jpeg(filename = plot_file("FiO2_aligned_MI.jpg"),width = 700,height = 1000)
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


png(filename = plot_file("CS_aligned_MI.png"),width = 1000,height = 1000)

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
