library("survival")
library("ggplot2")
library("survminer")
library("forestplot")

analysis_data = readRDS(rds_file("analysis_data"))

ventilation_data = analysis_data %>%
  filter(!is.na(hrstolib) & lib_event == 1) %>%
  group_by(studysubjectid) %>%
  filter(death != "Died") %>%
  dplyr::slice(1) %>%
  mutate(ethnic = factor(ethnic,levels = c("White","Asian","Black")))

saveRDS(ventilation_data,rds_file("ventilation_data"))

# Time to liberation from ventilation model
# Time to event ~ pim3 + age + trt+ ethnicity + treatment*ethnicity + Spo2base
cx_model = coxph(formula = Surv(time = hrstolib,event = lib_event) ~ scale(pims3_death) + scale(age) + trt + ethnic + ethnic*trt + scale(baseSpO2) + cluster(sites),
                 data = ventilation_data)

summary(cx_model)
surv_ci = confint(cx_model)

base_data_surv <- tibble::tibble(mean  = (cx_model$coefficients),
                                 lower = surv_ci[,1],
                                 upper = surv_ci[,2],
                                 study = c("PIM3 score (standardised)","Age (standardised)","Conservative treatment","Asian","Black","Baseline SpO2 (standardised)",
                                           "Conservative treatment * Asian","Conservative treatment * Black"),
                                 deaths_steroid = as.character(signif(cx_model$coefficients,2)),
                                 deaths_placebo = as.character(signif(surv_ci[,1],2)),
                                 OR = as.character(signif(surv_ci[,2],2)))

jpeg(plot_file("FP_Surv.jpg"),width = 1500,height = 800,res = 200)

base_data_surv |>
  forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
             boxsize = 0.1,
             xticks = seq(-1,1,by = 0.5),
             txt_gp = fpTxtGp(ticks=gpar(cex=1),xlab = gpar(cex = 1)),
             xlab = "Effect size",
             vertices = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue") |> 
  fp_add_lines(h_8 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
               h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
               h_10 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
  fp_add_header(study = c("Covariate"),
                deaths_steroid = c("Estimate"),
                deaths_placebo = c("Lower"),
                OR = c("Upper"))

dev.off()


# KAPLAN MEIER
# km_L <- survfit(Surv(time = hrstolib,event = lib_event) ~ Ethnicity, 
#               data = ventilation_data %>% rename(Ethnicity = ethnic) %>% mutate(treatment = ifelse(trt == "L","liberal",ifelse(trt == "C","conservative",NA))))

lib_data = ventilation_data %>% rename(Ethnicity = ethnic) %>% mutate(treatment = ifelse(trt == "L","Liberal Oxygenation",ifelse(trt == "C","Conservative Oxygenation",NA)))

km <- survfit(Surv(time = hrstolib,event = lib_event) ~ Ethnicity, 
              data = lib_data)


km_plot <- ggsurvplot_facet(km,
                            facet.by = "treatment",
                            data = lib_data,
                      conf.int = T,
                      xlim = c(0,720),
                      risk.table = T,
                      fun = "event",
                      short.panel.labs = T,
                      # panel.labs = list(ethnic = c("Asian","Black","White"),
                      #                   trt = c("Conservative","Liberal")),
                      break.time.by = 24) 

km_plot +
  scale_x_continuous(breaks = c(seq(0,168,by = 24),336,504,672),
                     labels = c(as.character(0:6),"7 days","2 Weeks","3 Weeks","4 Weeks"),
                     minor_breaks = seq(0,672,by = 24)) +
  scale_y_continuous(breaks = seq(0,1,by = 0.1),
                     labels = as.character(seq(0,100,by = 10))) +
  labs(x = "Time to liberation from ventilation",
       y = "Patients liberated (%)",
       colour = "",
       fill = "") +
  scale_fill_manual(breaks = c("White","Asian","Black"),values = c("red","green","blue")) +
  scale_colour_manual(breaks = c("White","Asian","Black"),values = c("red","green","blue")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1,size = 9))


ggsave(plot_file("KMeth.jpg"),height = cm(2),width = cm(3))
