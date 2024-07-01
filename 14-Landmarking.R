joint_data_mort = readRDS(rds_file("joint_data_mort"))
merged_data_set = readRDS(rds_file("merged_data_set"))

joint_data_mort_full = joint_data_mort %>%
  mutate(time_to_death_full = case_when(!is.na(dod) & !is.na(dtrand) ~ difftime(dod,dtrand,units = "days"),
                                        TRUE ~ NA)) %>%
  mutate(status = case_when(!is.na(dod) ~ "died",
                            !is.na(hospitaldays) & is.na(dod) ~ "discharged")) %>%
  mutate(time_to_event = case_when(status == "died" ~ as.numeric(time_to_death_full),
                                   status == "discharged" ~ as.numeric(hospitaldays))) %>%
  filter(daysfromrand <= hospitaldays)

ggplot(data = joint_data_mort_full) +
  geom_histogram(aes(as.numeric(time_to_death_full)),binwidth = 1) +
  theme_bw()

# joint_data_mort_lm = joint_data_mort_full %>%
#   filter(!is.na(oxygen_delivery_adjusted))

### Create LM frame
# data = joint_data_mort_full %>%
#   rename(time = daysfromrand)

make_lm <- function(data,steps,horizon){
  skeleton <- data %>% 
    group_by(studysubjectid) %>% 
    reframe(day = ceiling(max(time)))
  
  skeleton = skeleton %>% 
    uncount(day,.remove = F) %>%
    group_by(studysubjectid) %>%
    mutate(day = 1:day)
  
  skeleton = skeleton %>%
    left_join(data %>% select(studysubjectid,status,time_to_event) %>% unique,by = "studysubjectid")
  
  skeleton = skeleton %>%
    mutate(time_to_event_window = case_when(time_to_event > day + horizon ~ horizon,
                                            TRUE ~ (time_to_event - day))) %>%
    mutate(status_window = case_when((time_to_event <= day + horizon) & status == "died" ~ 1,
                                     TRUE  ~ 0))
  
  data %>% group_by(studysubjectid) %>%
     mutate(time)
}

add_values <- function(data_lm,number_days,varname){
  data_lm %>%
    group_by(studysubjectid,day) %>% 
    reframe(sum_val = sum(!!sym(varname),na.rm = T),
            n = n()) %>%
    mutate(vars= case_when(day > 1 ~ (sum_val + lag(sum_val))/(n + lag(n)),
                            day == 1 ~ sum_val/n)) %>%
    select(studysubjectid,day,!!varname)
}

joint_data_mort_full %>% filter(!is.na(oxygen_delivery_adjusted)) %>%
  group_by(studysubjectid) %>%
  arrange(daysfromrand) %>%
  dplyr::slice(1) %>%
  pull(daysfromrand) %>%floor %>%
  as.numeric %>%
    hist(breaks = 100,freq = F)

lm_coef <- function(x,y){
  coef(lm(y ~ x))[2] %>%
    as.numeric %>%
    return
}



# ------------------------------------------- First 3 days only
ods_3 =
joint_data_mort_full %>%
  group_by(studysubjectid) %>%
  filter(daysfromrand <= 3,
         !is.na(oxygen_delivery_adjusted)) %>%
  mutate(oxygen_delivery_adjusted = oxygen_delivery_adjusted/100000) %>%
  arrange(daysfromrand) %>%
  reframe(ods_slope3 = lm_coef(daysfromrand,oxygen_delivery_adjusted),
          ods_mean3 = mean(oxygen_delivery_adjusted))
  
ods_2 =
  joint_data_mort_full %>%
  group_by(studysubjectid) %>%
  filter(daysfromrand <= 2,
         !is.na(oxygen_delivery_adjusted)) %>%
  mutate(oxygen_delivery_adjusted = oxygen_delivery_adjusted/100000) %>%
  arrange(daysfromrand) %>%
  reframe(ods_slope2 = lm_coef(daysfromrand,oxygen_delivery_adjusted),
          ods_mean2 = mean(oxygen_delivery_adjusted))

ods_1 =
  joint_data_mort_full %>%
  group_by(studysubjectid) %>%
  filter(daysfromrand <= 1,
         !is.na(oxygen_delivery_adjusted)) %>%
  mutate(oxygen_delivery_adjusted = oxygen_delivery_adjusted/100000) %>%
  arrange(daysfromrand) %>%
  reframe(ods_slope1 = lm_coef(daysfromrand,oxygen_delivery_adjusted),
          ods_mean1 = mean(oxygen_delivery_adjusted))

survival_data = 
joint_data_mort_full %>%
  select(studysubjectid,trt,sites,baseSpO2,baseFiO2,pims3_death,age,time_to_event,status,marp) %>%
  group_by(studysubjectid) %>%
  unique %>%
  left_join(ods_1,by = "studysubjectid") %>%
  left_join(ods_2,by = "studysubjectid") %>%
  left_join(ods_3,by = "studysubjectid") %>%
  mutate(time_to_3 = case_when(time_to_event > 3 & time_to_event <= 33 ~ time_to_event - 3,
                            time_to_event <= 3 ~ NA_real_,
                            time_to_event > 33 ~ 30)) %>%
  mutate(death_3 = case_when(time_to_event > 3 & time_to_event <= 33 & status == "died" ~ 1,
                              time_to_event <= 3 ~ NA_real_,
                              TRUE ~ 0)) %>%
  mutate(time_to_2 = case_when(time_to_event > 2 & time_to_event <= 32 ~ time_to_event - 2,
                               time_to_event <= 2 ~ NA_real_,
                               time_to_event > 32 ~ 30)) %>%
  mutate(death_2 = case_when(time_to_event > 2 & time_to_event <= 32 & status == "died" ~ 1,
                             time_to_event <= 2 ~ NA_real_,
                             TRUE ~ 0)) %>%
  mutate(time_to_1 = case_when(time_to_event > 1 & time_to_event <= 31 ~ time_to_event - 1,
                               time_to_event <= 1 ~ NA_real_,
                               time_to_event > 31 ~ 30)) %>%
  mutate(death_1 = case_when(time_to_event > 1 & time_to_event <= 31 & status == "died" ~ 1,
                             time_to_event <= 1 ~ NA_real_,
                             TRUE ~ 0)) 

survival_data = survival_data %>%
  left_join(merged_data_set %>% select(studysubjectid,age) %>% rename(age_raw = age),by = "studysubjectid")

survFit_mort_1 <- coxph(Surv(time_to_1, death_1) ~ trt + ods_mean1 + ods_slope1 + trt*ods_mean1 + trt*ods_slope1 + scale(baseSpO2) + scale(age_raw) + scale(pims3_death), 
                        data = survival_data,
                        x = TRUE,
                        model = TRUE)  

survFit_mort_2 <- coxph(Surv(time_to_2, death_2) ~ trt + ods_mean2 + ods_slope2 + trt*ods_mean2 + trt*ods_slope2 + scale(baseSpO2) + scale(age_raw) + scale(pims3_death), 
                        data = survival_data,
                        x = TRUE,
                        model = TRUE)  

survFit_mort_3 <- coxph(Surv(time_to_3, death_3) ~ trt + ods_mean3 + ods_slope3 + trt*ods_mean3 + trt*ods_slope3+ scale(baseSpO2) + scale(age_raw) + scale(pims3_death), 
                        data = survival_data,
                        x = TRUE,
                        model = TRUE)  

sum1 = summary(survFit_mort_1)
sum2 = summary(survFit_mort_2)
sum3 = summary(survFit_mort_3)






fp_plot <- function(x,day){
  y <- tibble::tibble(mean  = x$conf.int[,1],
                                     lower = x$conf.int[,3],
                                     upper = x$conf.int[,4],
                                     study = c("Treatment (conservative)",paste0("Mean ODS (Day ",day,")"),paste0("Slope ODS (Day ",day,")"),"Baseline SpO2 (standardised)","Age (standardised)",
                                               "PIMS-3 (standardised)",paste0("Conservative treatment * Mean ODS (Day ",day,")"),paste0("Conservative treatment * Slope ODS (Day ",day,")")),
                                     deaths_steroid = as.character(signif(x$conf.int[,1],3)),
                                     deaths_placebo = as.character(signif(x$conf.int[,3],3)),
                                     OR = as.character(signif(x$conf.int[,4],3)))
  
  y[c(1,4,5,6,2,3,7,8),] |>
    forestplot(labeltext = c(study, deaths_steroid, deaths_placebo, OR),
               boxsize = 0.1,
               xticks = seq(0,2,by = 0.5),
               txt_gp = fpTxtGp(ticks=gpar(cex=1),
                                xlab = gpar(cex = 1)),
               zero = 1,
               xlab = "Decreases mortality     Increases mortality",
               vertices = TRUE) |>
    fp_set_style(box = "royalblue",
                 line = "darkblue") |> 
    fp_add_lines(h_8 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
                 h_6 = gpar(lty = 2,lwd = 1, columns = 1:4, col = "#000044"),
                 h_2 = gpar(lwd = 1, columns = 1:4, col = "#000044"),
                 h_10 = gpar(lwd = 1, columns = 1:4, col = "#000044")) |> 
    fp_add_header(study = c("Covariate"),
                  deaths_steroid = c("Estimate"),
                  deaths_placebo = c("Lower"),
                  OR = c("Upper"))
}

jpeg(plot_file("FP_Surv_ODS_1.jpg"),width = 1800,height = 800,res = 200)
fp_plot(sum1,"1")
dev.off()

jpeg(plot_file("FP_Surv_ODS_2.jpg"),width = 1800,height = 800,res = 200)
fp_plot(sum2,"2")
dev.off()

jpeg(plot_file("FP_Surv_ODS_3.jpg"),width = 1800,height = 800,res = 200)
fp_plot(sum3,"3")
dev.off()



create_slope <- function(time,id,data,window = 1){
  data_int = data %>%
    filter(studysubjectid == id,
           daysfromrand <= time,
           time - window < daysfromrand,
           !is.na(oxygen_delivery_adjusted))
  
  if(nrow(data) > 1){
    lm(formula = oxygen_delivery_adjusted ~ daysfromrand,data = data_int)$coef[2] %>% as.numeric %>% return
  }else{
    return(NA_real_)
  }
}

joint_data_mort_slope = joint_data_mort_full %>%
  filter(!is.na(oxygen_delivery_adjusted)) %>%
  rowwise() %>%
  mutate(ods_slope_3 = create_slope(time = daysfromrand,id = studysubjectid,data = joint_data_mort_full %>%
                                      mutate(oxygen_delivery_adjusted = oxygen_delivery_adjusted/10000),window = 3))








saveRDS(joint_data_mort_slope,rds_file("joint_data_mort_slope"))
joint_data_mort_slope = readRDS(rds_file("joint_data_mort_slope"))

# ************ Time varying approach
joint_data_mort_temp = joint_data_mort_slope %>%
  group_by(studysubjectid) %>%
  mutate(time_2 = daysfromrand,
         time_1 = lag(daysfromrand,default = 0),
         ods_value = lag(oxygen_delivery_adjusted/10000),
         ods_slope = lag(ods_slope_3))

# Create last 
event_row = joint_data_mort_slope %>% 
  group_by(studysubjectid) %>%
  arrange(daysfromrand) %>%
  summarise(status = unique(status),
            time_1 = max(daysfromrand),
            time_2 = unique(time_to_event),
            ods_value = last(ods_slope_3),
            ods_slope = last(oxygen_delivery_adjusted/10000))

time_varying_frame =
add_row(joint_data_mort_temp %>%
          select(studysubjectid,status,time_1,time_2,ods_value,ods_slope)  %>%
          mutate(status = "ongoing")%>% 
          ungroup(),
        event_row %>% 
          ungroup()) %>%
  arrange(studysubjectid,time_1) 

time_varying_frame %>% print(n = 30)

time_varying_frame = time_varying_frame %>%
  mutate(status = case_when(status == "died" ~ 1,
                            TRUE ~ 0))

# Add baseline values
time_varying_frame = time_varying_frame %>%
  left_join(merged_data_set %>%
              select(studysubjectid,age,pims3_death,baseSpO2,trt) %>%
              group_by(studysubjectid) %>% 
              unique,
            by = "studysubjectid")



library("survival")

fit <- coxph(Surv(time = time_1,time2 = time_2,event = status) ~ (age) + (pims3_death) + (baseSpO2) + ods_value + ods_slope + cluster(studysubjectid),
             data = time_varying_frame)

surv_sum = (fit %>% summary)$coefficients %>% as_tibble()
surv_table = surv_sum %>%
  rename(logEstimate = coef) %>%
  rename(se = "se(coef)") %>%
  rename(Estimate = "exp(coef)") %>%
  rename(p = 'Pr(>|z|)') %>%
  mutate(lowerlog = logEstimate - 1.96*se) %>%
  mutate(upperlog = logEstimate + 1.96*se) %>%
  mutate(lower = exp(lowerlog),
         upper = exp(upperlog)) %>%
  mutate(covariate = fit$coefficients %>% names)

cox.zph(fit) %>% plot
abline(h = 0)

library("finegray")
fit <- coxph(Surv(time = time_1,time2 = time_2,event = status) ~ (age) + (pims3_death) + (baseSpO2) + ods_value + ods_slope + cluster(studysubjectid),
             data = time_varying_frame)

#********** Create 3 day data frame

# joint_data_mort_full %>%
  # filter(!is.na(oxygen_delivery_adjusted))%>%
  # mutate(oxygen_delivery_adjusted= oxygen_delivery_adjusted/10000) %>%
  # group_by(studysubjectid) %>%
  # arrange(studysubjectid,daysfromrand) %>%
  # mutate(ods_auc = cumsum((daysfromrand - lag(daysfromrand,default = 0))*oxygen_delivery_adjusted)/daysfromrand) %>%
  # mutate(time_to_event = supportdays,
  #        event = case_when(supportdays == 31 ~ "deceased",
  #                          TRUE ~ "discharged")) %>%
  # # filter(!is.na(event)) %>%
  # select(studysubjectid,daysfromrand,time_to_event,event,ods_auc,age,pims3_death) %>%
  # mutate(time1 = lag(daysfromrand,default = ),
  #        time2 = daysfromrand) %>%
  # print(n = 30)


######## FIRST THREE DAYS AUC THING

# ************ Time varying approach
joint_data_mort_3day = joint_data_mort_slope %>%
  group_by(studysubjectid) %>%
  mutate(time_2 = daysfromrand,
         time_1 = lag(daysfromrand,default = 0),
         ods_value = lag(oxygen_delivery_adjusted/10000),
         ods_slope = lag(ods_slope_3)) %>%
  filter(!is.na(oxygen_delivery_adjusted))%>%
  mutate(oxygen_delivery_adjusted= oxygen_delivery_adjusted/10000) %>%
  group_by(studysubjectid) %>%
  arrange(studysubjectid,daysfromrand) %>%
  mutate(ods_auc = cumsum((daysfromrand - lag(daysfromrand,default = 0))*oxygen_delivery_adjusted)/daysfromrand) %>%
  filter(daysfromrand <= 3) %>%
  mutate(ods_auc = lag(ods_auc))

# check
joint_data_mort_3day %>%
  select(studysubjectid,daysfromrand,time_1,time_2,ods_auc,oxygen_delivery_adjusted,ods_value)

# Create last 
event_row = joint_data_mort_3day %>% 
  group_by(studysubjectid) %>%
  arrange(daysfromrand) %>%
  summarise(status = unique(status),
            time_1 = max(daysfromrand),
            time_2 = unique(time_to_event),
            ods_value = last(ods_slope_3),
            ods_slope = last(oxygen_delivery_adjusted/10000),
            ods_auc = last(ods_auc))

time_varying_frame_3d =
  add_row(joint_data_mort_3day %>%
            select(studysubjectid,status,time_1,time_2,ods_value,ods_slope,ods_auc)  %>%
            mutate(status = "ongoing")%>% 
            ungroup(),
          event_row %>% 
            ungroup()) %>%
  arrange(studysubjectid,time_1) 

time_varying_frame_3d %>% print(n = 30)

time_varying_frame_3d = time_varying_frame_3d %>%
  mutate(binary = case_when(status == "died" ~ 1,
                            TRUE ~ 0))

# Add baseline values
time_varying_frame_3d = time_varying_frame_3d %>%
  left_join(merged_data_set %>%
              select(studysubjectid,age,pims3_death,baseSpO2,trt) %>%
              group_by(studysubjectid) %>% 
              unique,
            by = "studysubjectid") 

time_varying_frame_3d = time_varying_frame_3d %>%
  mutate(status = case_when(time_2 > 30 & status != "ongoing" ~ "ongoing",
                            T ~ status)) %>%
  mutate(time_2 = case_when(time_2 > 30 ~ 30,
                            T ~ time_2))

time_varying_frame_3d = time_varying_frame_3d %>%
  group_by(studysubjectid) %>%
  mutate(status = case_when(time_2 == 30 ~ "ongoing",
                            TRUE ~ status))

saveRDS(time_varying_frame_3d,rds_file("time_varying_frame_3d"))
time_varying_frame_3d = readRDS(rds_file("time_varying_frame_3d"))

# time_varying_frame_3d$status %>% table
# 
# time_varying_frame_3d = time_varying_frame_3d %>%
#   group_by(studysubjectid) %>%
#   mutate(status = case_when(time_2 == 30 ~ "ongoing",
#                             TRUE ~ status))

cmprsk_3d_frame <- time_varying_frame_3d %>%
  mutate(status_num = case_when(status == "ongoing" ~ 0,
                                status == "died" ~ 1,
                                status == "discharged" ~ 2)) %>%
  group_by(studysubjectid) %>%
  mutate(time_cr = max(time_2) - time_1) %>%
  mutate(status_cr = case_when(any(status == "discharged") ~ 2,
                               any(status == "died") ~ 1))

##### COX MODEL. TIME VARYING COVARIATES
cox_3d_mort <- coxph(Surv(time = time_1,time2 = time_2,event = status == "died") ~ (age) + (pims3_death) + ods_auc + cluster(studysubjectid),
             data = time_varying_frame_3d)

cox_3d_disc <- coxph(Surv(time = time_1,time2 = time_2,event = status == "discharged") ~ (age) + (pims3_death) + ods_auc + cluster(studysubjectid),
                data = time_varying_frame_3d)

summary(cox_3d_mort)
summary(cox_3d_disc)

### Competing risks
fg_3d_mort <- crrc(ftime = cmprsk_3d_frame$time_2,
             fstatus = cmprsk_3d_frame$status_num,
             cov1 = cmprsk_3d_frame %>% ungroup %>% select(age,pims3_death,ods_auc),
             cluster = cmprsk_3d_frame$studysubjectid,
             failcode = 1,
             cencode = 0)

fg_3d_disc <- crrc(ftime = cmprsk_3d_frame$time_2,
                   fstatus = cmprsk_3d_frame$status_num,
                   cov1 = cmprsk_3d_frame %>% ungroup %>% select(age,pims3_death,ods_auc),
                   cluster = cmprsk_3d_frame$studysubjectid,
                   failcode = 2,
                   cencode = 0)

# fg_3d_cr <- crrc(ftime = cmprsk_3d_frame$time_cr,
#                  fstatus = cmprsk_3d_frame$status_cr,
#                  cov1 = cmprsk_3d_frame %>% ungroup %>% select(age,pims3_death,ods_auc),
#                  cluster = cmprsk_3d_frame$studysubjectid,
#                  failcode = 1,
#                  cencode = 0)

# summary(fg_3d_cr)
summary(fg_3d_mort)
summary(fg_3d_disc)

# What's the difference to the last data point only?
cmprsk_3d_last = cmprsk_3d_frame %>%
  group_by(studysubjectid) %>%
  dplyr::slice(n())

fg_3d_l <- crr(ftime = cmprsk_3d_last$time_2,
              fstatus = cmprsk_3d_last$status_num,
              cov1 = cmprsk_3d_last %>% ungroup %>% select(age,pims3_death,ods_auc),
              failcode = 1,
              cencode = 0)

summary(fg_3d_l)

library("cmprsk")
library("crrSC")
library("timereg")
comp.risk()




### Sensitivity
# status_names <- list(
#   'died'="Died (n = 36)",
#   'discharged'="Discharged (n = 1070)"
# )
# 
# status_labeller <- function(variable,value){
#   return(status_names[value])
# }

# ggplot(data = cmprsk_3d_frame %>%
#          group_by(studysubjectid) %>%
#          arrange(time_2) %>%
#          dplyr::slice(n())  %>%
#          filter(status != "ongoing"))) +
#   geom_histogram(aes(y = after_stat(time_2),fill = status))  +
#   geom_density(aes(x = time_2,fill = status),kernel = "epanechnikov") +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   facet_wrap(. ~ status,scales = "free_y")


ggplot(cmprsk_3d_frame %>%
         group_by(studysubjectid) %>%
         arrange(time_2) %>%
         dplyr::slice(n())  %>%
         filter(status != "ongoing")  %>%
         mutate(status = case_when(status == "died" ~ "Died (n=36)",
                                   status == "discharged" ~ "Discharged (n=1050)")), 
       aes(time_2, fill = status)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = 'identity',
                 alpha = 0.9) +
  geom_density(bw = 3,alpha = 0.2,kernel = "epanechnikov")  +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(. ~ status,scales = "free_y") +
  labs(fill = "",
       y = "",
       x = "Time in days")

ggsave(plot_file("TimeToEvent.png"),width = 4,height = 3)

# ggplot(data = cmprsk_3d_frame %>%
#          group_by(studysubjectid) %>%
#          arrange(time_2) %>%
#          dplyr::slice(n()) %>%
#          filter(status == "died")) +
#   geom_histogram(aes(x = (time_2)),fill = "blue") 

# cmprsk_3d_frame %>%
#   group_by(studysubjectid) %>%
#   arrange(time_2) %>%
#   dplyr::slice(n()) %>%
#   filter(status != "ongoing") %>%
#   group_by(status) %>%
#   reframe(print_continuous(time_2))


# cmprsk_3d_frame %>%
#   group_by(studysubjectid) %>%
#   arrange(time_2) %>%
#   dplyr::slice(n()) %>%
#   # filter(status != "ongoing") %>%
#   group_by(status) %>% tally


#***** Sensitivity, days 1, 2 and 3
#*fg_3d_mort <- crrc(ftime = cmprsk_3d_frame$time_2,
cmprsk_3d_frame_day1 <- cmprsk_3d_frame %>% 
  group_by(studysubjectid) %>% 
  mutate(number = 1:n()) %>% 
  filter(time_1 < 1 | number == n()) %>%
  mutate(time_1 = case_when(number == max(number) ~ lag(time_1),
                            TRUE ~ time_1)) %>%
  mutate(number = 1:n()) %>% 
  filter(number != (n() - 1))

cmprsk_3d_frame_day2 <- cmprsk_3d_frame %>% 
  group_by(studysubjectid) %>% 
  mutate(number = 1:n()) %>% 
  filter((time_1 < 2 & time_1 >= 1) | number == n()) %>%
  mutate(time_1 = case_when(number == max(number) ~ lag(time_1),
                            TRUE ~ time_1)) %>%
  mutate(number = 1:n()) %>% 
  filter(number != (n() - 1))

cmprsk_3d_frame_day3 <- cmprsk_3d_frame %>% 
  group_by(studysubjectid) %>% 
  mutate(number = 1:n()) %>% 
  filter((time_1 < 3 & time_1 >= 2) | number == n()) %>%
  mutate(time_1 = case_when(number == max(number) ~ lag(time_1),
                            TRUE ~ time_1)) %>%
  mutate(number = 1:n()) %>% 
  filter(number != (n() - 1))

# Day 1
fg_3d_mort_1 <- crrc(ftime = cmprsk_3d_frame_day1$time_2,
                   fstatus = cmprsk_3d_frame_day1$status_num,
                   cov1 = cmprsk_3d_frame_day1 %>% ungroup %>% select(age,pims3_death,ods_auc),
                   cluster = cmprsk_3d_frame_day1$studysubjectid,
                   failcode = 1,
                   cencode = 0)

fg_3d_disc_1 <- crrc(ftime = cmprsk_3d_frame_day1$time_2,
                   fstatus = cmprsk_3d_frame_day1$status_num,
                   cov1 = cmprsk_3d_frame_day1 %>% ungroup %>% select(age,pims3_death,ods_auc),
                   cluster = cmprsk_3d_frame_day1$studysubjectid,
                   failcode = 2,
                   cencode = 0)

# Day 2
fg_3d_mort_2 <- crrc(ftime = cmprsk_3d_frame_day2$time_2,
                     fstatus = cmprsk_3d_frame_day2$status_num,
                     cov1 = cmprsk_3d_frame_day2 %>% ungroup %>% select(age,pims3_death,ods_auc),
                     cluster = cmprsk_3d_frame_day2$studysubjectid,
                     failcode = 1,
                     cencode = 0)

fg_3d_disc_2 <- crrc(ftime = cmprsk_3d_frame_day2$time_2,
                     fstatus = cmprsk_3d_frame_day2$status_num,
                     cov1 = cmprsk_3d_frame_day2 %>% ungroup %>% select(age,pims3_death,ods_auc),
                     cluster = cmprsk_3d_frame_day2$studysubjectid,
                     failcode = 2,
                     cencode = 0)

# Day 3
fg_3d_mort_3 <- crrc(ftime = cmprsk_3d_frame_day3$time_2,
                     fstatus = cmprsk_3d_frame_day3$status_num,
                     cov1 = cmprsk_3d_frame_day3 %>% ungroup %>% select(age,pims3_death,ods_auc),
                     cluster = cmprsk_3d_frame_day3$studysubjectid,
                     failcode = 1,
                     cencode = 0)

fg_3d_disc_3 <- crrc(ftime = cmprsk_3d_frame_day3$time_2,
                     fstatus = cmprsk_3d_frame_day3$status_num,
                     cov1 = cmprsk_3d_frame_day3 %>% ungroup %>% select(age,pims3_death,ods_auc),
                     cluster = cmprsk_3d_frame_day3$studysubjectid,
                     failcode = 2,
                     cencode = 0)


m_1 = summary(fg_3d_mort_1)
d_1 = summary(fg_3d_disc_1)
m_2 = summary(fg_3d_mort_2)
d_2 = summary(fg_3d_disc_2)
m_3 = summary(fg_3d_mort_3)
d_3 = summary(fg_3d_disc_3)

rbind(c("Covariate","Estimate (odds)","Lower","Upper","P-value"),
cbind(rep("Day 1",3),c("Age","PIMS-3","ODS"),round(m_1$coef[,2],4),round(m_1$conf.int[,c(3,4)],4),round(m_1$coef[,5],4)),
cbind(rep("Day 2",3),c("Age","PIMS-3","ODS"),round(m_2$coef[,2],4),round(m_2$conf.int[,c(3,4)],4),round(m_2$coef[,5],4))
)




cox_d1_mort <- coxph(Surv(time = time_1,time2 = time_2,event = status == "died") ~ (age) + (pims3_death) + ods_auc + cluster(studysubjectid),
                     data = cmprsk_3d_frame_day1)

cox_d1_disc <- coxph(Surv(time = time_1,time2 = time_2,event = status == "discharged") ~ (age) + (pims3_death) + ods_auc + cluster(studysubjectid),
                     data = cmprsk_3d_frame_day1)


cox_d2_mort <- coxph(Surv(time = time_1,time2 = time_2,event = status == "died") ~ (age) + (pims3_death) + ods_auc + cluster(studysubjectid),
                     data = cmprsk_3d_frame_day2)

cox_d2_disc <- coxph(Surv(time = time_1,time2 = time_2,event = status == "discharged") ~ (age) + (pims3_death) + ods_auc + cluster(studysubjectid),
                     data = cmprsk_3d_frame_day2)


cox_d3_mort <- coxph(Surv(time = time_1,time2 = time_2,event = status == "died") ~ (age) + (pims3_death) + ods_auc + cluster(studysubjectid),
                     data = cmprsk_3d_frame_day3)

cox_d3_disc <- coxph(Surv(time = time_1,time2 = time_2,event = status == "discharged") ~ (age) + (pims3_death) + ods_auc + cluster(studysubjectid),
                     data = cmprsk_3d_frame_day3)

### NONLINEAR

### Competing risks
fg_3d_mort_nonlin <- crrc(ftime = cmprsk_3d_frame$time_2,
                   fstatus = cmprsk_3d_frame$status_num,
                   cov1 = cmprsk_3d_frame %>% ungroup %>% select(age,pims3_death,ods_auc) %>% mutate(ods_auc2 = ods_auc^2),
                   cluster = cmprsk_3d_frame$studysubjectid,
                   failcode = 1,
                   cencode = 0)

fg_3d_disc_nonlin <- crrc(ftime = cmprsk_3d_frame$time_2,
                   fstatus = cmprsk_3d_frame$status_num,
                   cov1 = cmprsk_3d_frame %>% ungroup %>% select(age,pims3_death,ods_auc) %>% mutate(ods_auc2 = ods_auc^2),
                   cluster = cmprsk_3d_frame$studysubjectid,
                   failcode = 2,
                   cencode = 0)

summary(fg_3d_mort_nonlin)
summary(fg_3d_disc_nonlin)


cox_d3_mort_nl <- coxph(Surv(time = time_1,time2 = time_2,event = status == "died") ~ (age) + (pims3_death) + pspline(ods_auc,df = 3)  + cluster(studysubjectid),
                     data = cmprsk_3d_frame)

cox_d3_disc_nl <- coxph(Surv(time = time_1,time2 = time_2,event = status == "discharged") ~ (age) + (pims3_death) + pspline(ods_auc,df = 3) + cluster(studysubjectid),
                     data = cmprsk_3d_frame)

summary(cox_d3_mort_nl)
summary(cox_d3_disc_nl)
