analysis_data_severity = readRDS(rds_file("analysis_data_sev"))
analysis_data_severity_97 = readRDS(rds_file("analysis_data_sev_97"))

#***** Strat by OSI, SPO2
ggplot(analysis_data_severity %>% filter(OSI_exceeded == 1)) +
  geom_histogram(aes(x = spo2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1) +
  theme_bw()  +
  labs(x = "SpO2",y = "Percentage (%)",title = "OSI >= 12") +
  coord_cartesian(xlim = c(70,100),ylim = c(0,15))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_OSI_exceeded.jpg"),width = cm(1.5),height = cm(2))

ggplot(analysis_data_severity %>% filter(OSI_exceeded == 0)) +
  geom_histogram(aes(x = spo2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1) +
  theme_bw()  +
  labs(x = "SpO2",y = "Percentage (%)",title = "OSI < 12") +
  coord_cartesian(xlim = c(70,100),ylim = c(0,15))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_OSI_under.jpg"),width = cm(1.5),height = cm(2))


# Stratified by treatment
ggplot(analysis_data_severity %>% filter(OSI_exceeded == 1) %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = spo2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1,alpha = 0.4,position = "identity") +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "SpO2",y = "Percentage (%)",title = "OSI >= 12",fill = "") +
  coord_cartesian(xlim = c(70,100),ylim = c(0,10))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_OSI_exceeded_trt.jpg"),width = cm(1.5),height = cm(2))

ggplot(analysis_data_severity %>% filter(OSI_exceeded == 0) %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = spo2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1,alpha = 0.4,position = "identity") +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "SpO2",y = "Percentage (%)",title = "OSI < 12",fill = "") +
  coord_cartesian(xlim = c(70,100),ylim = c(0,10))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_OSI_under_trt.jpg"),width = cm(1.5),height = cm(2))






#***** Strat by OSI, FIO2
ggplot(analysis_data_severity %>% filter(OSI_exceeded == 1)) +
  geom_histogram(aes(x = fio2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "stack") +
  theme_bw()  +
  labs(x = "FiO2",y = "Percentage (%)",title = "OSI >= 12") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL) +
  coord_cartesian(ylim = c(0,40))
ggsave(plot_file("FiO2_OSI_exceeded.jpg"),width = cm(1.5),height = cm(2))

ggplot(analysis_data_severity %>% filter(OSI_exceeded == 0)) +
  geom_histogram(aes(x = fio2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "stack") +
  theme_bw()  +
  labs(x = "FiO2",y = "Percentage (%)",title = "OSI < 12") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL) +
  coord_cartesian(ylim = c(0,40))
ggsave(plot_file("FiO2_OSI_under.jpg"),width = cm(1.5),height = cm(2))

# Stratified by treatment
ggplot(analysis_data_severity %>% filter(OSI_exceeded == 1) %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = fio2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "identity",alpha = 0.4) +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "FiO2",y = "Percentage (%)",title = "OSI >= 12",fill = "") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL) +
  coord_cartesian(ylim = c(0,30))
ggsave(plot_file("FiO2_OSI_exceeded_trt.jpg"),width = cm(1.5),height = cm(2))

ggplot(analysis_data_severity %>% filter(OSI_exceeded == 0) %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = fio2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "identity",alpha = 0.4) +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "FiO2",y = "Percentage (%)",title = "OSI < 12",fill = "") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL)+
  coord_cartesian(ylim = c(0,30))
ggsave(plot_file("FiO2_OSI_under_trt.jpg"),width = cm(1.5),height = cm(2))
