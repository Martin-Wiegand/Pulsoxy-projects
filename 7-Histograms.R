analysis_data = readRDS(rds_file("analysis_data"))

ggplot(analysis_data %>% filter(ethnic == "White")) +
  geom_histogram(aes(x = spo2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1) +
  theme_bw()  +
  labs(x = "SpO2",y = "Percentage (%)",title = "White") +
  coord_cartesian(xlim = c(70,100))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_ethnicity_white.jpg"),width = cm(1),height = cm(2))
                 
ggplot(analysis_data %>% filter(ethnic == "Asian")) +
  geom_histogram(aes(x = spo2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1) +
  theme_bw()  +
  labs(x = "SpO2",y = "Percentage (%)",title = "Asian") +
  coord_cartesian(xlim = c(70,100))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_ethnicity_asian.jpg"),width = cm(1),height = cm(2))

ggplot(analysis_data %>% filter(ethnic == "Black")) +
  geom_histogram(aes(x = spo2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1) +
  theme_bw()  +
  labs(x = "SpO2",y = "Percentage (%)",title = "Black") +
  coord_cartesian(xlim = c(70,100))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_ethnicity_black.jpg"),width = cm(1),height = cm(2))

# ------------------------- Fio2
ggplot(analysis_data %>% filter(ethnic == "White")) +
  geom_histogram(aes(x = fio2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "stack") +
  theme_bw()  +
  labs(x = "FiO2",y = "Percentage (%)",title = "White") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL)
ggsave(plot_file("FiO2_ethnicity_white.jpg"),width = cm(1),height = cm(2))

ggplot(analysis_data %>% filter(ethnic == "Asian")) +
  geom_histogram(aes(x = fio2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "stack") +
  theme_bw()  +
  labs(x = "FiO2",y = "Percentage (%)",title = "Asian") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL)
ggsave(plot_file("FiO2_ethnicity_asian.jpg"),width = cm(1),height = cm(2))

ggplot(analysis_data %>% filter(ethnic == "Black")) +
  geom_histogram(aes(x = fio2_,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "stack") +
  theme_bw()  +
  labs(x = "FiO2",y = "Percentage (%)",title = "Black") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL)
ggsave(plot_file("FiO2_ethnicity_black.jpg"),width = cm(1),height = cm(2))












### ----------------- STRATIFEID BY TREATMENT
ggplot(analysis_data %>% filter(ethnic == "White") %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = spo2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1,alpha = 0.4,position = "identity") +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "SpO2",y = "Percentage (%)",title = "White",fill = "") +
  coord_cartesian(xlim = c(70,100))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_ethnicity_white_trt.jpg"),width = cm(1),height = cm(2))

ggplot(analysis_data %>% filter(ethnic == "Asian") %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = spo2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1,alpha = 0.4,position = "identity") +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "SpO2",y = "Percentage (%)",title = "Asian",fill = "") +
  coord_cartesian(xlim = c(70,100))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_ethnicity_asian_trt.jpg"),width = cm(1),height = cm(2))

ggplot(analysis_data %>% filter(ethnic == "Black") %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = spo2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 1,alpha = 0.4,position = "identity") +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "SpO2",y = "Percentage (%)",title = "Black",fill = "") +
  coord_cartesian(xlim = c(70,100))  +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,15,by = 5),minor_breaks = seq(0,15,1))
ggsave(plot_file("SpO2_ethnicity_black_trt.jpg"),width = cm(1),height = cm(2))

# ------------------------- Fio2
ggplot(analysis_data %>% filter(ethnic == "Black") %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = fio2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "identity",alpha = 0.4) +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "FiO2",y = "Percentage (%)",title = "Black",fill = "") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL)

ggsave(plot_file("FiO2_ethnicity_black_trt.jpg"),width = cm(1),height = cm(2))

ggplot(analysis_data %>% filter(ethnic == "White") %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = fio2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "identity",alpha = 0.4) +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "FiO2",y = "Percentage (%)",title = "White",fill = "") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL)

ggsave(plot_file("FiO2_ethnicity_white_trt.jpg"),width = cm(1),height = cm(2))

ggplot(analysis_data %>% filter(ethnic == "Asian") %>% mutate(trt = ifelse(trt == "L","Liberal","Conservative"))) +
  geom_histogram(aes(x = fio2_,fill = trt,y = ((..count..)/sum(..count..)) * 100 ),binwidth = 5,origin = 0,position = "identity",alpha = 0.4) +
  theme_bw()  +
  theme(legend.position = "bottom") +
  labs(x = "FiO2",y = "Percentage (%)",title = "Asian",fill = "") +
  scale_x_continuous(breaks = seq(0,100,by = 10),minor_breaks = NULL)  +
  scale_y_continuous(breaks = seq(0,45,by = 5),minor_breaks = NULL)

ggsave(plot_file("FiO2_ethnicity_asian_trt.jpg"),width = cm(1),height = cm(2))
