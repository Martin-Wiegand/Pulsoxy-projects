analysis_data = readRDS(rds_file("analysis_data"))

analysis_data = analysis_data %>%
  mutate(sf_ratio = spo2_/fio2_)



ggplot(analysis_data, aes(sf_ratio, group = ethnic)) + 
  geom_histogram(aes(y = stat(density) * 0.5), binwidth = 0.5,closed = "left",boundary = 0) + 
  scale_y_continuous(breaks = c(0,0.05,0.1,0.15,0.2),labels = c("0%","5%","10%","15%","20%"),minor = NULL) +
  scale_x_continuous(breaks = 0:5,minor_breaks = NULL) +
  facet_wrap(~ ethnic) +
  theme_bw() +
  labs(x = "SpO2/FiO2 ratio",y = "Percentage within group")

ggsave(plot_file("SF_ratio.jpg"),width = cm(3),height = cm(3))

ks.test(x = analysis_data %>% filter(ethnic == "White") %>% pull(sf_ratio),
        y = analysis_data %>% filter(ethnic == "Black") %>% pull(sf_ratio))

ks.test(x = analysis_data %>% filter(ethnic == "White") %>% pull(sf_ratio),
        y = analysis_data %>% filter(ethnic == "Asian") %>% pull(sf_ratio))

ks.test(x = analysis_data %>% filter(ethnic == "Asian") %>% pull(sf_ratio),
        y = analysis_data %>% filter(ethnic == "Black") %>% pull(sf_ratio))

x = seq(0,5,by = 0.1)
ecdf_white = ecdf(analysis_data %>% filter(ethnic == "White") %>% pull(sf_ratio))
ecdf_black = ecdf(analysis_data %>% filter(ethnic == "Black") %>% pull(sf_ratio))
ecdf_asian = ecdf(analysis_data %>% filter(ethnic == "Asian") %>% pull(sf_ratio))

plot_tibble = bind_rows(tibble(x = x,
                               y = ecdf_white(x),
                               ethnic = "White"),
                        tibble(x = x,
                               y = ecdf_black(x),
                               ethnic = "Black"),
                        tibble(x = x,
                               y = ecdf_asian(x),
                               ethnic = "Asian"))

ggplot(plot_tibble) +
  geom_line(aes(x = x,y = y,colour = ethnic),linewidth = 1.2) +
  theme_bw() +
  labs(y = "Cumulative Distribution Function",
       x = "S/F ratio",
       colour = "") +
  scale_y_continuous(breaks = seq(0,1,by = 0.2),minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(0,5,by = 1),minor_breaks = NULL) +
  theme(legend.position = "bottom")
  
ggsave(plot_file("SF_CDF.jpg"),width = cm(3),height = cm(3))
