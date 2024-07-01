library(minpack.lm)
library(ggplot2)
library(ggforce)
library(investr)

merged_data_set = readRDS(rds_file("merged_data_set"))%>%
  mutate(fio2_ = fio2_*100,
         baseFiO2 = baseFiO2*100)

# Pao2 - Spo2 relation
pao2_data = merged_data_set %>%
  filter(!is.na(pao2paed_) & !is.na(spo2_))


ggplot(data = pao2_data) +
  geom_point(aes(x = pao2paed_,y = spo2_))


fit <- nls(spo2_ ~ 100 * pao2paed_ /(a + pao2paed_),
           start = list(a = 0),
           data = pao2_data)

fit_arctan <- nls(spo2_ ~ 200/pi*atan(a*pao2paed_),
                  start = list(a = 1),
                  data = pao2_data)

summary(fit)
summary(fit_arctan)

fct_data = pao2_data %>%
  mutate(fct = predict(fit,pao2paed_)) %>%
  mutate(fctatan = predict(fit_arctan,pao2paed_)) %>%
  select(studysubjectid,daysfromrand,spo2_,fct,pao2paed_,fctatan) %>%
  arrange(studysubjectid,daysfromrand)

fct_data %>%
  reframe(x_diff = mean(abs(fctatan-spo2_)/spo2_),
          atan_diff = mean(abs(fct-spo2_)/spo2_))

ggplot(data = fct_data)  +
  geom_point(aes(x = pao2paed_,y = spo2_)) +
  geom_line(aes(x = pao2paed_,y = fct),colour = "red") +
  geom_line(aes(x = pao2paed_,y = fctatan),colour = "blue") +
  theme_bw()

  

#### Stratify by ethnicity
  
fit_arctan_white <- nls(spo2_ ~ 100*tanh(a*pao2paed_ + b),
                    start = list(a = 0.01,b = 0),
                    data = pao2_data %>% filter(ethnic == "White"),
                    control = list(maxiter = 200))
  
fit_arctan_asian <- nls(spo2_ ~ 100*tanh(a*pao2paed_ + b),
                    start = list(a = 0.01,b = 0),
                    data = pao2_data %>% filter(ethnic == "Asian"),
                    control = list(maxiter = 200))
  
fit_arctan_black <- nls(spo2_ ~ 100*tanh(a*pao2paed_ + b),
                        start = list(a = 0.01,b = 0),
                        data = pao2_data %>% filter(ethnic == "Black"),
                        control = list(maxiter = 200))


fit_arctan_white <- nls(spo2_ ~ 100*tanh(a*pao2paed_),
                        start = list(a = 0.01),
                        data = pao2_data %>% filter(ethnic == "White"),
                        control = list(maxiter = 200))

fit_arctan_asian <- nls(spo2_ ~ 100*tanh(a*pao2paed_),
                        start = list(a = 0.01),
                        data = pao2_data %>% filter(ethnic == "Asian"),
                        control = list(maxiter = 200))

fit_arctan_black <- nls(spo2_ ~ 100*tanh(a*pao2paed_ ) ,
                        start = list(a = 0.01),
                        data = pao2_data %>% filter(ethnic == "Black"),
                        control = list(maxiter = 200))




cbind(fit_arctan_white %>% coef,confint(fit_arctan_white)) %>% round(digits = 3)
cbind(fit_arctan_asian %>% coef,confint(fit_arctan_asian)) %>% round(digits = 3)
cbind(fit_arctan_black %>% coef,confint(fit_arctan_black)) %>% round(digits = 3)

c(fit_arctan_white %>% coef,confint(fit_arctan_white)) %>% round(digits = 3)
c(fit_arctan_asian %>% coef,confint(fit_arctan_asian)) %>% round(digits = 3)
c(fit_arctan_black %>% coef,confint(fit_arctan_black)) %>% round(digits = 3)

stratified_pao2 =
bind_rows(pao2_data %>% filter(ethnic == "White") %>% mutate(fct_ethnic = predict(fit_arctan_white,new_data = pao2paed_)),
          pao2_data %>% filter(ethnic == "Black") %>% mutate(fct_ethnic = predict(fit_arctan_black,new_data = pao2paed_)),
          pao2_data %>% filter(ethnic == "Asian") %>% mutate(fct_ethnic = predict(fit_arctan_asian,new_data = pao2paed_)))



pf_white = bind_cols(pao2_data %>% filter(ethnic == "White"),predFit(fit_arctan_white,newdata = pao2_data %>% filter(ethnic == "White") ,interval = "confidence",level = 0.95))
pf_asian = bind_cols(pao2_data %>% filter(ethnic == "Asian"),predFit(fit_arctan_asian,newdata = pao2_data %>% filter(ethnic == "Asian") ,interval = "confidence",level = 0.95))
pf_black = bind_cols(pao2_data %>% filter(ethnic == "Black"),predFit(fit_arctan_black,newdata = pao2_data %>% filter(ethnic == "Black") ,interval = "confidence",level = 0.95))

pf_combined =
  bind_rows(pf_white,
            pf_asian,
            pf_black)

pf_points =
bind_rows(predFit(fit_arctan_white,newdata = tibble(pao2paed_ = seq(0,120,by = 0.1)) ,interval = "confidence",level = 0.95) %>% as_tibble %>% mutate(ethnic = "White",pao2paed_ = seq(0,120,by = 0.1)),
          predFit(fit_arctan_asian,newdata = tibble(pao2paed_ = seq(0,120,by = 0.1)) ,interval = "confidence",level = 0.95) %>% as_tibble %>% mutate(ethnic = "Asian",pao2paed_ = seq(0,120,by = 0.1)),
          predFit(fit_arctan_black,newdata = tibble(pao2paed_ = seq(0,120,by = 0.1)) ,interval = "confidence",level = 0.95) %>% as_tibble %>% mutate(ethnic = "Black",pao2paed_ = seq(0,120,by = 0.1)))

plot_1 =
ggplot() +
  geom_point(aes(x = pao2paed_,y = spo2_,colour = ethnic),alpha = 0.02,data = pao2_data %>% 
               filter(ethnic %in% c("White","Black","Asian"))) +
  geom_line(aes(x = pao2paed_,y = fit,colour = ethnic),data = pf_points) +
  geom_ribbon(aes(x = pao2paed_,ymin = lwr,ymax = upr,fill = ethnic),alpha = 0.1,show.legend = F,data = pf_points) +
  theme_bw() +
  scale_colour_manual(breaks = c("White","Asian","Black"),values = c("red","green","blue")) +
  scale_fill_manual(breaks = c("White","Asian","Black"),values = c("red","green","blue")) +
  theme(legend.position = "bottom") +
  labs(x = expression('PaO'[2]*' (mm Hg)'),
       y = expression('SpO'[2]*" (%)"),
       colour = "Ethnicity") +
  scale_x_continuous(breaks = seq(0,120,by = 10),minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0,120,by = 10),minor_breaks = NULL) +
  guides(colour = guide_legend(nrow = 1)) 


plot_1 +
  annotation_custom(ggplotGrob(plot_1  + 
                                 coord_cartesian(xlim = c(0,40),
                                                 ylim = c(80,100)) +
                                 theme_bw() +
                                 theme(legend.position = "None",
                                       plot.margin = margin(-1,-1,-1,-1, "cm")) +
                                 scale_x_continuous(breaks = seq(0,120,by = 10),minor_breaks = NULL) +
                                 scale_y_continuous(breaks = seq(0,120,by = 5),minor_breaks = NULL) +
                                 labs(x = "",
                                      y = "")),
                      xmin = 30,xmax = 115,ymin = 48,ymax = 85)


ggsave(plot_file("Pao2_Spo2_ethnicity_1coef.jpg"),width = cm(3),height = cm(3))



ggplot() +
  geom_point(aes(x = pao2paed_,y = spo2_),data = pao2_data %>% 
               filter(ethnic %in% c("White","Black","Asian")),alpha  = 0.01) +
  facet_wrap(. ~ ethnic) +
  theme_bw() +
  theme(legend.position = NULL) +
  labs(x = expression('PaO'[2]*' (mm Hg)'),
       y = expression('SpO'[2]*" (%)")) +
  geom_smooth(aes(x = pao2paed_,y = spo2_),data = pao2_data %>% 
                filter(ethnic %in% c("White","Black","Asian")),method = "gam")

ggsave(plot_file("Pao2_Spo2_ethnicity_point.jpg"),width = cm(4),height = cm(2))

dev.off()

saveRDS(pao2_data %>% filter(ethnic %in% c("White","Asian","Black"),
                             !is.na(pao2paed_) & !is.na(spo2_)),
        rds_file("exploratory_data"))
