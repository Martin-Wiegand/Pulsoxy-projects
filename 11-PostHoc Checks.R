ggplot(data = analysis_data %>% filter(ethnic == "Asian") %>% group_by(studysubjectid) %>% reframe(n = n()) %>% arrange(desc(n))) +
  geom_histogram(aes(x = n)) +
  theme_bw() +
  labs(x = "Number of SpO2 measurements",
       y = "Number of Asian Patients")


lm_fio2_primary_sens <- lmer(fio2_ ~ ethnic + spo2_ + spo2_*ethnic + marp_ + baseSpO2 + baseFiO2 + basemarp + trt + (1|sites) + (1|sites:studysubjectid),
                        data = analysis_data %>% filter(!studysubjectid %in% ids),
                        REML = T)

summary(lm_fio2_primary_sens)
