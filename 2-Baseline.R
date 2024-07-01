chimera_patients = readRDS(rds_file("chimera_patients"))
analysis_data = readRDS(rds_file("analysis_data"))
exploratory_data = readRDS(rds_file("exploratory_data"))

# Exclusion crit.
baseline_data = analysis_data %>% group_by(studysubjectid) %>% slice(1) %>% ungroup

exploratory_baseline = exploratory_data %>% group_by(studysubjectid) %>% slice(1)

# baseline_data = chimera_patients %>% filter(ethnic %in% c("White","Black","Asian"))

# baseline_data = baseline_data %>%
#   mutate(baseSpO2 = case_when(baseSpO2 > 100 | baseSpO2 <= 0 ~ NA_real_,
#                               TRUE ~ baseSpO2)) %>%
#   mutate(marp = case_when(marp > 30 | marp < 6 ~ NA_real_,
#                           TRUE ~ marp)) %>%
#   mutate(syspaed = case_when(syspaed < 5 ~ NA_real_,
#                              TRUE ~ syspaed)) %>%
#   mutate(baseFiO2 = baseFiO2*100)

saveRDS(baseline_data,rds_file("baseline_data"))

# Print Table 1
rbind(c("","Liberal","Conservative"),
c("Age at randomisation",
  baseline_data %>%
  group_by(trt) %>% 
  summarise(print_continuous(age)) %>% t %>% as_tibble %>% slice(2)),
c("N",
  baseline_data %>%
  group_by(trt) %>% 
  summarise(n()) %>% t %>% as_tibble %>% slice(2)),
c("Ethnicities","",""),
c(baseline_data %>%
            filter(trt == "L") %>% 
            reframe(print_classes(ethnic)),
          baseline_data %>%
            filter(trt == "C") %>% 
            reframe(print_classes(ethnic)),by = "x") %>% as.matrix,
c("Comorbidities","",""),
  c("Airway/ Respiratory",
    baseline_data %>%
      group_by(trt) %>% 
      summarise(print_binary(awresp)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Cardiac/ Vascular",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_binary(cavas)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Neurological/ Neuromuscular",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_binary(neuro)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Congential/ Genetic/ Syndrome",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_binary(genetic)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Gastro/ Surgical",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_binary(gisurg)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Haematology/ Oncology",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_binary(haemonc)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Metabolic/ Endocrine",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_binary(metend)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Immunodeficiency",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_binary(immuno)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Other comorbidity",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_binary(othco)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Baseline FiO2",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_continuous(baseFiO2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Baseline SpO2",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_continuous(baseSpO2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Baseline PaO2",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_continuous(basepao2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Baseline MARP",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_continuous(basemarp)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("PIM-3",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_continuous(pims3_death)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Haemoglobin",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_continuous(basehb)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Blood pressure (syst)",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_continuous(syspaed)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
c("Age adjusted heart rate",
  baseline_data %>%
    group_by(trt) %>% 
    summarise(print_continuous(hrcat)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix)
) %>% 
  write.csv2(file = csv_out_file("Table1.csv"))


### Full
baseline_data_full = chimera_patients %>%
  mutate(baseSpO2 = case_when(baseSpO2 > 100 | baseSpO2 <= 0 ~ NA_real_,
                              TRUE ~ baseSpO2)) %>%
  mutate(marp = case_when(marp > 30 | marp < 6 ~ NA_real_,
                          TRUE ~ marp)) %>%
  mutate(syspaed = case_when(syspaed < 5 ~ NA_real_,
                             TRUE ~ syspaed))

saveRDS(baseline_data_full,rds_file("baseline_data_full"))

baseline_data_full = baseline_data_full %>%
  mutate(ethnic = ifelse(is.na(ethnic),"Not known",ethnic))

# Full table
rbind(c("","Liberal","Conservative"),
      c("Age at randomisation",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(age)) %>% t %>% as_tibble %>% slice(2)),
      c("N",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(n()) %>% t %>% as_tibble %>% slice(2)),
      c("Comorbidities","",""),
      c("Airway/ Respiratory",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(awresp)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Cardiac/ Vascular",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(cavas)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Neurological/ Neuromuscular",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(neuro)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Congential/ Genetic/ Syndrome",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(genetic)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Gastro/ Surgical",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(gisurg)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Haematology/ Oncology",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(haemonc)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Metabolic/ Endocrine",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(metend)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Immunodeficiency",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(immuno)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Other comorbidity",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_binary(othco)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline FiO2",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(baseFiO2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline SpO2",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(baseSpO2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline PaO2",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(basepao2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline MARP",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(marp)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("PIM-3",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(pims3_death)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Haemoglobin",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(basehb)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Blood pressure (syst)",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(syspaed)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Age adjusted heart rate",
        baseline_data_full %>%
          group_by(trt) %>% 
          summarise(print_continuous(hrcat)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix)
) %>% 
  write.csv2(file = csv_out_file("Table1_full.csv"),sep = ";")


rbind(c("","White","Asian","Black"),
      c("Age at randomisation",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(age)) %>% t %>% as_tibble %>% slice(2)) %>% unlist,
      c("N",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(n()) %>% t %>% as_tibble %>% slice(2))%>% unlist,
      c("Treatment",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(cavas)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Comorbidities","","",""),
      c("Airway/ Respiratory",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(awresp)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Cardiac/ Vascular",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(cavas)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Neurological/ Neuromuscular",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(neuro)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Congential/ Genetic/ Syndrome",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(genetic)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Gastro/ Surgical",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(gisurg)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Haematology/ Oncology",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(haemonc)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Metabolic/ Endocrine",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(metend)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Immunodeficiency",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(immuno)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Other comorbidity",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_binary(othco)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline FiO2",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(baseFiO2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline SpO2",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(baseSpO2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline PaO2",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(basepao2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline MARP",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(marp)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("PIM-3",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(pims3_death)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Haemoglobin",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(basehb)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Blood pressure (syst)",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(syspaed)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Age adjusted heart rate",
        exploratory_baseline %>%
          group_by(ethnic) %>% 
          summarise(print_continuous(hrcat)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix)
) %>% 
  write.csv2(file = csv_out_file("Table3.csv"))


#### SEVERITY BASELINE
analysis_data_sev_baseline = readRDS(rds_file("analysis_data_sev_baseline")) %>%
  filter(!is.na(OSI) & !is.na(pims3_death) & !is.na(age) &!is.na(supportdeath))

analysis_data_sev_baseline %>% group_by(trt,OSI_exceeded) %>% tally
analysis_data_sev_baseline %>% group_by(trt,OSI_exceeded) %>% summarise(print_continuous(age))

rbind(c("","OSI < 12","OSI < 12","OSI >= 12","OSI >= 12"),
      c("","Liberal Oxygenation","Conservative Oxygenation","Liberal Oxygenation","Conservative Oxygenation"),
      c("N",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(n()) %>% t %>% as_tibble %>% dplyr::slice(3))%>% unlist,
      c("Sex",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(sex == "Male")) %>% t %>% as_tibble %>% dplyr::slice(3))%>% unlist,
      c("Age at randomisation",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(age)) %>% t %>% as_tibble %>% dplyr::slice(3)) %>% unlist,
      c("PARDS",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(OSI_exceeded)) %>% t %>% as_tibble %>% dplyr::slice(3))%>% unlist,
      # c("Treatment",
      #   analysis_data_sev_baseline %>%
      #     group_by(OSI_exceeded,trt) %>% 
      #     summarise(print_binary(cavas)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      
      c("Comorbidities","",""),
      c("Airway/ Respiratory",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(awresp)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Cardiac/ Vascular",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(cavas)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Neurological/ Neuromuscular",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(neuro)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Congential/ Genetic/ Syndrome",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(genetic)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Gastro/ Surgical",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(gisurg)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Haematology/ Oncology",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(haemonc)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Metabolic/ Endocrine",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(metend)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Immunodeficiency",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(immuno)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Other comorbidity",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_binary(othco)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Baseline FiO2",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(baseFiO2)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Baseline SpO2",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(baseSpO2)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Baseline PaO2",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(basepao2)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Baseline MARP",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(basemarp) ) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("PIM-3",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(pims3_death)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Haemoglobin",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(basehb)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Blood pressure (syst)",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(syspaed)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix),
      c("Age adjusted heart rate",
        analysis_data_sev_baseline %>%
          group_by(OSI_exceeded,trt) %>% 
          summarise(print_continuous(hrcat)) %>% t %>% as_tibble %>% dplyr::slice(3) %>% as.matrix)
) %>% 
  write.csv2(file = csv_out_file("TablePARDS.csv"))




###### SEVERITY BASELINE SPO2 <= 97
analysis_data_sev_baseline_97 = readRDS(rds_file("analysis_data_sev_baseline_97")) %>%
  filter(!is.na(OSI))


rbind(c("","Liberal Oxygenation","Conservative Oxygenation"),
      c("Age at randomisation",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(age)) %>% t %>% as_tibble %>% slice(2)) %>% unlist,
      c("N",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(n()) %>% t %>% as_tibble %>% slice(2))%>% unlist,
      c("Treatment",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(cavas)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      
      c("Comorbidities","",""),
      c("Airway/ Respiratory",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(awresp)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Cardiac/ Vascular",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(cavas)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Neurological/ Neuromuscular",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(neuro)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Congential/ Genetic/ Syndrome",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(genetic)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Gastro/ Surgical",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(gisurg)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Haematology/ Oncology",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(haemonc)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Metabolic/ Endocrine",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(metend)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Immunodeficiency",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(immuno)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Other comorbidity",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_binary(othco)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline FiO2",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(baseFiO2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline SpO2",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(baseSpO2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline PaO2",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(basepao2)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Baseline MARP",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(basemarp) ) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("PIM-3",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(pims3_death)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Haemoglobin",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(basehb)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Blood pressure (syst)",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(syspaed)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix),
      c("Age adjusted heart rate",
        analysis_data_sev_baseline_97 %>%
          group_by(trt) %>% 
          summarise(print_continuous(hrcat)) %>% t %>% as_tibble %>% slice(2) %>% as.matrix)
) %>% 
  write.csv2(file = csv_out_file("TableSev97.csv"))
