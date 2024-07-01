library("readstata13")

chimera_obs = read.dta13(dta_file("chimera_obs"))
chimera_patients = read.dta13(dta_file("chimera_patients"))

# Create tibbles
chimera_obs = as_tibble(chimera_obs)
chimera_patients = as_tibble(chimera_patients)

chimera_patients = chimera_patients %>%
  mutate_at(vars(awresp:othco),
            funs(case_when(. == "Y" ~ TRUE,
                           . == "N" ~ FALSE,
                           TRUE ~ NA)))

# Ethnicity
chimera_patients = chimera_patients %>%
  mutate(ethnic = case_when(ethnic == 6 ~ "Not known",
                            ethnic == 5 ~ "Other",
                            ethnic == 4 ~ "Black",
                            ethnic == 3 ~ "Asian",
                            ethnic == 2 ~ "Mixed",
                            ethnic == 1 ~ "White",
                            TRUE ~ NA_character_)) %>%
  mutate(diagcat = case_when(diagcat == 2 ~ "Other",
                             diagcat == 1 ~ "Lower respiratory tract infection",
                             TRUE ~ NA_character_)) %>%
  mutate(OlderThan12Months = case_when(agecat == 1 ~ TRUE,
                                       agecat == 0 ~ FALSE,
                                       TRUE ~ NA)) %>%
  mutate(basesev = case_when(basesev == 2 ~ "Other",
                             basesev == 1 ~ "SF Ratio<221 with PEEP >= 5",
                             TRUE ~ NA_character_)) 

chimera_patients %>% pull(supportdeath) %>% summary
chimera_patients %>% filter(is.na(supportdeath)) %>% select(studysubjectid,trtalloc,trt,ref30,death,dtrand,dwith,dod,supportdeath,supportdays) %>% print(n = Inf)
# All missing outcomes appear to have withdrawn - propose censoring at withdrawal time?

chimera_obs = chimera_obs %>%chimera_obs = chimera_obs %>%chimera_obs = chimera_obs %>%
  group_by(studysubjectid) %>%
  arrange(studysubjectid,daysfromrand)

chimera_obs %>% names
chimera_patients %>% names

# Check data completeness - 1986
chimera_obs %>% group_by(studysubjectid) %>% n_groups()
chimera_patients %>% group_by(studysubjectid) %>% n_groups()


# Inclusion/ Exclusion





# Save
saveRDS(chimera_obs,rds_file("chimera_obs"))
saveRDS(chimera_patients,rds_file("chimera_patients"))
