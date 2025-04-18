
# Follow-up time (continuous and categorical)
# using model data (md), as this is the relevant dataset for the PH assumptions
md %>%
  mutate(
    fu = case_when(
      Time < 2             ~ factor("<2"),
      Time >= 2 & Time < 5 ~ factor("2-5"),
      Time >= 5            ~ factor(">=5"),
    ),
  ) %>% select(Time, fu) %>%
  tbl_summary(
    label = list(Time="Follow-up time, continuous (years)", fu="Follow-up time, categorical (years)"),
  ) #%>% as_gt() %>% gtsave("~/Downloads/tab_follow-up_time.rtf")

# counts of Education categories (collapsed)
# Obs: raw data is not available in the "Brennan data" dataset
md %>% count(EDUCATION)

# raw data EDUCATION
read_rds("dataset/raw/tbims_form1_raw.rds") %>%
  # missing data treatment: explicit NA
  naniar::replace_with_na(replace=list(EDUCATION=c(999))) %>%
  # convert haven_labelled to factor (missing value codes are used automatically)
  mutate(across(where(is.labelled), as_factor),) %>%
  select(EDUCATION) %>%
  mutate(EDUCATION = fct_recode(EDUCATION, "Bachelor's or greater" = "21")) %>%
  mutate(across(where(is.factor), fct_drop)) %>%
  tbl_summary() #%>% as_gt() %>% gtsave("~/Downloads/tab_education_raw.rtf")
