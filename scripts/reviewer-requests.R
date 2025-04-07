
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
  )

# counts of Education categories (collapsed)
# Obs: raw data is not available in the "Brennan data" dataset
md %>% count(EDUCATION)
