# this script creates the tables from describe.R and inference.R, an saves them to disk, for optimum performance

# table 1 -----------------------------------------------------------------

tab_desc <- analytical %>%
  select(-FIMMOTD, -FIMCOGD,) %>%
  tbl_summary(
    include = -id,
    # by = exposure,
  ) %>%
  bold_labels() %>%
  modify_table_styling(columns = "label", align = "center")

# table 2 -----------------------------------------------------------------

tab_inf <- tbl_merge(
  tbls = list(
    mod.crude %>% tbl_regression(exp = TRUE, include = exposure) %>% bold_labels() %>% bold_p(), # crude HR
    mod.social %>% tbl_regression(exp = TRUE, include = exposure) %>% modify_footnote(estimate ~ "Adjusted by demographic variables") %>% bold_labels() %>% bold_p(), # aHR
    mod.social.clinical %>% tbl_regression(exp = TRUE, include = exposure) %>% modify_footnote(estimate ~ "Adjusted by demographic + clinical variables") %>% bold_labels() %>% bold_p(), # aHR
    mod.final %>% tbl_regression(exp = TRUE, include = exposure) %>% modify_footnote(estimate ~ "Adjusted by demographic + clinical + geographical variables") %>% bold_labels() %>% bold_p() # aHR
    # mod.late %>% tbl_regression(exp = TRUE, include = exposure) %>% modify_footnote(update = list(estimate = "Test")) %>% bold_labels() %>% bold_p() # Late deaths
  ),
  tab_spanner = c("Crude estimate", "Model 2", "Model 3", "Model 4")
)

# table A1 ----------------------------------------------------------------

# use SE instead of CI
theme_gtsummary_journal("qjecon")
theme_gtsummary_compact()

tab_app <- tbl_merge(
  tbls = list(
    mod.crude %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # crude HR
    mod.social %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # aHR
    mod.social.clinical %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # aHR
    mod.final %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p() # aHR
    # mod.late %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p() # Late deaths
  ),
  tab_spanner = c("Crude estimate", "Model 2", "Model 3", "Model 4")
)

# revert theme to previous
theme_ff_gtsummary()
theme_gtsummary_compact()

# save tables -------------------------------------------------------------

write_rds(tab_desc, "dataset/tab_desc.rds")
write_rds(tab_inf, "dataset/tab_inf.rds")
write_rds(tab_app, "dataset/tab_app.rds")
