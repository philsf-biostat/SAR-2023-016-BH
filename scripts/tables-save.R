# this script creates the tables from describe.R and inference.R, an saves them to disk, for optimum performance

theme_se <- list(
  `pkgwide-str:theme_name` = "QJECON without linebreaks",
  `tbl_regression-arg:conf.int` = FALSE,
  `tbl_summary-fn:percent_fun` = function(x) style_number(x, digits = 1, scale = 100),
  `tbl_regression-fn:addnl-fn-to-run` = function(x) {
    new_header_text <- paste(
      x$table_styling$header %>% filter(.data$column == "estimate") %>% pull("label"),
      "**(SE)**", sep = " ")
    estimate_footnote <- x$table_styling$footnote_abbrev %>%
      filter(.data$column %in% "estimate") %>%
      filter(dplyr::row_number() == dplyr::n(), !is.na(.data$footnote)) %>%
      dplyr::pull("footnote") %>%
      c("SE = Standard Error") %>%
      paste(collapse = ", ")
    x %>% add_significance_stars(pattern = "{estimate}{stars} ({std.error})", hide_se = TRUE) %>%
      modify_header(list(estimate = new_header_text)) %>%
      modify_footnote(estimate ~ estimate_footnote, abbreviation = TRUE)
  },
  `as_gt-lst:addl_cmds` = list(
    tab_spanner = list(rlang::expr(gt::fmt_markdown(columns = everything())),
                       rlang::expr(gt::tab_style(style = "vertical-align:top", locations = gt::cells_body(columns = dplyr::any_of("label"))))
    )))

tab <- function(model, include = contains("exposure"), ...) {
  model %>%
    tbl_regression(
      exp = TRUE,
      include = include,
      ...
      ) %>%
    bold_labels() %>%
    bold_p()
}

# table 1 -----------------------------------------------------------------

# requires manually disabling "filter(FollowUpPeriod != 0)" in input (exclusion criteria: redundant participant observations: pick last date of follow up)
tab_desc <- analytical %>%
  filter(FollowUpPeriod==0) %>%
  select(
    -FIMMOTD,
    -FIMCOGD,
    -FollowUpPeriod,
    -Time,
    -outcome,
  ) %>%
  tbl_summary(
    include = -id,
    # by = exposure,
    missing_text = "Missing",
  ) %>%
  bold_labels() %>%
  modify_table_styling(columns = "label", align = "center")

# table 2 -----------------------------------------------------------------

model2.lab <- "Adjusted by demographic variables"
model3.lab <- "Adjusted by demographic + geographical variables"
model4.lab <- "Adjusted by demographic + geographical + clinical variables"
model5.lab <- "Adjusted by demographic + geographical + clinical variables + FIM scores"
model6.lab <- "Adjusted by demographic + geographical + clinical variables + FIM scores + Interactions"

tab_inf <- tbl_merge(
  tbls = list(
    model1 %>% tab(conf.int = FALSE), # crude HR
    model2 %>% tab(conf.int = FALSE) %>% modify_footnote(estimate ~ model2.lab), # aHR
    model3 %>% tab(conf.int = FALSE) %>% modify_footnote(estimate ~ model3.lab), # aHR
    model4 %>% tab(conf.int = FALSE) %>% modify_footnote(estimate ~ model4.lab), # aHR
    model5 %>% tab(conf.int = FALSE) %>% modify_footnote(estimate ~ model5.lab), # aHR
    model6 %>% tab(conf.int = FALSE) %>% modify_footnote(estimate ~ model6.lab)  # aHR
  ),
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
  )

# table A1 ----------------------------------------------------------------

# use SE instead of CI
# theme_gtsummary_journal("qjecon")
set_gtsummary_theme(theme_se)
theme_gtsummary_compact()

tab_app <- tbl_merge(
  tbls = list(
    model1 %>% tab(include = everything()), # crude HR
    model2 %>% tab(include = everything()), # aHR
    model3 %>% tab(include = everything()), # aHR
    model4 %>% tab(include = everything()), # aHR
    model5 %>% tab(include = everything()), # aHR
    model6 %>% tab(include = everything())
  ),
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
  )

# revert theme to previous
theme_ff_gtsummary()
theme_gtsummary_compact()

# save tables -------------------------------------------------------------

write_rds(tab_desc, "dataset/tab_desc_016.rds")
write_rds(tab_inf, "dataset/tab_inf_016.rds")
write_rds(tab_app, "dataset/tab_app_016.rds")
model6 %>% tab() %>% modify_footnote(estimate ~ model6.lab) %>% write_rds("dataset/tab_m6_016.rds")
