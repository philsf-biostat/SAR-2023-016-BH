# setup -------------------------------------------------------------------

tab <- function(model) model %>% tbl_regression(exp=TRUE, include = contains("exposure")) %>% bold_labels() %>% bold_p()

model2.lab <- "Adjusted by demographic variables"
model3.lab <- "Adjusted by demographic + geographical variables"
model4.lab <- "Adjusted by demographic + geographical + clinical variables"
model5.lab <- "Adjusted by demographic + geographical + clinical variables + FIM scores"
model6.lab <- "Adjusted by demographic + geographical + clinical variables + FIM scores + Interactions"

# run analysis ------------------------------------------------------------

source('scripts/input.R', encoding = 'UTF-8') |> suppressMessages()
# source("scripts/input-brennan.R")
source('scripts/describe.R', encoding = 'UTF-8') |> suppressMessages()
source('scripts/modeling.R', encoding = 'UTF-8') |> suppressMessages()
source('scripts/inference.R', encoding = 'UTF-8') |> suppressMessages()

# participant & event counts ----------------------------------------------

md %>% nrow() %>% print()
md %>% distinct(id) %>% nrow() %>% print()
md %>% filter(outcome==1) %>% nrow() %>%  print()

paste0(
  "N=", md %>% nrow(), " observations on ",
  md %>% distinct(id) %>% nrow(), " participants,",
  " with ", md %>% filter(outcome==1) %>% nrow(), " events") %>%
  print()

# Table 2 -----------------------------------------------------------------

tab_inf <- tbl_merge(
  tbls = list(
    model1 %>% tab(), # crude HR
    model2 %>% tab() %>% modify_footnote(estimate ~ model2.lab), # aHR
    model3 %>% tab() %>% modify_footnote(estimate ~ model3.lab), # aHR
    model4 %>% tab() %>% modify_footnote(estimate ~ model4.lab), # aHR
    model5 %>% tab() %>% modify_footnote(estimate ~ model5.lab), # aHR
    model6 %>% tab() %>% modify_footnote(estimate ~ model6.lab)  # aHR
  ),
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
)

# New gtsumary feature: plot
gg.model6 <- model6 %>% tbl_regression(exp=TRUE, include = contains("exposure")) %>% plot()

# Table App ---------------------------------------------------------------

tab_app <- tbl_merge(
  tbls = list(
    model1 %>% tab(), # crude HR
    model2 %>% tab(), # aHR
    model3 %>% tab(), # aHR
    model4 %>% tab(), # aHR
    model5 %>% tab(), # aHR
    model6 %>% tab()
  ),
  tab_spanner = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
)

# save to  disk -----------------------------------------------------------

# tab_inf %>% as_gt() %>% gtsave("figures/model6_multi_tab.png")
# 
# tab_inf %>%
#   as_gt() %>%
#   as_rtf() %>%
#   writeLines(con = "report/SAR-2023-016-BH-v03-T2.rtf")
# 
# ggsave(filename = "figures/model6_multi.png", plot = gg.model6, height = 18, width = 18, units = "cm")
# 
# tab_app %>%
#   as_gt() %>%
#   as_rtf() %>%
#   writeLines(con = "report/SAR-2023-016-BH-v03-Tapp.rtf")
