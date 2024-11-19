# instructions ------------------------------------------------------------

# 1 comment out "cutpoint <- 1" definition in modeling.R (section "time-dependent")
# 2 run modeling with each cutpoint
# 3 save tables (once)

# setup -------------------------------------------------------------------

cutpoint <- 1

source("scripts/input-brennan.R")
source('scripts/describe.R', encoding = 'UTF-8') |> suppressMessages()
# define Length of Stay in periods of 3 days (DAYStoREHABdc)
analytical <- analytical %>% mutate(los=DAYStoREHABdc/3) %>% set_variable_labels(los = attr(analytical$DAYStoREHABdc, "label"))
source('scripts/modeling.R', encoding = 'UTF-8') |> suppressMessages()
source('scripts/inference.R', encoding = 'UTF-8') |> suppressMessages()
source('scripts/plots.R', encoding = 'UTF-8') |> suppressMessages()
# source('scripts/tables-save.R', encoding = 'UTF-8') |> suppressMessages() ## only manual!

# 2 months ----------------------------------------------------------------

12*.1666 # 2 months
cutpoint <- .1666
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")

sch.df1 # none
sch.df2 # rehab, FIM MOT
writexl::write_xlsx(list("early" = sch.df1, "late" = sch.df2), "dataset/schoenfeld-2mo.xlsx")

# tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
# tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_2mo.png")

# 3 months ----------------------------------------------------------------

12*.25 # 3 months
cutpoint <- .25
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")

sch.df1 # none
sch.df2 # empl, FIM MOT
writexl::write_xlsx(list("early" = sch.df1, "late" = sch.df2), "dataset/schoenfeld-3mo.xlsx")

# tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
# tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_3mo.png")

# 4 months ----------------------------------------------------------------

12*.333 # 4 months
cutpoint <- .333
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")

sch.df1 # none
sch.df2 # none
writexl::write_xlsx(list("early" = sch.df1, "late" = sch.df2), "dataset/schoenfeld-4mo.xlsx")

# tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
# tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_4mo.png")
# tbl_merge(list(model4.1 %>% tab(everything()), model5.2 %>% tab(everything())), c("Model 4 early", "Model 5 late")) %>% write_rds("~/Downloads/Brennan delivery/tab_inf_4mo.rds")

# 6 months ----------------------------------------------------------------

12*.5 # 6 months
cutpoint <- .5
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")

sch.df1 # SCI
sch.df2 # none
writexl::write_xlsx(list("early" = sch.df1, "late" = sch.df2), "dataset/schoenfeld-6mo.xlsx")

# tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
# tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_6mo.png")


# save tables -------------------------------------------------------------

## 2 months
## ## FIM MOT COG
## ## MOT
## 3 months
## ## empl MOT COG
## ## MOT
## 4 months
## ## MOT COG
## ## none
## 6 months
## ## MOT COG
## ## none

# 12*.333 # 4 months
# cutpoint <- .333
# source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
# 
# sch.df1 # none # MOT COG
# sch.df2 # none # none
# 
# tab_app1 <- tbl_merge(
#   tbls = list(
#     model1.1 %>% tab(include = everything()) # crude HR
#     , model2.1 %>% tab(include = everything()) # aHR
#     , model3.1 %>% tab(include = everything()) # aHR
#     , model4.1 %>% tab(include = everything()) # aHR
#     #, model5.1 %>% tab(include = everything()) # aHR
#     #, model6.1 %>% tab(include = everything())
#   ),
#   tab_spanner = c("Model 1"
#                   , "Model 2"
#                   , "Model 3"
#                   , "Model 4"
#                   #              , "Model 5"
#                   #              , "Model 6"
#   )
# )
# tab_app2 <- tbl_merge(
#   tbls = list(
#     # model1.2 %>% tab(include = everything()) # crude HR
#     #,
#     model2.2 %>% tab(include = everything()) # aHR
#     , model3.2 %>% tab(include = everything()) # aHR
#     , model4.2 %>% tab(include = everything()) # aHR
#     , model5.2 %>% tab(include = everything()) # aHR
#     , model6.2 %>% tab(include = everything())
#   ),
#   tab_spanner = c(#"Model 1"
#                   #,
#     "Model 2"
#                   , "Model 3"
#                   , "Model 4"
#                   , "Model 5"
#                   , "Model 6"
#   )
# )
# 
# tab_app1
# tab_app2
# 
# tab_app1 %>% as_gt() %>% gtsave("~/Downloads/tab_app_4mo_interactions_early.rtf")
tab_app2 %>% as_gt() %>% gtsave("~/Downloads/tab_app_4mo_interactions_late.rtf")
