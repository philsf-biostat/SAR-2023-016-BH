# 1 comment out cutpoint definition in modeling.R
# 2 run modeling with each cutpoint
# 3 save tables (once)

cutpoint <- 1

source("scripts/results.R")
# source('scripts/tables-save.R', encoding = 'UTF-8') |> suppressMessages() ## only manual!

12*.1666 # 2 months
cutpoint <- .1666
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_2mo.png")

sch.df1 # none
sch.df2 # rehab, FIM MOT
writexl::write_xlsx(list("early" = sch.df1, "late" = sch.df2), "dataset/schoenfeld-2mo.xlsx")

12*.25 # 3 months
cutpoint <- .25
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_3mo.png")

sch.df1 # none
sch.df2 # empl, FIM MOT
writexl::write_xlsx(list("early" = sch.df1, "late" = sch.df2), "dataset/schoenfeld-3mo.xlsx")

12*.333 # 4 months
cutpoint <- .333
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_4mo.png")
tbl_merge(list(model4.1 %>% tab(everything()), model5.2 %>% tab(everything())), c("Model 4 early", "Model 5 late")) %>% write_rds("~/Downloads/Brennan delivery/tab_inf_4mo.rds")

sch.df1 # none
sch.df2 # none
writexl::write_xlsx(list("early" = sch.df1, "late" = sch.df2), "dataset/schoenfeld-4mo.xlsx")

12*.5 # 6 months
cutpoint <- .5
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_6mo.png")

sch.df1 # SCI
sch.df2 # none
writexl::write_xlsx(list("early" = sch.df1, "late" = sch.df2), "dataset/schoenfeld-6mo.xlsx")
