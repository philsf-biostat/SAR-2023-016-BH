
source("scripts/results.R")
# source('scripts/tables-save.R', encoding = 'UTF-8') |> suppressMessages() ## only manual!

# 1 run models
# 2 save tables (once)
# 3 comment out cutpoint definition in modeling.R
# 4 run modeling with each cutpoint

12*.1666 # 2 months
cutpoint <- .1666
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_2mo.png")

12*.25 # 3 months
cutpoint <- .25
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_3mo.png")

12*.333 # 4 months
cutpoint <- .333
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_4mo.png")

12*.5 # 6 months
cutpoint <- .5
source("~/Documents/Consultoria/2023/SAR/SAR-2023-016-BH/scripts/modeling.R")
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late"))
tbl_merge(list(model4.1 %>% tab, model5.2 %>% tab), c("Model 4 early", "Model 5 late")) %>% as_gt() %>% gtsave("~/Downloads/Brennan delivery/tab_inf_6mo.png")