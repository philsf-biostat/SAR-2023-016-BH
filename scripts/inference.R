# setup -------------------------------------------------------------------
# library(infer)

# tables ------------------------------------------------------------------

# tables are created in tables-save.R and saved to disk. They are loaded here

tab_inf <- read_rds("dataset/tab_inf_016.rds")
tab_app <- read_rds("dataset/tab_app_016.rds")

estimates <- bind_rows(
  mod1 = model1 %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod2 = model2 %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod3 = model3 %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod4 = model4 %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod5 = model5 %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod6 = model6 %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  .id = "model",
  ) %>%
  filter(str_detect(term, "^exposure")) %>%
  mutate(term = str_remove(term, "exposure"))
