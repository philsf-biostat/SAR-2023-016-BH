# setup -------------------------------------------------------------------
# library(infer)

# tables ------------------------------------------------------------------

# tables are created in tables-save.R and saved to disk. They are loaded here

tab_inf <- read_rds("dataset/tab_inf_016.rds")
tab_app <- read_rds("dataset/tab_app_016.rds")

estimates <- bind_rows(
  mod1 = mod.crude %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod2 = mod.social %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod3 = mod.social.geo %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod4 = mod.social.geo.clinical %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod5 = mod.final %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  mod6 = mod.interaction %>% tidy(conf.int = TRUE, exponentiate = TRUE),
  .id = "model",
  ) %>%
  filter(str_detect(term, "^exposure")) %>%
  mutate(term = str_remove(term, "exposure"))
