# setup -------------------------------------------------------------------

# library(skimr) # skim
library(gtsummary)
library(gt)

# setup gtsummary theme

# theme_ff_gtsummary()
theme_gtsummary_mean_sd()
theme_gtsummary_compact()
# theme_gtsummary_language(language = "pt") # traduzir

# exploratory -------------------------------------------------------------

# overall description
# analytical %>% skimr::skim()

# outcome by exposure
analytical %>% group_by(exposure) %>% skimr::skim(outcome)

# tables ------------------------------------------------------------------

# tables are created in tables-save.R and saved to disk. They are loaded here

tab_desc <- read_rds("dataset/tab_desc_016.rds")
