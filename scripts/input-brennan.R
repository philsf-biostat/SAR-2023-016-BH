# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
# library(readxl)
# library(haven)
# library(foreign)
# library(naniar)
library(labelled)

study_period <- c("2010-01-01", "2018-12-31") %>% as.Date()
load(file = "dataset/brennan_metadata.rds")
Nobs_incl_per <- 0
Nobs_incl_id <- 0
Nobs_invalid <- 0

# tbims_dci_merged <- readRDS("data/processed/tbims_dci_merged.rds")

demographics <- c(
  "Mod1id", "Birth", "AGE",
  "SexF", "Race", "EDUCATION",
  "EMPLOYMENT", "Mar",
  "PROBLEMUse", "ZipInj",
  "ZipDis", "ZipF", "RURALdc",
  "DCIDistressScore",
  "DCIQuintile", "DCIDecile"
)

clinical <- c(
  "Mod1id", "FollowUpPeriod",
  "IntStatus", "Injury",
  "RehabDis", "Followup",
  "Death", "DeathF",
  "ResDis", "Cause",
  "PriorSeiz", "SCI",
  "RehabPay1", "DAYStoREHABdc",
  "FIMMOTD", "FIMCOGD"
)

num_vars <- c(
  "AGE",
  "DAYStoREHABdc",
  "FIMMOTD",
  "FIMCOGD",
  "FollowUpPeriod" # TROUBLESHOOT: Included in num_vars list in SAR 016 but not SAR 017
)

# load("data/processed/cc_samples_combined.RData")
# load("data/processed/ic_samples_combined.RData")
load("data/processed/analytic_samples_combined.RData")
# load("data/processed/analytic_subsamples_combined.RData")

# analytical <- cc_sample_select_covariates %>%
analytical <- analytic_sample %>%
  mutate(
    outcome = if_else(
      !is.na(TimetoDeathF),
      1,
      0
    ),
    Mar2 = Mar,
    Mar3 = Mar,
  ) %>%
  rename(
    id = Mod1id,
    exposure = DCIQuintile,
    Time = TimetoEvent,
  ) %>%
  mutate(
    # exposure = fct_rev(exposure),
    # FIMMOTD4 = fct_rev(FIMMOTD4),
    # FIMCOGD4 = fct_rev(FIMCOGD4),
  ) %>%
  select(
    -Death,
    -Birth,
    -DeathF,
    -Followup,
    -Injury,
    -RehabDis,
    -ValidDate,
    -ValidIndex,
    # -PriorSeiz,
    -AGEExpired,
    -AGECensored,
    -TimetoDeathF,
    -TimetoFollowup,
    -ZipF,
    -ZipInj,
    -ZipDis,
    -IntStatus,
    -DCIDecile,
    -DCIDistressScore,
  ) %>%
  set_variable_labels(
    exposure = "SES quintiles",
    outcome = "Mortality",
    FIMMOTD4 = str_replace(attr(analytic_sample$FIMMOTD, "label"), ":", " quartiles"),
    FIMCOGD4 = str_replace(attr(analytic_sample$FIMCOGD, "label"), ":", " quartiles"),
  )

Nobs_final <- analytical %>% nrow()
Nid_final <- analytical %>% distinct(id) %>% nrow()

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", "N") ) %>%
# analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(Nobs_final) ) ) %>%
  left_join(analytical %>% head(0) %>% mutate(id=as.character(id)), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
