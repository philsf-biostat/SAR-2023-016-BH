# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
# library(readxl)
# library(haven)
# library(foreign)
# library(naniar)
library(labelled)

study_period <- c("2010-01-01", "2018-12-31") %>%
  as.Date()

model <- "single"
# model <- "multiple"

# data loading ------------------------------------------------------------
set.seed(42)

# Nobs_orig, Nvar_orig, Nid_orig
load(file = "dataset/brennan_metadata.rds")
# original NVar *before* all manipulations in 017
Nvar_orig <- 711

print(model)

# data.raw <- case_when(
#   model == "single" ~
#     # option 1: single observation data - original data
#     read_rds("dataset/brennan_data_17.rds") %>%
#     ungroup() %>%
#     unnest(data) %>%
#     filter(dataset=="cc") %>%
#     select(-dataset),
#   model == "multiple" ~
#     # # option 2: multiple observations data, imputed
#     read_rds("dataset/brennan_data_17.rds") %>%
#     ungroup() %>%
#     unnest(data) %>%
#     filter(dataset=="locf") %>%
#     select(-dataset),
#   TRUE ~ stop("model definition must be single or multiple"),
#   )

if (model == "single") {

  # option 1: single observation data - original data
  data.raw <- read_rds("dataset/brennan_data_17.rds") %>%
    filter(dataset=="cc") %>%
    unnest(data) %>%
    ungroup() %>%
    select(-dataset)
  } else {

  # # option 2: multiple observations data, imputed
  data.raw <- read_rds("dataset/brennan_data_17.rds") %>%
    filter(dataset=="locf") %>%
    unnest(data) %>%
    ungroup() %>%
    select(-dataset)
  }

# save labels before processing
labs <- var_label(data.raw)

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  select(
    everything(),
  ) %>%
  rename(
    id = Mod1id,
    # ## new varnames after manipulations in SAR-2023-017
    # Date = Followup,
    # Time_d = Time,
  ) %>%
  mutate(
    # # create new Date with either DeathF OR Followup - prioritize Deaths over Followup when both are present
    # Date = if_else(is.na(DeathF), Followup, DeathF),
    # # status at followup Date
    # outcome = as.numeric(!is.na(DeathF)), # 0=alive, 1=dead
    # # time to event (in days)
    # Time_d = as.duration(interval(RehabDis, Date)),
    # Time = Time_d/dyears(1),
    Time = Time/dyears(1),
  ) %>%
  filter(
  )

# manual fixes
# ids with a wrong FU session indicator
data.raw[data.raw$id%in%c(14785, 7131 ) & data.raw$FollowUpPeriod==15, ]$FollowUpPeriod <- c(2, 1)

# exposure at discharge for single observation model
if (model == "single") {
  data.raw <- data.raw %>%
    mutate(
      exposure = DCIQuintile_Dis,
    )
  }

# rename selecting vars
demographics <- str_replace(demographics, "Mod1id", "id")
demographics <- str_replace(demographics, "DCIQuintile", "exposure")
clinical <- str_replace(clinical, "Mod1id", "id")
clinical <- str_replace(clinical, "DCIQuintile", "exposure")

# inclusion criteria: select observations starting at discharge
data.raw <- data.raw %>%
  filter(
    FollowUpPeriod >= 0 # Injury = -1, Discharge = 0
  )

# exclusion criteria: COVID is a possible confounder, use outcome Status Date to exclude
data.raw <- data.raw %>%
  filter(
    Date <= as.Date("2019-12-31") # last date (status)
  )

# exclusion criteria: before 2020
Nobs_incl_per <- data.raw %>% nrow()

# inclusion criteria: up to 10yr of follow up
data.raw <- data.raw %>%
  filter(
    FollowUpPeriod <= 10,
  )

# exclusion criteria: redundant participant observations: pick last date of follow up
if (model == "single") {
  data.raw <- data.raw %>%
    # observation at discharge is obsolete for the non TDC model
    filter(FollowUpPeriod != 0) %>%
    # remove all followups but the last
    group_by(id) %>%
    filter(
      FollowUpPeriod == max(FollowUpPeriod, na.rm = TRUE),
    ) %>%
    ungroup()
  }

# inclusion criteria: 10yr follow up + unique IDs
Nobs_incl_id <- data.raw %>% nrow()

# inclusion criteria: study period
data.raw <- data.raw %>%
  filter(
    between(RehabDis, study_period[1], study_period[2]), # discharge date
  )

# inclusion criteria: valid times
data.raw <- data.raw %>%
  filter(Time>=0)

# remove invalid observations (outcome at time 0 or below)
Nobs_invalid <- data.raw %>% nrow()

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    id = as.character(id), # or as.factor
    # label SES quintiles
    exposure = factor(exposure, labels = c("Prosperous", "Comfortable", "Mid-Tier", "At-Risk", "Distressed")),
    # age at time of injury
    AGE = if_else(is.na(AGE), floor(as.duration(interval(Birth, Injury))/dyears(1)), AGE),
    # reduce number of categories
    Race = fct_collapse(Race,
                        White = "White",
                        Black = "Black",
                        Hispanic = "Hispanic Origin",
                        other_level = "Other",
                        ),
    EDUCATION = fct_collapse(EDUCATION,
                             "Less Than High School" = c("8th Grade or Less", "9th - 11th Grade"),
                             "High School/GED" = c("GED", "HS", "HS/GED", "777"),
                             # "21" is presumed to be "Trade"
                             "Greater Than High School" = c("Trade", "21", "Some College", "Associate", "Bachelors", "Masters", "Doctorate"),
                             # other_level = "Other",
                             ),
    EMPLOYMENT = fct_collapse(EMPLOYMENT,
                              Employed = "Employed",
                              Unemployed = "Unemployed",
                              other_level = "Other",
                              ),
    RehabPay1 = fct_collapse(RehabPay1,
                             "Private Insurance" = c("Private Insurance", "Workers Compensation", "Auto Insurance"),
                             "Public Insurance" = c("Medicaid", "Medicare", "State or County"),
                             other_level = "Other",
                             ),
    ResDis = fct_collapse(ResDis,
                          "Private Residence" = "Private Residence",
                          other_level = "Other",
                          ),
    Cause = fct_collapse(Cause,
                         Vehicular = c("Motor Vehicle", "Motorcycle", "Bicycle", "All-Terrain Vehicle (ATV) and All-Terrain Cycle (ATC)", "Other Vehicular: Unclassified"),
                         Falls = "Fall",
                         Violence = c("Gunshot Wound", "Assaults With Blunt Instrument", "Other Violence"),
                         other_level = "Other",
                         ),
    SCI = as.numeric(SCI == "Yes"),
    PROBLEMUse = as.numeric(PROBLEMUse == "Yes"),
    # relevel variables
    SexF = fct_relevel(SexF, "Male"),
    RehabPay1 = fct_relevel(RehabPay1, "Private Insurance"),
    EDUCATION = fct_relevel(EDUCATION, "Greater Than High School"),
    RURALdc = fct_relevel(RURALdc, "Suburban"),
    # exposure = relevel(exposure, "Mid-Tier"),
    # FIM quartiles
    FIMMOTD4 = cut(FIMMOTD, breaks = c(0, quantile(FIMMOTD, probs = c(.25, .50, .75), na.rm = TRUE), 100), labels = c("Q1", "Q2", "Q3", "Q4")), #, labels = c("Q1", "Q2", "Q3", "Q4"), right = FALSE
    FIMCOGD4 = cut(FIMCOGD, breaks = c(0, quantile(FIMCOGD, probs = c(.25, .50, .75), na.rm = TRUE), 100), labels = c("Q1", "Q2", "Q3", "Q4")),
    # aggregate Mar into broader categories
    # option 1
    Mar2 = fct_collapse(Mar, Single = "Single (Never Married)", Married = "Married", Sep = c("Divorced", "Separated"), Other = c("Widowed", "Other")),
    # option 2
    Mar3 = fct_other(Mar, keep = c("Single (Never Married)", "Married")),
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    exposure = "SES quintiles",
    outcome = "Mortality",
    # reprocessed vars
    AGE = labs$AGE,
    SCI = labs$SCI,
    PROBLEMUse = labs$PROBLEMUse,
    # new vars
    Time = "Time of follow up (years)",
    Date = "Date of last follow up",
    FIMMOTD4 = str_replace(attr(data.raw$FIMMOTD, "label"), ":", " quartiles"),
    FIMCOGD4 = str_replace(attr(data.raw$FIMCOGD, "label"), ":", " quartiles"),
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    id,
    exposure,
    outcome,
    Date,
    Time,
    everything(),
    -starts_with("Zip"),
    -starts_with("DCI"),
    -where(is.Date),
    -IntStatus,
    # -FollowUpPeriod,
    # -Time_d,
    # -FIMMOTF,
    # -FIMCOGF,
  )

Nvar_final <- analytical %>% ncol
Nobs_final <- analytical %>% nrow

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", "N") ) %>%
# analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(Nobs_final) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
