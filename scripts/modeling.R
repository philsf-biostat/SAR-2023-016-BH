# setup -------------------------------------------------------------------
library(broom)
library(survival)

# model data
md <- analytical %>%
  select(
    # -id,
    -PriorSeiz,
    # -Mar,
    ) %>%
  drop_na()
Nobs_model <- md %>% nrow()

# raw estimate ------------------------------------------------------------

mod.crude <- coxph(Surv(Time, outcome) ~ exposure, md)

# adjusted ----------------------------------------------------------------

mod.full <- coxph(Surv(Time, outcome) ~ exposure +
                    SexF +
                    Race +
                    AGE +
                    PROBLEMUse +
                    EDUCATION +
                    EMPLOYMENT +
                    RURALdc +
                    SCI +
                    Cause +
                    RehabPay1 +
                    ResDis +
                    DAYStoREHABdc +
                    FIMMOTD +
                    FIMCOGD,
                  md)

# nested models -----------------------------------------------------------

# crude + social
mod.social <- update(mod.crude, . ~ .
                     + SexF
                     + Race
                     + AGE
                     + EDUCATION
                     + EMPLOYMENT
                     )

# crude + social + clinical
mod.social.clinical <- update(mod.social, . ~ .
                              + strata(Cause)
                              + RehabPay1
                              + SCI
                              + PROBLEMUse
                              + FIMMOTD4
                              + FIMCOGD4
                              )

# crude + social + clinical + geographical
mod.final <- update(mod.social.clinical, . ~ .
                    + ResDis
                    + RURALdc
                    )

# # add interaction terms to the model
# mod.final <- update(mod.final, . ~ . + exposure*(RehabPay1 + RURALdc))

# late deaths (over 1 year)
mod.late <- update(mod.final, data = filter(md, Time > 1))

# predictions & curves ----------------------------------------------------

newdat <- expand.grid(
  exposure = levels(analytical$exposure),
  SexF = levels(analytical$SexF),
  # SexF = "Male",
  Race = "White",
  AGE = round(mean(analytical$AGE, na.rm = TRUE)),
  PROBLEMUse = 0, # PROBLEMUse = "No",
  EDUCATION = "Greater Than High School",
  EMPLOYMENT = "Employed",
  RURALdc = "Urban",
  SCI = 0, # SCI = "No",
  Cause = "Vehicular",
  ResDis = "Private Residence",
  DAYStoREHABdc = round(mean(analytical$DAYStoREHABdc, na.rm = TRUE)),
  FIMMOTD = round(mean(analytical$FIMMOTD, na.rm = TRUE)),
  FIMCOGD = round(mean(analytical$FIMCOGD, na.rm = TRUE)),
  FIMMOTD4 = "Q2",
  FIMCOGD4 = "Q1",
  RehabPay1 = "Private Insurance"
  )
rownames(newdat) <- letters[1:10]
