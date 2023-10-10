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

model1 <- coxph(Surv(Time, outcome) ~ exposure, md, id = id)

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
                  md, id = id)

# nested models -----------------------------------------------------------

# crude + social
model2 <- update(model1, . ~ .
                     + SexF
                     + Race
                     + AGE
                     + EDUCATION
                     + EMPLOYMENT
                     )

# model2 + geographical
model3 <- update(model2, . ~ .
                    + ResDis
                    + RURALdc
                    )

# model3 + clinical
model4 <- update(model3, . ~ .
                       # + strata(Cause)
                       + RehabPay1
                       + SCI
                       + PROBLEMUse
                       + DAYStoREHABdc
                       )
# model4 + FIM scores w/ interactions
model5 <- update(model4, . ~ .
                    # + strata(Cause)
                    + FIMMOTD4
                    + FIMCOGD4
                    # + FIMMOTD4*exposure
                    # + FIMCOGD4*exposure
                    )

# FIM interactions
model6 <- update(model5, . ~ .
                          # + strata(Cause)
                          + FIMMOTD4*exposure
                          + FIMCOGD4*exposure
                          )
  
  
# # add interaction terms to the model
# mod.final <- update(mod.final, . ~ . + exposure*(RehabPay1 + RURALdc))

# late deaths (over 1 year)
# mod.late <- update(model6, data = filter(md, Time > 1))

# Schoenfeld residuals of all models --------------------------------------

model1.sch <- cox.zph(model1)
model2.sch <- cox.zph(model2)
model3.sch <- cox.zph(model3)
model4.sch <- cox.zph(model4)
model5.sch <- cox.zph(model5)
model6.sch <- cox.zph(model6)

sch.df <- bind_rows(
  model1 = sch(model1.sch, sort = FALSE),
  model2 = sch(model2.sch, sort = FALSE),
  model3 = sch(model3.sch, sort = FALSE),
  model4 = sch(model4.sch, sort = FALSE),
  model5 = sch(model5.sch, sort = FALSE),
  model6 = sch(model6.sch, sort = FALSE),
  .id = "model",
) %>%
  select(-chisq, -df) %>%
  pivot_wider(names_from = term, values_from = p) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame()
colnames(sch.df) <- sch.df[1, ]
sch.df <- sch.df[-1, ]
sch.df <- sch.df %>% rownames_to_column("term")

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
