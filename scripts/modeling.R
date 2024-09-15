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

# function to inspect Schoenfeld test
sch <- function(x, sort = TRUE) {
  if(class(x) != "cox.zph") stop("Not a Schoenfeld residuals object!")

  x <- x$table %>% as.data.frame()

  # sort p-values or identify terms
  if(sort) {
    x <- x %>%
      arrange(p)
  } else {
    x <- x %>%
      rownames_to_column(var = "term")
  }

  # format output
  x %>% mutate(p = style_pvalue(p))
}

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
  # PROBLEMUse = 0,
  PROBLEMUse = "No",
  # EDUCATION = "Greater than High School",
  EDUCATION = "Greater Than High School",
  EMPLOYMENT = "Employed",
  RURALdc = "Urban",
  # SCI = 0,
  SCI = "No",
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

# Mar models --------------------------------------------------------------

model6.mar1 <- model6 %>% update(.~. +Mar ) %>% suppressWarnings()
model6.mar2 <- model6 %>% update(.~. +Mar2)
model6.mar3 <- model6 %>% update(.~. +Mar3)

model6.mar1.sch <- model6.mar1 %>% cox.zph()
model6.mar2.sch <- model6.mar2 %>% cox.zph()
model6.mar3.sch <- model6.mar3 %>% cox.zph()

tab.mar <- bind_rows(
  Mar1 = sch(model6.mar1.sch, sort = FALSE),
  Mar2 = sch(model6.mar2.sch, sort = FALSE),
  Mar3 = sch(model6.mar3.sch, sort = FALSE),
  .id = "model",
) %>%
  select(-chisq, -df) %>%
  pivot_wider(names_from = term, values_from = p) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame()
colnames(tab.mar) <- tab.mar[1, ]
tab.mar <- tab.mar[-1, ]
tab.mar <- tab.mar %>% rownames_to_column("term")
