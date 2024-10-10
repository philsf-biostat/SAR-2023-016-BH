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

# formulas ----------------------------------------------------------------

# model1 = crude
formula1 <- formula(Surv(Time, outcome) ~ exposure)

# model2 = model1 + social
formula2 <- formula(. ~ .
                    + SexF
                    + Race
                    + AGE
                    + EDUCATION
                    + EMPLOYMENT
)

# model3 = model2 + geographical
formula3 <- formula(. ~ .
                    # + ResDis
                    + RURALdc
)

# model4 = model3 + clinical
formula4 <- formula(. ~ .
                    # + strata(Cause)
                    + RehabPay1
                    + SCI
                    + PROBLEMUse
                    + DAYStoREHABdc
)

# model5 = model4 + FIM scores
formula5 <- formula(. ~ .
                    # + strata(Cause)
                    + FIMMOTD
                    + FIMCOGD
                    # + FIMMOTD4
                    # + FIMCOGD4
)

# model6 = model5 + FIM interactions
formula6 <- formula(. ~ .
                    # + strata(Cause)
                    # + FIMMOTD4*exposure
                    # + FIMCOGD4*exposure
                    + FIMMOTD*exposure
                    + FIMCOGD*exposure
                    )

# raw estimate ------------------------------------------------------------

# model1 = crude
model1 <- coxph(formula1, md, id = id)

# adjusted ----------------------------------------------------------------

# mod.full <- coxph(Surv(Time, outcome) ~ exposure +
#                     SexF +
#                     Race +
#                     AGE +
#                     PROBLEMUse +
#                     EDUCATION +
#                     EMPLOYMENT +
#                     RURALdc +
#                     SCI +
#                     Cause +
#                     RehabPay1 +
#                     ResDis +
#                     DAYStoREHABdc +
#                     FIMMOTD +
#                     FIMCOGD,
#                   md, id = id)

# nested models -----------------------------------------------------------

# model2 = model1 + social
model2 <- update(model1, formula2)

# model3 = model2 + geographical
model3 <- update(model2, formula3)

# model4 = model3 + clinical
model4 <- update(model3, formula4)

# model5 = model4 + FIM scores
model5 <- update(model4, formula5)

# # model6 = model5 + FIM interactions
# model6 <- update(model5, formula6)

# # model6 = model5 + FIM interactions
# model6 <- update(model5, . ~ .
#                           # + strata(Cause)
#                           # + FIMMOTD4*exposure
#                           # + FIMCOGD4*exposure
#                           - FIMMOTD
#                           - FIMCOGD
#                           + tt(FIMMOTD)
#                           + tt(FIMCOGD)
#                  , tt = function(x, t, ...) x*log(t+.5)
#                           )

# Schoenfeld residuals of all models --------------------------------------

model1.sch <- cox.zph(model1)
model2.sch <- cox.zph(model2)
model3.sch <- cox.zph(model3)
model4.sch <- cox.zph(model4)
model5.sch <- cox.zph(model5)
# model6.sch <- cox.zph(model6)

sch.df <- bind_rows(
  model1 = sch(model1.sch, sort = FALSE),
  model2 = sch(model2.sch, sort = FALSE),
  model3 = sch(model3.sch, sort = FALSE),
  model4 = sch(model4.sch, sort = FALSE),
  model5 = sch(model5.sch, sort = FALSE),
  # model6 = sch(model6.sch, sort = FALSE),
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
  EDUCATION = "Greater than High School",
  # EDUCATION = "Greater Than High School",
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

# model6.mar1 <- model6 %>% update(.~. +Mar ) %>% suppressWarnings()
# model6.mar2 <- model6 %>% update(.~. +Mar2)
# model6.mar3 <- model6 %>% update(.~. +Mar3)
# 
# model6.mar1.sch <- model6.mar1 %>% cox.zph()
# model6.mar2.sch <- model6.mar2 %>% cox.zph()
# model6.mar3.sch <- model6.mar3 %>% cox.zph()
# 
# tab.mar <- bind_rows(
#   Mar1 = sch(model6.mar1.sch, sort = FALSE),
#   Mar2 = sch(model6.mar2.sch, sort = FALSE),
#   Mar3 = sch(model6.mar3.sch, sort = FALSE),
#   .id = "model",
# ) %>%
#   select(-chisq, -df) %>%
#   pivot_wider(names_from = term, values_from = p) %>%
#   as.matrix() %>%
#   t() %>%
#   as.data.frame()
# colnames(tab.mar) <- tab.mar[1, ]
# tab.mar <- tab.mar[-1, ]
# tab.mar <- tab.mar %>% rownames_to_column("term")

# time-dependent ----------------------------------------------------------

# time split (years)
cutpoint <- 1

md2 <- md %>%
  mutate(
    status1 = ifelse(Time < cutpoint, outcome, 0),
    status2 = ifelse(Time >=cutpoint, outcome, 0),
  )

model1.1 <- coxph(Surv(Time, status1) ~ exposure, md2)
model1.2 <- coxph(Surv(Time, status2) ~ exposure, md2)

model2.1 <- update(model1.1, formula2)
model2.2 <- update(model1.2, formula2)

model3.1 <- update(model2.1, formula3)
model3.2 <- update(model2.2, formula3)

model4.1 <- update(model3.1, formula4)
model4.2 <- update(model3.2, formula4)

model5.1 <- update(model4.1, formula5)
model5.2 <- update(model4.2, formula5)

model6.1 <- update(model5.1, formula6)
model6.2 <- update(model5.2, formula6)

model1.1.sch <- model1.1 %>% cox.zph()
model1.2.sch <- model1.2 %>% cox.zph()
model2.1.sch <- model2.1 %>% cox.zph()
model2.2.sch <- model2.2 %>% cox.zph()
model3.1.sch <- model3.1 %>% cox.zph()
model3.2.sch <- model3.2 %>% cox.zph()
model4.1.sch <- model4.1 %>% cox.zph()
model4.2.sch <- model4.2 %>% cox.zph()
model5.1.sch <- model5.1 %>% cox.zph()
model5.2.sch <- model5.2 %>% cox.zph()
model6.1.sch <- model6.1 %>% cox.zph()
model6.2.sch <- model6.2 %>% cox.zph()

sch.df1 <- bind_rows(
  model1.1 = sch(model1.1.sch, sort = FALSE),
  model2.1 = sch(model2.1.sch, sort = FALSE),
  model3.1 = sch(model3.1.sch, sort = FALSE),
  model4.1 = sch(model4.1.sch, sort = FALSE),
  model5.1 = sch(model5.1.sch, sort = FALSE),
  model6.1 = sch(model6.1.sch, sort = FALSE),
  .id = "model",
) %>%
  select(-chisq, -df) %>%
  pivot_wider(names_from = term, values_from = p) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame()
colnames(sch.df1) <- sch.df1[1, ]
sch.df1 <- sch.df1[-1, ]
sch.df1 <- sch.df1 %>% rownames_to_column("term")

sch.df2 <- bind_rows(
  model1.2 = sch(model1.2.sch, sort = FALSE),
  model2.2 = sch(model2.2.sch, sort = FALSE),
  model3.2 = sch(model3.2.sch, sort = FALSE),
  model4.2 = sch(model4.2.sch, sort = FALSE),
  model5.2 = sch(model5.2.sch, sort = FALSE),
  model6.2 = sch(model6.2.sch, sort = FALSE),
  .id = "model",
) %>%
  select(-chisq, -df) %>%
  pivot_wider(names_from = term, values_from = p) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame()
colnames(sch.df2) <- sch.df2[1, ]
sch.df2 <- sch.df2[-1, ]
sch.df2 <- sch.df2 %>% rownames_to_column("term")

# obsolete tdc ------------------------------------------------------------

# md2 <- tmerge(md, md, id=id,
#               dstat=event(Time, outcome),
#               FIMMOT = tdc(FIMMOTD),
#               FIMCOG = tdc(FIMCOGD)
#               ) %>% tibble()
# 
# survSplit(formula(model5), md, id="id", cut = c(0, .3)) %>% tibble()
# 
# md3 <- survSplit(Surv(Time, outcome)~., md, cut = c(0, 1)) %>% tibble()
# 
# # coxph()
# model6 <- update(model5, data=md3)
# model6.sch <- cox.zph(model6)
# 
# model6.sch %>% sch()
