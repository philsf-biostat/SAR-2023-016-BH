# setup -------------------------------------------------------------------
library(survminer)
library(ggsurvfit)
library(alluvial)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
ff.pal <- "Paired"    # good for binary groups scale fill/color brewer

gg <- analytical %>%
  drop_na(exposure) %>%
  ggplot() +
  scale_color_brewer(palette = ff.pal) +
  scale_fill_brewer(palette = ff.pal) +
  theme_ff()

crop <- 0.5

plot_data <- analytical %>%
  select(Sex=SexF, SES=exposure, Death=outcome) %>%
  table() %>%
  as.data.frame()

# plots -------------------------------------------------------------------

# gg.outcome <- gg +
#   geom_bar(
#     data = drop_na(analytical, exposure),
#     aes(exposure,
#         fill = factor(outcome, labels = c("Survived", "Death"))),
#     position = "fill") +
#   scale_y_continuous(labels = scales::label_percent()) +
#   xlab(attr(analytical$exposure, "label")) +
#   ylab("Mortality") +
#   labs(fill = "")

# # this alluvial plot will be saved built in plots-save
# png("figures/outcome.png")
# alluvial(plot_data[, 1:3], freq=plot_data$Freq,
#          col = ifelse(plot_data$Death == 1, "red", "grey"),
#          border = ifelse(plot_data$Death == 1, "red", "grey"),
#          #hide = plot_data$Freq == 0,
#          cex = 0.7
#          )
# dev.off()

gg.age <- gg +
  geom_density(
    data = drop_na(analytical, SexF),
    aes(AGE, fill = SexF),
    alpha = .9) +
  xlab(attr(analytical$AGE, "label")) +
  ylab("Distribution density") +
  labs(fill = "")

gg.ses <- gg +
  geom_bar(
    data = drop_na(analytical, SexF, exposure),
    aes(exposure,
        fill = SexF),
    position = "fill") +
  scale_y_continuous(labels = scales::label_percent()) +
  xlab(attr(analytical$exposure, "label")) +
  ylab("") +
  labs(fill = "")

# survival curves ---------------------------------------------------------

# cxsf <- survfit(model6, newdata = newdat)
cxsf <- survfit(model5, newdata = newdat)
surv_cxsf <- surv_summary(cxsf, data = analytical) %>% tibble()
m_newdat <- newdat[as.character(surv_cxsf$strata), ]

## plotting data frame
surv_df <- cbind(surv_cxsf, m_newdat)

gg.surv <- surv_df %>%
  ggsurvplot_df(
    # config básica do plot
    surv.geom = geom_line,
    color = "exposure",
    # linetype = "exposure",
    linetype = "SexF",
    lwd = .2,
    # config extras (optional)
    # surv.median.line = "hv",
    # risk.table = TRUE,
    censor = FALSE,
    conf.int = FALSE,
    # crop. uncrop in separate figure
    ylim = c(crop, 1),
    # labels
    title = "Effect of SES on survival",
    xlab = "Time (years)",
    # ylab = "Survival",
    surv.scale = "percent",
    # theme
    palette = "OrRd",
    ggtheme = theme_ff(),
  ) +
  theme(
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    # when SexF is added to the plot, we need to change legend position
    # legend.position = "right",
    legend.position = c(.20, .35),
  )

gg.surv.uncrop <- gg.surv +
  ylim(c(0, 1)) +
  scale_y_continuous(labels = scales::label_percent())

# # Schoenfeld residuals
# ggcoxzph(cox.zph(mod.full), ggtheme = theme_ff(), font.main = 10)

# gg.cause <- update(mod.full, . ~ .
#                    -Cause
#                    + strata(Cause)
#                    # -FIMMOTD -FIMCOGD + FIMMOTD4 + FIMCOGD4
#                    ) %>%
#   survfit() %>%
#   ggsurvfit() +
#   theme_ff()
# gg.cause
