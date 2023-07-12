library(readxl)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(survminer)
library(Rcpp)


# Import Data and Create DFs ----------------------------------------------
SGA_df <- read_excel(
  "Schwab Database SGA.xlsx",
  col_types = c(
    "numeric",
    "date",
    "numeric",
    "text",
    "text",
    "numeric",
    "numeric",
    "date",
    "text",
    "numeric",
    "text",
    "numeric",
    "date",
    "numeric",
    "numeric"
  )
)

SGA_df <-
  rename(SGA_df,
         event = `unplanned secondary surgery (0-12 months postop)`,
         time = `follow up (months)`,
         group = `following algorithm`)
SGA_df$group <-
  factor(
    SGA_df$group,
    levels = c(0, 1),
    labels = c("Not following SGA", "Following SGA")
  )

# Let's create a second dataframe where all outcomes are capped at 12 months
SGA_12mo_df <- SGA_df
SGA_12mo_df$event[SGA_12mo_df$event == 1 & SGA_12mo_df$time > 12] <-
  0
SGA_12mo_df$time[SGA_12mo_df$event == 0 & SGA_12mo_df$time > 12] <-
  12

# Generate KM Survival Curves ---------------------------------------------

# Overall KM Survival Curve
fit_SGA <- survfit2(Surv(time, event) ~ group, data = SGA_df)
fit_SGA %>% ggsurvfit() +
  labs(title = "Overall Kaplan-Meier Survival Curve for St. Gallen Algorithm",
       y = "Percentage Survival",
       x = "Follow-up time, months") +
  add_censor_mark() +
  add_confidence_interval() +
  theme(legend.position = "bottom") +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent,
    expand = c(0.01, 0)
  )

# 12-month KM Survival Curve
fit_SGA_12mo <-
  survfit2(Surv(time, event) ~ group, data = SGA_12mo_df)
fit_SGA_12mo %>% ggsurvfit() +
  labs(title = "12 Month Kaplan-Meier Survival Curve for St. Gallen Algorithm",
       y = "Percentage Survival",
       x = "Follow-up time, months") +
  add_censor_mark() +
  add_confidence_interval() +
  theme(legend.position = "bottom") +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent,
    expand = c(0.01, 0)
  ) +
  scale_x_continuous(limits = c(0, 12),
                     breaks = c(3, 6, 9))

# Logrank test ------------------------------------------------------------

surv_diff <- survdiff(Surv(time, event) ~ group, data = SGA_df)
surv_diff
surv_diff_12mo <-
  survdiff(Surv(time, event) ~ group, data = SGA_12mo_df)
surv_diff_12mo

ggsurvplot(
  fit_SGA,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Change risk table color by groups
  linetype = "strata",
  # Change line type by groups
  surv.median.line = "hv",
  # Specify median survival
  ggtheme = theme_bw(),
  # Change ggplot2 theme
  palette = c("#E7B800", "#2E9FDF")
)

ggsurvplot(
  fit_SGA_12mo,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  # Add risk table
  risk.table.col = "strata",
  # Change risk table color by groups
  linetype = "strata",
  # Change line type by groups
  surv.median.line = "hv",
  # Specify median survival
  ggtheme = theme_bw(),
  # Change ggplot2 theme
  palette = c("#E7B800", "#2E9FDF")
)
