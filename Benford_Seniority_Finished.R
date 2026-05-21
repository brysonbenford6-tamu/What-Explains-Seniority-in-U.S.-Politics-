###############################################################################
# Title:    What Explains Seniority in the U.S. House of Representatives?
# Author:   Bryson Benford
# Purpose:  Clean, portfolio-ready replication script for the 116th Congress
# Updated:  March 2026
###############################################################################

# ── Package Setup ──────────────────────────────────────────────────────────────

required_packages <- c(
  "readr", "dplyr", "ggplot2", "forcats",
  "broom", "sandwich", "lmtest", "scales"
)
install.packages("forcats")
install.packages("broom")
install.packages("sandwich")
install.packages("lmtest")
install.packages("scales")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    paste(
      "Please install the following packages before running this script:",
      paste(missing_packages, collapse = ", ")
    )
  )
}

library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(broom)
library(sandwich)
library(lmtest)
library(scales)

# ── File Paths ────────────────────────────────────────────────────────────────
# This script looks for the data file in the current working directory first.
# If you keep the CSV in the same folder as this script, the default path below
# should work without any interactive setup.

data_path <- "congress116data.csv"
output_dir <- "output"

if (!file.exists(data_path)) {
  stop("Data file 'congress116data.csv' was not found in the working directory.")
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ── Load and Prepare Data ─────────────────────────────────────────────────────

congress116 <- read_csv("Desktop/congress116data.csv")
View(congress116)
# Full descriptive dataset
descriptive_data <- congress116 %>%
  filter(!is.na(seniority), !is.na(party))

# Major-party analytic sample for regression
analysis_data <- congress116 %>%
  filter(
    party %in% c("Republican", "Democrat"),
    !is.na(seniority),
    !is.na(inc_pres_pct_2p),
    !is.na(meddist)
  ) %>%
  mutate(
    party = factor(party, levels = c("Republican", "Democrat"))
  )

# ── Descriptive Statistics ────────────────────────────────────────────────────

party_summary <- descriptive_data %>%
  group_by(party) %>%
  summarise(
    n = n(),
    mean_seniority = mean(seniority, na.rm = TRUE),
    sd_seniority = sd(seniority, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_seniority))

major_party_summary <- analysis_data %>%
  group_by(party) %>%
  summarise(
    n = n(),
    mean_seniority = mean(seniority),
    mean_district_safety = mean(inc_pres_pct_2p),
    mean_meddist = mean(meddist),
    .groups = "drop"
  )

write_csv(party_summary, file.path(output_dir, "table_descriptive_by_party.csv"))
write_csv(major_party_summary, file.path(output_dir, "table_major_party_summary.csv"))

# ── Figure 1: Mean Seniority by Party ─────────────────────────────────────────

figure1 <- ggplot(party_summary, aes(x = fct_reorder(party, mean_seniority), y = mean_seniority)) +
  geom_col(width = 0.65, fill = "gray40") +
  geom_text(aes(label = paste0("n = ", n)), vjust = -0.4, size = 3.5) +
  labs(
    title = "Mean Seniority by Party",
    subtitle = "116th U.S. House of Representatives",
    x = "Party",
    y = "Average terms served"
  ) +
  expand_limits(y = max(party_summary$mean_seniority) + 1) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


ggsave(
  filename = file.path(output_dir, "figure1_mean_seniority_by_party.png"),
  plot = figure1,
  width = 7.2,
  height = 4.3,
  dpi = 300
)

# ── Figure 2: Distribution of Seniority ───────────────────────────────────────

figure2 <- ggplot(descriptive_data, aes(x = seniority)) +
  geom_histogram(bins = 20, fill = "gray45", color = "white") +
  labs(
    title = "Distribution of House Seniority",
    subtitle = "116th U.S. House of Representatives",
    x = "Terms served",
    y = "Number of representatives"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(
  filename = file.path(output_dir, "figure2_seniority_distribution.png"),
  plot = figure2,
  width = 7.2,
  height = 4.0,
  dpi = 300
)

# ── Regression Models ─────────────────────────────────────────────────────────
# Model A: Bivariate party difference
# Model B: Party + district safety + ideological distance

model_a <- lm(seniority ~ party, data = analysis_data)
model_b <- lm(seniority ~ party + inc_pres_pct_2p + meddist, data = analysis_data)

# HC3 robust standard errors
robust_a <- coeftest(model_a, vcov. = vcovHC(model_a, type = "HC3"))
robust_b <- coeftest(model_b, vcov. = vcovHC(model_b, type = "HC3"))

# Tidy output
model_a_tidy <- tidy(model_a, conf.int = TRUE) %>%
  mutate(
    robust_se = sqrt(diag(vcovHC(model_a, type = "HC3"))),
    model = "Bivariate"
  )

model_b_tidy <- tidy(model_b, conf.int = TRUE) %>%
  mutate(
    robust_se = sqrt(diag(vcovHC(model_b, type = "HC3"))),
    model = "Controlled"
  )

regression_table <- bind_rows(model_a_tidy, model_b_tidy) %>%
  mutate(
    term = recode(
      term,
      "(Intercept)" = "Intercept",
      "partyDemocrat" = "Democrat (ref. Republican)",
      "inc_pres_pct_2p" = "District safety (inc_pres_pct_2p)",
      "meddist" = "Ideological distance (meddist)"
    )
  ) %>%
  select(model, term, estimate, robust_se, statistic, p.value, conf.low, conf.high)

write_csv(regression_table, file.path(output_dir, "table_regression_results.csv"))

# ── Coefficient Plot for Controlled Model ─────────────────────────────────────

coef_plot_data <- model_b_tidy %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      "partyDemocrat" = "Democrat\n(vs. Republican)",
      "inc_pres_pct_2p" = "District safety\n(inc_pres_pct_2p)",
      "meddist" = "Ideological distance\n(meddist)"
    )
  )

figure3 <- ggplot(coef_plot_data, aes(x = estimate, y = fct_rev(term))) +
  geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed") +
  geom_point(size = 2.4, color = "gray20") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, color = "gray35") +
  labs(
    title = "Regression Estimates Predicting House Seniority",
    subtitle = "Controlled model with 95% confidence intervals",
    x = "Coefficient estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(output_dir, "figure3_coefficient_plot.png"),
  plot = figure3,
  width = 7.2,
  height = 4.3,
  dpi = 300
)
print(figure3)
print(figure2)
print(figure1)
# ── Console Output ────────────────────────────────────────────────────────────

cat("\nDescriptive summary by party:\n")
print(party_summary)

cat("\nControlled model with HC3 robust standard errors:\n")
print(robust_b)

cat("\nFiles written to:", normalizePath(output_dir), "\n")
