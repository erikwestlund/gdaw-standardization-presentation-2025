# Simulation 2: Age + Sex → Late Presentation
#
# Purpose: Demonstrate Simpson's Paradox when P(age|sex) differs.
# Uses the SAME base_population as sim1.
#
# Causal structure:
#   - Sex → Age (women live longer, different age distributions by sex)
#   - Age → Late Presentation (same age-specific rates as sim1)
#   - Sex → Late Presentation (men higher risk within age strata, constant logit-scale effect)
#
# REQUIRES: base_population (from 00_base_population.R)

library(dplyr)
library(tidyr)

# Helper function
inv_logit <- function(z) 1/(1+exp(-z))

# Baseline logits for Women reference group
# Target: Young ~5%, Middle ~15%, Older ~40%
logit_base <- c(
  Young = qlogis(0.05),
  Middle = qlogis(0.15),
  Older = qlogis(0.40)
)

# Men effect: constant on logit scale (~+3pp at low baseline, smaller at high)
# This keeps gaps from blowing up at high baseline risk
logit_male <- 0.25

# Build linear predictor
lp <- logit_base[as.character(base_population$age_group)] +
      ifelse(base_population$sex == "Men", logit_male, 0)

# Convert to probabilities
rate <- inv_logit(lp)

# Simulate late presentation events (SAME individuals as sim1, different outcome)
late_presentation <- rbinom(nrow(base_population), 1, rate)

# Create sim2 dataset - now include Sex
sim2_data <- base_population |>
  select(id, sex, age_group) |>
  mutate(late_presentation = late_presentation)

# Print summary
cat("\n=== Simulation 2: Age + Sex → Late Presentation ===\n\n")
cat("Using SAME base_population as sim1 (n=", nrow(base_population), ")\n", sep = "")
cat("Now we examine both Age and Sex\n\n")

cat("Observed age distribution by sex (from base_population):\n")
print(prop.table(table(sim2_data$age_group, sim2_data$sex), margin = 2))

cat("\nTrue age-sex specific rates (target design):\n")
cat("  Young: Women ~5%, Men ~8%\n")
cat("  Middle: Women ~15%, Men ~20%\n")
cat("  Older: Women ~40%, Men ~48%\n")
cat("  (Men higher within every age group, constant logit-scale effect)\n\n")

crude_by_sex <- sim2_data |>
  group_by(sex) |>
  summarise(crude_rate = mean(late_presentation))

cat("Observed crude rates by sex:\n")
print(crude_by_sex)

age_sex_rates <- sim2_data |>
  group_by(sex, age_group) |>
  summarise(rate = mean(late_presentation), .groups = "drop")

cat("\nObserved age-sex specific rates:\n")
print(age_sex_rates)

cat("\n*** Simpson's Paradox! ***\n")
cat("Men have higher risk within EVERY age group\n")
cat("But women are concentrated in older ages (where everyone has higher risk)\n")
cat("So crude rates show women worse overall\n")
cat("\nDirect standardization needed to compare fairly.\n")

# Sanity checks
cat("\n=== Sanity Checks ===\n")

cat("\n1) Within-age rates (should be M > F in every stratum):\n")
within_age <- sim2_data |>
  group_by(age_group, sex) |>
  summarise(rate = mean(late_presentation), .groups = "drop")
print(within_age)

cat("\n2) Crude rates (ideally F > M to show paradox):\n")
crude_check <- sim2_data |>
  group_by(sex) |>
  summarise(crude = mean(late_presentation))
print(crude_check)

cat("\n3) Direct-standardized by age (should flip to M > F):\n")
std_w <- sim2_data |>
  count(age_group) |>
  mutate(w = n/sum(n))
std_rates <- sim2_data |>
  group_by(sex, age_group) |>
  summarise(rate = mean(late_presentation), .groups = "drop") |>
  left_join(std_w, by = "age_group") |>
  group_by(sex) |>
  summarise(std = sum(rate*w))
print(std_rates)
