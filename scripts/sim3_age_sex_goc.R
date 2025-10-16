# Simulation 3: Age + Sex + GOC → Late Presentation
#
# Purpose: Show limits of manual standardization with multiple confounders.
# Uses the SAME base_population as sim1 and sim2.
#
# GOC = Gender Opportunity Context (gendered norms/power/access)
#
# Causal structure:
#   - GOC → Age (low GOC = younger; constraints/shorter healthy lifespan)
#   - GOC → Late Presentation (low GOC = higher risk due to norms/access barriers)
#   - Sex → Age (women live longer)
#   - Age → Late Presentation (same age effect as sim1/sim2)
#   - Sex → Late Presentation (same sex effect as sim2: constant logit-scale effect)
#
# REQUIRES: base_population (from 00_base_population.R)

# Helper function
inv_logit <- function(z) 1/(1+exp(-z))

# Baseline logits for Woman, High GOC reference group
# Target: Young ~5%, Middle ~15%, Older ~40%
logit_base <- c(
  Young = qlogis(0.05),
  Middle = qlogis(0.15),
  Older = qlogis(0.40)
)

# Effects: constant on logit scale
# Man: ~+3pp at low baseline
# GOC Mid: ~+2pp at low baseline
# GOC Low: ~+4pp at low baseline
logit_male <- 0.25
logit_goc_mid <- 0.15
logit_goc_low <- 0.35

# Build linear predictor per person
lp <- logit_base[as.character(base_population$age_group)] +
      ifelse(base_population$sex == "Man", logit_male, 0) +
      ifelse(base_population$goc == "Mid",  logit_goc_mid, 0) +
      ifelse(base_population$goc == "Low",  logit_goc_low, 0)

# Convert to probabilities (nicely bounded)
rate <- inv_logit(lp)

# Simulate late presentation events (SAME individuals as sim1/sim2, different outcome)
late_presentation <- rbinom(nrow(base_population), 1, rate)

# Create sim3 dataset - now include Sex, Age, AND GOC
sim3_data <- base_population %>%
  mutate(late_presentation = late_presentation)

# Print summary
cat("\n=== Simulation 3: Age + Sex + GOC → Late Presentation ===\n\n")
cat("Using SAME base_population as sim1 and sim2 (n=", nrow(base_population), ")\n", sep = "")
cat("Now we examine Age, Sex, AND Gender Opportunity Context (GOC)\n\n")

cat("Age distribution by Sex (from base_population):\n")
print(prop.table(table(sim3_data$age_group, sim3_data$sex), margin = 2))

cat("\nAge distribution by GOC (from base_population):\n")
print(prop.table(table(sim3_data$age_group, sim3_data$goc), margin = 2))

crude_overall <- mean(sim3_data$late_presentation)
cat("\nOverall crude rate:", round(crude_overall, 4), "\n")

crude_by_sex <- sim3_data %>%
  group_by(sex) %>%
  summarise(crude_rate = mean(late_presentation))
cat("\nCrude rates by sex:\n")
print(crude_by_sex)

crude_by_goc <- sim3_data %>%
  group_by(goc) %>%
  summarise(crude_rate = mean(late_presentation))
cat("\nCrude rates by GOC:\n")
print(crude_by_goc)

cat("\n*** Multiple confounding pathways! ***\n")
cat("True effects (by design):\n")
cat("  - Age: Young ~5%, Middle ~15%, Older ~40%\n")
cat("  - Sex: Men higher (constant logit-scale effect)\n")
cat("  - GOC: Low/Mid higher than High (constant logit-scale effects)\n")
cat("    (lower gender equality context = more barriers)\n")
cat("\nBut crude comparisons are biased because:\n")
cat("  - GOC → Age (low GOC = younger)\n")
cat("  - Sex → Age (women = older)\n")
cat("  - All three affect outcome\n")
cat("\nManual stratification becomes unmanageable.\n")
cat("Need regression (Poisson/Cox) to handle multiple confounders.\n")

# Sanity checks
cat("\n=== Sanity Checks ===\n")

cat("\n1) Within-age comparison (Man vs Woman):\n")
age_sex_check <- sim3_data %>%
  group_by(age_group, sex) %>%
  summarise(rate = mean(late_presentation), .groups = "drop") %>%
  pivot_wider(names_from = sex, values_from = rate) %>%
  mutate(diff = Man - Woman)
print(age_sex_check)
cat("(All diffs should be positive: Men > Women within each age)\n")

cat("\n2) Crude rates by GOC (may be confounded):\n")
goc_crude <- sim3_data %>%
  group_by(goc) %>%
  summarise(crude = mean(late_presentation))
print(goc_crude)
