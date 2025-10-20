# Simulation 1: Age → Late Presentation
#
# Purpose: Demonstrate how crude rates depend on population age structure.
# This is the simplest case - no confounding, just composition.
#
# Causal structure: Age → Late Presentation (single cause)
#
# REQUIRES: base_population (from 00_base_population.R)

# Helper function
inv_logit <- function(z) 1/(1+exp(-z))

# True age-specific rates on logit scale (the "truth" - SAME across all datasets)
# Target: Young ~5%, Middle ~15%, Older ~40%
logit_base <- c(
  Young = qlogis(0.05),
  Middle = qlogis(0.15),
  Older = qlogis(0.40)
)

# Apply to population
lp <- logit_base[as.character(base_population$age_group)]
rate <- inv_logit(lp)

# Simulate late presentation events
late_presentation <- rbinom(nrow(base_population), 1, rate)

# Create sim1 dataset - just Age and outcome
# (ignoring Sex and GOC for this example)
sim1_data <- base_population |>
  select(id, age_group) |>
  mutate(late_presentation = late_presentation)

# Print summary
cat("\n=== Simulation 1: Age → Late Presentation ===\n\n")
cat("Using base_population (n=", nrow(base_population), ")\n", sep = "")
cat("For this example, we only look at Age (ignoring Sex/GOC)\n\n")

cat("True age-specific rates (known):\n")
true_rates <- data.frame(
  age_group = c("Young", "Middle", "Older"),
  true_rate = c(0.05, 0.15, 0.40)
)
print(true_rates)

cat("\nObserved crude rate:", round(mean(sim1_data$late_presentation), 4), "\n")
cat("Expected from true rates:", round(sum(c(0.30, 0.35, 0.35) * c(0.05, 0.15, 0.40)), 4), "\n")
cat("\nKey point: Crude rate = weighted average of age-specific rates,\n")
cat("weighted by population age structure.\n")
