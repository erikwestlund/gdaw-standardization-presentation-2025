# Base Population Structure
#
# Purpose: Create ONE population with Age, Sex, and GOC that will be used
# consistently across ALL three examples. Each example will add different
# outcomes to this same population.
#
# GOC = Gender Opportunity Context: represents norms/power/access context
# (Low/Mid/High gender equality context)
#
# This ensures:
# - Same individuals across all examples
# - Same marginal P(Age) across all examples
# - Same P(Age | Sex) and P(Age | Sex, GOC) distributions
# - Population structure visualizations show the SAME data

library(dplyr)

set.seed(1234)

n <- 20000

# Target marginal age distribution (SAME across all datasets)
age_levels <- c("Young", "Middle", "Older")
p_age_target <- c(0.30, 0.35, 0.35)

# First: Create exact age distribution to match target
n_young <- round(n * p_age_target[1])
n_middle <- round(n * p_age_target[2])
n_older <- n - n_young - n_middle  # Ensure exact n

age_group <- rep(age_levels, c(n_young, n_middle, n_older))

# Shuffle age_group to randomize
age_group <- sample(age_group)

# Sex (balanced)
sex <- sample(c("Men", "Women"), n, replace = TRUE)

# Gender Opportunity Context conditional on age
# This creates confounding: Age → GOC, GOC will appear related to outcomes
goc <- character(n)
for (i in 1:n) {
  if (age_group[i] == "Young") {
    # Younger people more likely in Low GOC contexts
    goc[i] <- sample(c("Low", "Mid", "High"), 1, prob = c(0.55, 0.30, 0.15))
  } else if (age_group[i] == "Middle") {
    # Middle age balanced
    goc[i] <- sample(c("Low", "Mid", "High"), 1, prob = c(0.45, 0.35, 0.20))
  } else {
    # Older people more likely in High GOC contexts
    goc[i] <- sample(c("Low", "Mid", "High"), 1, prob = c(0.35, 0.40, 0.25))
  }
}

# Adjust sex distribution within age to create Sex → Age confounding
# Women live longer, so adjust to have more women in older ages
temp_df <- data.frame(age_group, sex, goc)
temp_df <- temp_df %>%
  group_by(age_group) %>%
  mutate(
    sex = if (age_group[1] == "Older") {
      # Much more women in older age (60% women)
      ifelse(row_number() <= n() * 0.60, "Women", "Men")
    } else if (age_group[1] == "Young") {
      # Much more men in younger age (60% men, 40% women)
      ifelse(row_number() <= n() * 0.40, "Women", "Men")
    } else {
      sex  # Keep middle balanced
    }
  ) %>%
  ungroup()

age_group <- temp_df$age_group
sex <- temp_df$sex
goc <- temp_df$goc

# Create base population dataframe
base_population <- data.frame(
  id = 1:n,
  sex = factor(sex, levels = c("Men", "Women")),
  age_group = factor(age_group, levels = age_levels),
  goc = factor(goc, levels = c("Low", "Mid", "High"))
)

# Print summary
cat("\n=== BASE POPULATION STRUCTURE ===\n\n")
cat("Sample size:", nrow(base_population), "\n")
cat("This population will be used in ALL three examples.\n\n")

# Check marginal P(Age)
observed_p_age <- prop.table(table(base_population$age_group))[age_levels]
cat("Target marginal P(Age):", p_age_target, "\n")
cat("Observed marginal P(Age):", round(observed_p_age, 3), "\n\n")

cat("Age distribution by Sex:\n")
print(prop.table(table(base_population$age_group, base_population$sex), margin = 2))

cat("\nAge distribution by GOC (Gender Opportunity Context):\n")
print(prop.table(table(base_population$age_group, base_population$goc), margin = 2))

cat("\nGOC distribution:\n")
print(prop.table(table(base_population$goc)))

cat("\n*** This base_population object will be reused in sim1, sim2, sim3 ***\n")
