# Generate and Save Simulation Data
#
# This script generates all simulation datasets and saves them using Framework's
# data management system. Run this once to generate the data, then the
# presentation loads it from the data catalog.

library(framework)
scaffold()

# Generate base population
source("scripts/00_base_population.R")
save_data(base_population, "base_population")

# Generate simulation 1: Age only
source("scripts/sim1_age_only.R")
save_data(sim1_data, "sim1_data")

# Generate simulation 2: Age + Gender
source("scripts/sim2_age_gender.R")
save_data(sim2_data, "sim2_data")

# Generate simulation 3: Age + Gender + GOC
source("scripts/sim3_age_gender_goc.R")
save_data(sim3_data, "sim3_data")

cat("\n✓ All simulation datasets generated and saved!\n")
cat("  - base_population\n")
cat("  - sim1_data\n")
cat("  - sim2_data\n")
cat("  - sim3_data\n\n")
