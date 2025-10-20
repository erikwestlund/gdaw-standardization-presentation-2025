# Render DAG Images
#
# Purpose: Pre-render all DAG visualizations as static PNG images
# to avoid ggdag rendering bugs in knitr/Quarto contexts
#
# Output: resources/images/dag1.png, dag2.png, dag3.png

library(ggdag)
library(ggplot2)

# Ensure output directory exists
dir.create("resources/images", recursive = TRUE, showWarnings = FALSE)

# DAG 1: Age → Outcome (Simple case, no confounding)
dag1 <- dagify(
  Out ~ Age,
  coords = list(
    x = c(Age = 1, Out = 2),
    y = c(Age = 1, Out = 1)
  )
)

p1 <- ggdag(dag1, text_size = 5, node_size = 20) +
  theme_dag_blank() +
  labs(title = "Causal Structure: Age Determines Outcome") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

ggsave(
  "resources/images/dag1.png",
  p1,
  width = 8,
  height = 4,
  dpi = 300,
  bg = "white"
)

cat("✓ Saved dag1.png\n")

# DAG 2: Age + Gender → Outcome (Simpson's Paradox)
dag2 <- dagify(
  Out ~ Age + Gender,
  Age ~ Gender,
  coords = list(
    x = c(Gender = 1, Age = 2, Out = 3),
    y = c(Gender = 2, Age = 1, Out = 2)
  )
)

p2 <- ggdag(dag2, text_size = 5, node_size = 20) +
  theme_dag_blank() +
  labs(title = "Causal Structure: Gender → Age → Outcome, Gender → Outcome") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

ggsave(
  "resources/images/dag2.png",
  p2,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)

cat("✓ Saved dag2.png\n")

# DAG 3: Age + Gender + GOC → Outcome (Multiple confounders)
dag3 <- dagify(
  Out ~ Age + Gender + GOC,
  Age ~ Gender + GOC,
  coords = list(
    x = c(GOC = 1, Gender = 1, Age = 2, Out = 3),
    y = c(GOC = 1, Gender = 3, Age = 2, Out = 2)
  )
)

p3 <- ggdag(dag3, text_size = 5, node_size = 20) +
  theme_dag_blank() +
  labs(title = "Gender Opportunity Context introduces multiple pathways") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

ggsave(
  "resources/images/dag3.png",
  p3,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("✓ Saved dag3.png\n")

cat("\nAll DAG images rendered successfully!\n")
