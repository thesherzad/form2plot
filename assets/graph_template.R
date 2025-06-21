# ============================================================================
# PROGRAM:     graph_template.R
# PURPOSE:     Create <Graph Title> Graph
# AUTHOR:      <Your Name>
# DATE:        <call get_current_time>
# INPUT:       ADaM dataset (e.g., ADLB, ADAE)
# OUTPUT:      <output_name>.<plot_format>
# R VERSION:   <call get_r_version>
# ============================================================================


# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(scales)

# Optional: for KM plots or other types
# library(survival)
# library(survminer)

# Custom ggplot theme
company_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, hjust = 0.5),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 2),
      legend.position = "bottom",
      legend.title = element_text(size = base_size - 1),
      legend.text = element_text(size = base_size - 2),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
}

# Parameters (to be customized) -------------------------------------------
dataset_path <- "<dataset path>"     # File path or pre-loaded object
output_dir   <- "outputs/graphs/"
plot_id      <- "<Plot ID>"
plot_title   <- "<Plot title>"
output_name  <- 'paste0(<Type of Plot>, "-" <Plot ID>, "-grpby-", <Grouping Variable>)'
plot_format  <- "png"               # or "pdf"
filter_expr  <- "expr(<Filter value (if any)>)"


# Read Dataset ------------------------------------------------------------
# Assume data is either loaded or read via haven/readr/etc.
adlb <- readr::read_csv(dataset_path)


# Data Preparation --------------------------------------------------------
plot_data <- adlb %>%
  filter(!!filter_expr) %>%
  group_by(AVISIT, TRT01P) %>%
  summarise(
    mean_chg = mean(CHG, na.rm = TRUE),
    sd_chg = sd(CHG, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_chg / sqrt(n),
    ci_lower = mean_chg - 1.96 * se,
    ci_upper = mean_chg + 1.96 * se
  )


# Plot Construction -------------------------------------------------------
plot <- ggplot(plot_data, aes(x = "<x-axis variable>", y = "<y-axis variable>", group = "<group variable>", color = "<fille appropriately>")) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(
    title = "<graph title>",
    x = "<x-axis variable>",
    y = "<y-axis variable>",
    color = "<fille appropriately>"
  ) +
  company_theme()

plot

# Save Plot ---------------------------------------------------------------
ggsave(
  filename = file.path(output_dir, paste0(output_name, ".", plot_format)),
  plot = plot,
  width = 8,
  height = 6
)


# Logging Output ----------------------------------------------------------
message("Plot ", plot_id, " saved as ", output_name, ".", plot_format)


# Session Info ------------------------------------------------------------

sessionInfo()