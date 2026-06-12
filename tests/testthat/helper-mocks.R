mock_scatter_plan <- function() {
  list(
    plot_type = "scatter",
    package = "ggplot2",
    mappings = list(x = "speed", y = "distance", color = "group"),
    transformations = list(),
    facets = NULL,
    labels = list(
      title = "Distance by speed",
      subtitle = "Grouped by treatment",
      x = "Speed",
      y = "Distance",
      color = "Group"
    ),
    theme = list(base_size = 12),
    accessibility = list(
      palette = "okabe_ito",
      colorblind_safe = TRUE,
      alt_text = "Scatter plot of distance by speed grouped by treatment."
    ),
    output = list(width = 7, height = 5, dpi = 300, filename = "distance-by-speed.png"),
    design_rationale = "Scatter plots show the relationship between two numeric variables.",
    clarification_questions = character(),
    confidence = 0.95
  )
}
