test_that("planner prompt contains compact visualization design rules", {
  data <- data.frame(speed = 1:3, distance = c(2, 4, 8), group = c("a", "b", "a"))
  profile <- ap_profile_data(data, max_sample_rows = 2)
  prompt <- ap_build_planner_prompt(
    data_profile = profile,
    request = "plot distance by speed",
    rules = ap_load_viz_rules("scatter", refresh = TRUE)
  )

  expect_match(prompt, "AutoPlotR Visualization Design Guide")
  expect_match(prompt, "Use readable labels")
  expect_match(prompt, "Return only structured data")
  expect_match(prompt, "Do not invent columns")
  expect_match(prompt, "State assumptions")
})

test_that("plotter prompt contains execution and safety standards", {
  plan <- c(
    mock_scatter_plan(),
    list(design_guide_version = ap_load_viz_rules(refresh = TRUE)$version)
  )
  rules <- ap_load_viz_rules("scatter", refresh = TRUE)
  prompt <- ap_build_plotter_prompt(
    plan = plan,
    data_path = "data.rds",
    plot_path = "plot.png",
    rules = rules
  )

  expect_match(prompt, "Validate required columns")
  expect_match(prompt, "Create a ggplot object named p")
  expect_match(prompt, "ggplot2::ggsave")
  expect_match(prompt, "Do not install packages")
})

test_that("ap_plan returns a validated design-aware visualization plan", {
  data <- data.frame(speed = 1:3, distance = c(2, 4, 8), group = c("a", "b", "a"))
  withr::local_options(list(AutoPlotR.mock_planner_response = mock_scatter_plan()))

  plan <- ap_plan(data, "plot distance by speed colored by group")

  expect_s3_class(plan, "ap_viz_plan")
  expect_equal(plan$plot_type, "scatter")
  expect_equal(plan$mappings$x, "speed")
  expect_equal(plan$mappings$y, "distance")
  expect_equal(plan$design_guide_version, ap_load_viz_rules(refresh = TRUE)$version)
  expect_true(plan$accessibility$colorblind_safe)
})

test_that("ap_plan rejects unknown mapped variables", {
  data <- data.frame(speed = 1:3, distance = c(2, 4, 8))
  bad_plan <- mock_scatter_plan()
  bad_plan$mappings$x <- "missing_column"
  withr::local_options(list(AutoPlotR.mock_planner_response = bad_plan))

  expect_error(
    ap_plan(data, "plot distance by speed"),
    "Unknown variable"
  )
})

test_that("normalize_plan handles every top-level schema field", {
  # If this test fails, a field was added to ap_plan_schema() in R/ellmer.R
  # but not handled in ap_normalize_plan() in R/planner.R.
  # Add the missing field to the ALLOWED_UNHANDLED list, or add a default line.

  schema_fields <- c(
    "plot_type", "package", "mappings", "transformations",
    "facets", "labels", "theme", "accessibility", "output",
    "design_rationale", "clarification_questions", "confidence"
  )

  # Fields intentionally left without a %||% default in normalize_plan
  # because they are optional and NULL is a valid value.
  ALLOWED_UNHANDLED <- c("design_rationale")

  profile <- ap_profile_data(mtcars)
  rules <- ap_load_viz_rules("scatter", refresh = TRUE)

  minimal_plan <- list(plot_type = "scatter")
  plan <- ap_normalize_plan(minimal_plan, profile, "test", rules)

  for (field in schema_fields) {
    expect_true(
      field %in% names(plan) || field %in% ALLOWED_UNHANDLED,
      info = paste("Schema field", field, "not in normalized plan. Add to normalize_plan or ALLOWED_UNHANDLED.")
    )
  }
})
