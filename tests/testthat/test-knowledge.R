test_that("visualization guide and default rules load", {
  guide <- ap_load_viz_guide(compact = TRUE)
  expect_type(guide, "character")
  expect_length(guide, 1)
  expect_match(guide, "AutoPlotR Visualization Design Guide")
  expect_match(guide, "Non-negotiable principles")

  rules <- ap_load_viz_rules(refresh = TRUE)
  expect_type(rules, "list")
  expect_named(rules, c("version", "core", "defaults", "plot_types"), ignore.order = TRUE)
  expect_true("scatter" %in% names(rules$plot_types))
})

test_that("plot-type rule extraction keeps core defaults and selected plot rules", {
  rules <- ap_load_viz_rules("scatter", refresh = TRUE)

  expect_equal(rules$plot_type, "scatter")
  expect_equal(rules$version, ap_load_viz_rules(refresh = TRUE)$version)
  expect_true("core" %in% names(rules))
  expect_true("defaults" %in% names(rules))
  expect_true("rules" %in% names(rules))
  expect_match(paste(unlist(rules$rules), collapse = " "), "overplotting")
})

test_that("project and user visualization rule overrides are merged in order", {
  td <- tempfile("autoplotr-rules-")
  dir.create(td)
  withr::local_dir(td)
  withr::local_options(list(AutoPlotR.config_dir = file.path(td, "user-config")))
  dir.create(getOption("AutoPlotR.config_dir"), recursive = TRUE)

  yaml::write_yaml(
    list(
      version = "user-test",
      defaults = list(width = 8),
      plot_types = list(scatter = list(overplotting = "Use user alpha."))
    ),
    ap_viz_rules_path("user")
  )
  yaml::write_yaml(
    list(
      version = "project-test",
      defaults = list(height = 4),
      plot_types = list(scatter = list(overplotting = "Use project alpha."))
    ),
    "autoplotr-viz-rules.yml"
  )

  rules <- ap_load_viz_rules("scatter", refresh = TRUE)

  expect_equal(rules$version, "project-test")
  expect_equal(rules$defaults$width, 8)
  expect_equal(rules$defaults$height, 4)
  expect_equal(rules$rules$overplotting, "Use project alpha.")
})
