test_that("ap_plot writes code, plan, profile, and rendered plot artifacts via agent", {
  data <- data.frame(speed = 1:5, distance = c(2, 4, 8, 16, 32), group = c("a", "b", "a", "b", "a"))
  plan <- structure(
    c(mock_scatter_plan(), list(design_guide_version = ap_load_viz_rules(refresh = TRUE)$version)),
    class = c("ap_viz_plan", "list")
  )
  td <- tempfile("autoplotr-output-")

  mock_code <- paste(
    "p <- ggplot2::ggplot(data, ggplot2::aes(x = speed, y = distance, color = group)) +",
    "  ggplot2::geom_point() + ggplot2::theme_minimal()",
    "p",
    sep = "\n"
  )
  withr::local_options(list(AutoPlotR.mock_plotter_response = list(code = mock_code)))

  result <- ap_plot(
    data = data,
    plan = plan,
    output_dir = td,
    filename = "distance-by-speed",
    save_data = FALSE
  )

  expect_s3_class(result, "ap_plot_result")
  expect_true(file.exists(result$script_path))
  expect_true(file.exists(result$plot_path))
  expect_true(file.exists(file.path(td, "plan.json")))
  expect_true(file.exists(file.path(td, "profile.json")))

  code <- paste(readLines(result$script_path, warn = FALSE), collapse = "\n")
  expect_match(code, "ggplot2::ggplot")
  expect_match(code, "ggplot2::geom_point")

  saved_plan <- jsonlite::read_json(file.path(td, "plan.json"), simplifyVector = FALSE)
  expect_equal(saved_plan$design_guide_version, ap_load_viz_rules(refresh = TRUE)$version)
})

test_that("ap_plot can render without persisting input data", {
  data <- data.frame(speed = 1:5, distance = c(2, 4, 8, 16, 32), group = c("a", "b", "a", "b", "a"))
  plan <- structure(
    c(mock_scatter_plan(), list(design_guide_version = ap_load_viz_rules(refresh = TRUE)$version)),
    class = c("ap_viz_plan", "list")
  )
  td <- tempfile("autoplotr-output-")

  mock_code <- paste(
    "p <- ggplot2::ggplot(data, ggplot2::aes(x = speed, y = distance, color = group)) +",
    "  ggplot2::geom_point() + ggplot2::theme_minimal()",
    "p",
    sep = "\n"
  )
  withr::local_options(list(AutoPlotR.mock_plotter_response = list(code = mock_code)))

  result <- ap_plot(
    data = data,
    plan = plan,
    output_dir = td,
    save_data = FALSE
  )

  expect_true(file.exists(result$plot_path))
  expect_null(result$data_path)
  expect_false(file.exists(file.path(td, "data.rds")))
})

test_that("ap_parse_transformation handles valid and invalid inputs", {
  parsed <- ap_parse_transformation("new_col = old_col * 2")
  expect_equal(parsed$name, "new_col")
  expect_equal(parsed$expr, "old_col * 2")

  parsed2 <- ap_parse_transformation("sig <- ifelse(padj < 0.05, 'yes', 'no')")
  expect_equal(parsed2$name, "sig")
  expect_match(parsed2$expr, "ifelse")

  expect_error(
    ap_parse_transformation("no_assignment_here"),
    "Invalid transformation"
  )

  expect_error(
    ap_parse_transformation("123bad = x * 2"),
    "Invalid transformation output column"
  )
})

test_that("generated plot code rejects dangerous calls before execution", {
  code <- paste(
    "data <- data.frame(x = 1, y = 1)",
    "system('echo unsafe')",
    "p <- ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::geom_point()",
    sep = "\n"
  )

  expect_error(
    ap_validate_plot_code(code),
    "Disallowed call"
  )
})
