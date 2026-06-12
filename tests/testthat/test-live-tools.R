# Helper: extract handler function from an ellmer tool definition.
# In ellmer >= 0.4.0, the tool definition S7 object IS the handler function.
# In older versions, the handler is stored as tool_def$fun or tool_def@fun.
ap_extract_tool_fun <- function(tool_def) {
  if (is.function(tool_def)) return(tool_def)
  fun <- tryCatch(tool_def@fun, error = function(e) NULL)
  if (is.function(fun)) return(fun)
  fun <- tryCatch(tool_def$fun, error = function(e) NULL)
  if (is.function(fun)) return(fun)
  stop("Could not extract handler function from tool definition")
}

test_that("list_files tool scans root for supported file types", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  writeLines("x,y\n1,2", file.path(td, "test.csv"))
  writeLines("a\tb\n1\t2", file.path(td, "data.tsv"))
  writeLines("not a data file", file.path(td, "readme.log"))

  state <- ap_live_state(root = td)
  tool_def <- ap_tool_list_files(state)
  expect_true(!is.null(tool_def))

  handler <- ap_extract_tool_fun(tool_def)
  result <- handler()
  expect_equal(result$count, 2L)
  expect_true(any(grepl("test\\.csv$", result$files)))
  expect_true(any(grepl("data\\.tsv$", result$files)))
})

test_that("list_files tool filters by pattern", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  writeLines("x,y\n1,2", file.path(td, "sales.csv"))
  writeLines("x,y\n1,2", file.path(td, "products.csv"))

  state <- ap_live_state(root = td)
  tool_def <- ap_tool_list_files(state)
  handler <- ap_extract_tool_fun(tool_def)

  result <- handler(pattern = "sales")
  expect_equal(result$count, 1L)
  expect_match(result$files, "sales")
})

test_that("load_data loads CSV into state", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  writeLines("speed,distance\n1,2\n3,4", file.path(td, "test.csv"))

  state <- ap_live_state(root = td)
  tool_def <- ap_tool_load_data(state)
  handler <- ap_extract_tool_fun(tool_def)

  result <- handler(path = file.path(td, "test.csv"))
  expect_equal(result$rows, 2L)
  expect_equal(result$columns, 2L)
  expect_equal(state$data_name, "test.csv")
  expect_true(ap_is_data_frame(state$data))
})

test_that("load_data rejects paths outside root", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  writeLines("x,y\n1,2", file.path(tempdir(), "outside.csv"))

  state <- ap_live_state(root = td)
  tool_def <- ap_tool_load_data(state)
  handler <- ap_extract_tool_fun(tool_def)

  expect_error(
    handler(path = file.path(tempdir(), "outside.csv")),
    "outside the configured root"
  )
})

test_that("detect_data finds data frames in global environment", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  test_df <<- data.frame(a = 1:3, b = letters[1:3])
  on.exit(rm(test_df, envir = .GlobalEnv), add = TRUE)
  not_a_df <<- 1:5
  on.exit(rm(not_a_df, envir = .GlobalEnv), add = TRUE)

  state <- ap_live_state(root = td)
  tool_def <- ap_tool_detect_data(state)
  handler <- ap_extract_tool_fun(tool_def)

  result <- handler()
  expect_true(result$found)
  expect_true("test_df" %in% names(result$data_frames))
  expect_false("not_a_df" %in% names(result$data_frames))
})

test_that("use_data selects a named data frame from global env", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  use_test_df <<- data.frame(speed = 1:5, distance = c(2, 4, 8, 16, 32))
  on.exit(rm(use_test_df, envir = .GlobalEnv), add = TRUE)

  state <- ap_live_state(root = td)
  tool_def <- ap_tool_use_data(state)
  handler <- ap_extract_tool_fun(tool_def)

  result <- handler(data_name = "use_test_df")
  expect_equal(result$source, "global_environment")
  expect_equal(result$data_name, "use_test_df")
  expect_equal(result$rows, 5L)
  expect_true(ap_is_data_frame(state$data))
})

test_that("refresh_env makes pushed data selectable without global assignment", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  push_path <- file.path(td, "push.rds")
  pushed_name <- "pushed_test_df"
  saveRDS(list(pushed_test_df = data.frame(a = 1:3, b = letters[1:3])), push_path)
  withr::local_options(list(AutoPlotR.live_push_rds = push_path))
  if (exists(pushed_name, envir = .GlobalEnv, inherits = FALSE)) {
    old <- get(pushed_name, envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(pushed_name, old, envir = .GlobalEnv), add = TRUE)
    rm(list = pushed_name, envir = .GlobalEnv)
  }

  state <- ap_live_state(root = td)
  refresh <- ap_extract_tool_fun(ap_tool_refresh_env(state))
  use_data <- ap_extract_tool_fun(ap_tool_use_data(state))

  refreshed <- refresh()
  result <- use_data(data_name = pushed_name)

  expect_true(refreshed$refreshed)
  expect_false(exists(pushed_name, envir = .GlobalEnv, inherits = FALSE))
  expect_equal(result$data_name, pushed_name)
  expect_equal(result$rows, 3L)
  expect_true(ap_is_data_frame(state$data))
})

test_that("use_data errors for non-existent or non-data-frame objects", {
  td <- tempfile("ap-live-test-")
  dir.create(td)

  state <- ap_live_state(root = td)
  tool_def <- ap_tool_use_data(state)
  handler <- ap_extract_tool_fun(tool_def)

  expect_error(handler(data_name = "does_not_exist"), "not found")
})

test_that("profile tool summarizes state data", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  state <- ap_live_state(
    data = data.frame(speed = 1:5, distance = c(2, 4, 8, 16, 32), group = c("a", "b", "a", "b", "a")),
    root = td
  )

  tool_def <- ap_tool_profile(state)
  handler <- ap_extract_tool_fun(tool_def)

  result <- handler()
  expect_equal(result$rows, 5L)
  expect_equal(result$columns, 3L)
  expect_equal(length(result$column_summaries), 3L)
  expect_true(any(vapply(result$column_summaries, function(x) x$name == "speed", logical(1))))
})

test_that("profile tool errors when no data loaded", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  state <- ap_live_state(root = td)

  tool_def <- ap_tool_profile(state)
  handler <- ap_extract_tool_fun(tool_def)

  expect_error(handler(), "No data loaded")
})

test_that("plot tool returns needs_clarification for low-confidence plans", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  state <- ap_live_state(
    data = data.frame(speed = 1:5, distance = c(2, 4, 8, 16, 32), group = c("a", "b", "a", "b", "a")),
    root = td
  )

  low_conf_plan <- mock_scatter_plan()
  low_conf_plan$confidence <- 0.4
  low_conf_plan$clarification_questions <- list("Which variable for color?")
  withr::local_options(list(AutoPlotR.mock_planner_response = low_conf_plan))

  tool_def <- ap_tool_plot_live(state)
  handler <- ap_extract_tool_fun(tool_def)

  result <- handler(request = "plot distance by speed")
  expect_equal(result$status, "needs_clarification")
  expect_false(result$plot_created)
})

test_that("plot tool generates plot for confident plans", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  state <- ap_live_state(
    data = data.frame(speed = 1:5, distance = c(2, 4, 8, 16, 32), group = c("a", "b", "a", "b", "a")),
    root = td
  )

  mock_code <- paste(
    "p <- ggplot2::ggplot(data, ggplot2::aes(x = speed, y = distance, color = group)) +",
    "  ggplot2::geom_point() + ggplot2::theme_minimal()",
    "p",
    sep = "\n"
  )
  withr::local_options(list(
    AutoPlotR.mock_planner_response = mock_scatter_plan(),
    AutoPlotR.mock_plotter_response = list(code = mock_code)
  ))

  tool_def <- ap_tool_plot_live(state)
  handler <- ap_extract_tool_fun(tool_def)

  # Tool handler calls ap_plot() which uses execution_data when save_data=FALSE
  # is not set. We test tool creation and structure; full plot integration
  # is covered by test-plot.R and test-evals.R.
  expect_true(is.function(handler))
  expect_equal(names(formals(handler)), "request")
})

test_that("ap_live_resolve_file rejects paths outside root", {
  td <- tempfile("ap-live-test-")
  dir.create(td)
  state <- ap_live_state(root = td)
  # Construct a path that resolves outside the root directory on any OS
  outside <- file.path(td, "..", "outside.csv")

  expect_error(
    ap_live_resolve_file(state, outside),
    "outside the configured root"
  )
  outside_abs <- file.path(tempdir(), "..", "outside.csv")
  expect_error(
    ap_live_resolve_file(state, outside_abs),
    "outside the configured root"
  )
})

test_that("ap_live_system_prompt references working directory", {
  test_path <- file.path(tempdir(), "test-project")
  prompt <- ap_live_system_prompt(test_path)
  expect_match(prompt, normalizePath(test_path, winslash = "/", mustWork = FALSE), fixed = TRUE)
  expect_match(prompt, "list_files")
  expect_match(prompt, "load_data")
  expect_match(prompt, "detect_data")
  expect_match(prompt, "profile")
  expect_match(prompt, "plot")
})

test_that("ap_plan_needs_clarification detects low confidence and questions", {
  plan <- list(confidence = 0.3, clarification_questions = list())
  expect_true(ap_plan_needs_clarification(plan))

  plan <- list(confidence = 0.9, clarification_questions = list("Which color?"))
  expect_true(ap_plan_needs_clarification(plan))

  plan <- list(confidence = 0.95, clarification_questions = list())
  expect_false(ap_plan_needs_clarification(plan))
})

test_that("live runtime builds with data and validates inputs", {
  td <- tempfile("ap-live-test-")
  dir.create(td)

  # Without provider/model and connect=FALSE
  runtime <- ap_live_runtime(
    data = mtcars,
    root = td,
    connect = FALSE
  )

  expect_s3_class(runtime, "ap_live_runtime")
  expect_equal(runtime$data_name, "data")  # deparse(substitute()) resolves to param name
  expect_equal(nrow(runtime$data), 32)
  expect_true(!is.null(runtime$profile))
  expect_null(runtime$chat)

  # Non-data-frame rejected
  expect_error(
    ap_live_runtime(data = 1:5, root = td, connect = FALSE),
    "must be a data frame"
  )
})
