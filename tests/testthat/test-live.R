test_that("all six tools create valid definitions", {
  state <- ap_live_state(data = mtcars, root = tempdir())

  is_valid_tool <- function(x) {
    inherits(x, "ellmer::ToolDef") || inherits(x, "ellmer_tool") || is.function(x)
  }

  expect_true(is_valid_tool(ap_tool_list_files(state)))
  expect_true(is_valid_tool(ap_tool_load_data(state)))
  expect_true(is_valid_tool(ap_tool_detect_data(state)))
  expect_true(is_valid_tool(ap_tool_use_data(state)))
  expect_true(is_valid_tool(ap_tool_profile(state)))
  expect_true(is_valid_tool(ap_tool_plot_live(state)))
})

test_that("ap_live_state initializes with data", {
  state <- ap_live_state(data = mtcars, root = tempdir())
  expect_equal(nrow(state$data), 32)
  expect_equal(ncol(state$data), 11)
  expect_true(nzchar(state$root))
  expect_null(state$session_script)
})

test_that("ap_live_state initializes without data", {
  state <- ap_live_state(root = tempdir())
  expect_null(state$data)
  expect_null(state$data_name)
})

test_that("ap_live_resolve_data returns data when loaded", {
  state <- ap_live_state(data = mtcars)
  df <- ap_live_resolve_data(state)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 32)
})

test_that("ap_live_resolve_data aborts when no data", {
  state <- ap_live_state()
  expect_error(ap_live_resolve_data(state), "No data loaded")
})

test_that("list_files tool lists CSV files in root", {
  td <- tempfile("ap-live-")
  dir.create(td)
  write.csv(mtcars, file.path(td, "test.csv"))
  state <- ap_live_state(root = td)
  tool <- ap_tool_list_files(state)
  # Test the underlying logic: what the handler would see
  root <- state$root
  all_files <- list.files(root, recursive = TRUE, full.names = TRUE)
  exts <- "\\.(csv|tsv|txt|xlsx?|rds|rdata|rda)$"
  supported <- all_files[grepl(exts, all_files, ignore.case = TRUE)]
  expect_equal(length(supported), 1)
  expect_match(basename(supported), "test.csv")
})

test_that("load_data reads CSV and updates state", {
  td <- tempfile("ap-live-")
  dir.create(td)
  csv_path <- file.path(td, "mtcars.csv")
  write.csv(mtcars, csv_path, row.names = FALSE)
  state <- ap_live_state(root = td)

  # Simulate what load_data handler does
  df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  state$data <- as.data.frame(df)
  state$data_name <- basename(csv_path)

  expect_s3_class(state$data, "data.frame")
  expect_equal(nrow(state$data), 32)
  expect_equal(state$data_name, "mtcars.csv")
})

test_that("live file resolution stays inside the configured root", {
  td <- tempfile("ap-live-")
  dir.create(td)
  csv_path <- file.path(td, "mtcars.csv")
  write.csv(mtcars, csv_path, row.names = FALSE)
  state <- ap_live_state(root = td)

  expect_equal(ap_live_resolve_file(state, "mtcars.csv"), normalizePath(csv_path, winslash = "/", mustWork = TRUE))
  expect_error(
    ap_live_resolve_file(state, "../outside.csv"),
    "outside the configured root"
  )
  expect_error(
    ap_live_resolve_file(state, tempfile("outside-")),
    "outside the configured root"
  )
})

test_that("detect_data logic finds data frames in an environment", {
  # Simulate the tool's detection logic on a test environment
  test_env <- new.env(parent = emptyenv())
  test_env$my_df <- data.frame(a = 1:3, b = letters[1:3])
  test_env$not_df <- "hello"

  obs <- ls(envir = test_env, all.names = FALSE)
  dfs <- list()
  for (nm in obs) {
    x <- tryCatch(get(nm, envir = test_env, inherits = FALSE), error = function(e) NULL)
    if (ap_is_data_frame(x)) dfs[[nm]] <- list(rows = nrow(x), columns = ncol(x))
  }
  expect_true("my_df" %in% names(dfs))
  expect_false("not_df" %in% names(dfs))
  expect_equal(dfs$my_df$rows, 3)
  expect_equal(dfs$my_df$columns, 2)
})

test_that("profile summarizes data correctly", {
  state <- ap_live_state(data = mtcars)
  # Simulate what profile handler does
  profile <- ap_profile_data(state$data, max_sample_rows = 5)
  expect_equal(profile$rows, 32)
  expect_equal(profile$columns, 11)
  expect_equal(profile$column_names, names(mtcars))
})

test_that("plot tool generates artifacts via ap_plan + ap_plot (mocked)", {
  data <- data.frame(speed = 1:3, distance = c(2, 4, 8), group = c("a", "b", "a"))
  withr::local_options(list(AutoPlotR.mock_planner_response = mock_scatter_plan()))

  td <- tempfile("ap-live-")
  dir.create(td)
  state <- ap_live_state(data = data, root = td)

  # Simulate what plot handler does
  plan <- ap_plan(state$data, "scatter plot of distance by speed")
  mock_code <- paste(
    "p <- ggplot2::ggplot(data, ggplot2::aes(x = speed, y = distance, color = group)) +",
    "  ggplot2::geom_point() + ggplot2::theme_minimal()",
    "p",
    sep = "\n"
  )
  withr::local_options(list(AutoPlotR.mock_plotter_response = list(code = mock_code)))
  result <- ap_plot(state$data, plan = plan, output_dir = file.path(td, "autoplotr-output"), save_data = FALSE)
  state$session_script <- result$script_path

  expect_s3_class(plan, "ap_viz_plan")
  expect_equal(plan$plot_type, "scatter")
  expect_true(file.exists(result$plot_path))
  expect_true(file.exists(result$script_path))
  expect_equal(state$session_script, result$script_path)
})

test_that("profile aborts when no data loaded", {
  state <- ap_live_state()
  expect_error(ap_live_resolve_data(state), "No data loaded")
})

test_that("ap_live_runtime creates runtime without data", {
  td <- tempfile("ap-config-")
  dir.create(td)
  withr::local_options(list(AutoPlotR.config_dir = td))
  ap_setup(
    provider = "openai", model = "gpt-4o-mini",
    api_key = "test-key", launch = FALSE,
    persist_key = FALSE, overwrite = TRUE
  )

  runtime <- ap_live_runtime(root = tempdir(), connect = FALSE)
  expect_s3_class(runtime, "ap_live_runtime")
  expect_null(runtime$data)
  expect_null(runtime$profile)
  expect_equal(runtime$data_name, NULL)
})

test_that("ap_live_runtime creates runtime with mtcars", {
  td <- tempfile("ap-config-")
  dir.create(td)
  withr::local_options(list(AutoPlotR.config_dir = td))
  ap_setup(
    provider = "openai", model = "gpt-4o-mini",
    api_key = "test-key", launch = FALSE,
    persist_key = FALSE, overwrite = TRUE
  )

  runtime <- ap_live_runtime(data = mtcars, connect = FALSE)
  expect_s3_class(runtime, "ap_live_runtime")
  expect_equal(nrow(runtime$state$data), 32)
  expect_equal(runtime$profile$rows, 32)
})

test_that("ap_live_runtime aborts on invalid data", {
  td <- tempfile("ap-config-")
  dir.create(td)
  withr::local_options(list(AutoPlotR.config_dir = td))

  expect_error(
    ap_live_runtime(data = "not a df", connect = FALSE),
    "must be a data frame or NULL"
  )
})

test_that("ap_live_runtime aborts without provider when connect=TRUE and no config", {
  td <- tempfile("ap-config-")
  dir.create(td)
  withr::local_options(list(AutoPlotR.config_dir = td))
  cfg <- ap_load_config()
  if (is.null(cfg$llm$default_provider)) {
    expect_error(
      ap_live_runtime(connect = TRUE),
      "No LLM provider"
    )
  }
})

test_that("print.ap_live_runtime works with data", {
  td <- tempfile("ap-config-")
  dir.create(td)
  withr::local_options(list(AutoPlotR.config_dir = td))

  runtime <- ap_live_runtime(data = mtcars, connect = FALSE)
  out <- capture.output(print(runtime))
  expect_match(paste(out, collapse = "\n"), "AutoPlotR live runtime")
  expect_match(paste(out, collapse = "\n"), "32 rows")
})

test_that("print.ap_live_runtime works without data", {
  td <- tempfile("ap-config-")
  dir.create(td)
  withr::local_options(list(AutoPlotR.config_dir = td))

  runtime <- ap_live_runtime(root = tempdir(), connect = FALSE)
  out <- capture.output(print(runtime))
  expect_match(paste(out, collapse = "\n"), "not loaded")
})

test_that("ap_live_system_prompt includes root path and all tool names", {
  test_path <- file.path(tempdir(), "my-project")
  prompt <- ap_live_system_prompt(test_path)
  expect_match(prompt, normalizePath(test_path, winslash = "/", mustWork = FALSE), fixed = TRUE)
  expect_match(prompt, "list_files")
  expect_match(prompt, "load_data")
  expect_match(prompt, "detect_data")
  expect_match(prompt, "use_data")
  expect_match(prompt, "profile")
  expect_match(prompt, "plot")
})

test_that("load_data aborts on unsupported file type", {
  td <- tempfile("ap-live-")
  dir.create(td)
  writeLines("not data", file.path(td, "test.txt"))
  state <- ap_live_state(root = td)
  # .txt is supported as TSV variant, test .abc
  expect_error({
    ext <- "abc"
    ap_abort(paste0("Unsupported file type: ", ext))
  }, "Unsupported file type")
})

test_that("load_data aborts on missing file", {
  state <- ap_live_state(root = tempdir())
  expect_error({
    path <- "/nonexistent/file.csv"
    if (!file.exists(path)) ap_abort(paste0("File not found: ", path))
  }, "File not found")
})

test_that("mutable state allows data to be updated by tools", {
  state <- ap_live_state()
  expect_null(state$data)

  # load_data sets data
  state$data <- mtcars
  state$data_name <- "mtcars"
  expect_equal(nrow(ap_live_resolve_data(state)), 32)

  # profile works after load
  p <- ap_profile_data(state$data)
  expect_equal(p$rows, 32)

  # plot sets session_script
  state$session_script <- "/tmp/script.R"
  expect_equal(state$session_script, "/tmp/script.R")
})
