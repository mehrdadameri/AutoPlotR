test_that("agent card documents the simple AutoPlotR agent boundary", {
  card_path <- system.file("agent-card.yml", package = "AutoPlotR")
  if (!nzchar(card_path)) {
    card_path <- file.path(testthat::test_path("..", ".."), "inst", "agent-card.yml")
  }

  expect_true(file.exists(card_path))
  card <- yaml::read_yaml(card_path)
  expect_equal(card$name, "AutoPlotR")
  expect_equal(card$autonomy_level, "partial")
  expect_true(all(c("text_request", "data_profile") %in% card$sensors))
  expect_true(all(c("list_files", "load_data", "detect_data", "use_data", "profile", "plot") %in% card$tools))
  expect_true(length(card$guardrails) >= 3)
})

test_that("default config keeps structured agent calls deterministic and bounded", {
  cfg <- ap_load_config(list())

  for (agent_id in ap_agent_ids()) {
    expect_equal(cfg$agents[[agent_id]]$args$temperature, 0)
    expect_true(is.numeric(cfg$agents[[agent_id]]$args$max_tokens))
    expect_lte(cfg$agents[[agent_id]]$args$max_tokens, 4096)
  }
})

test_that("chat arguments pass decoding controls through provider params", {
  fake_chat <- function(system_prompt, model, params = list(), api_key = NULL) NULL

  args <- ap_prepare_chat_args(
    chat_fun = fake_chat,
    agent_args = list(temperature = 0, max_tokens = 2048, seed = 42),
    system_prompt = "system",
    model = "model",
    api_key = "key"
  )

  expect_equal(args$params$temperature, 0)
  expect_equal(args$params$max_tokens, 2048)
  expect_equal(args$params$seed, 42)
  expect_false("temperature" %in% names(args))
  expect_false("max_tokens" %in% names(args))
})

test_that("data profiling can bound very wide context", {
  wide <- as.data.frame(stats::setNames(as.list(seq_len(8)), paste0("col", seq_len(8))))

  profile <- ap_profile_data(wide, max_columns = 3, max_sample_rows = 1)

  expect_equal(profile$columns, 8)
  expect_equal(profile$profiled_columns, 3)
  expect_equal(profile$column_names, paste0("col", 1:3))
  expect_equal(profile$omitted_columns, paste0("col", 4:8))
})

test_that("planner retries once after validation failure", {
  bad <- mock_scatter_plan()
  bad$mappings$y <- "not_a_column"
  good <- mock_scatter_plan()
  withr::local_options(list(
    AutoPlotR.mock_planner_responses = list(bad, good),
    AutoPlotR.mock_planner_response_index = 0
  ))

  plan <- ap_plan(
    data.frame(speed = 1:3, distance = 1:3, group = c("a", "b", "a")),
    "scatter plot of distance by speed"
  )

  expect_equal(plan$mappings$y, "distance")
  expect_equal(getOption("AutoPlotR.mock_planner_response_index"), 2)
})

test_that("plot runs write manifest and trace artifacts for reproducibility", {
  data <- data.frame(speed = 1:5, distance = c(2, 4, 8, 16, 32), group = c("a", "b", "a", "b", "a"))
  plan <- structure(
    c(mock_scatter_plan(), list(design_guide_version = ap_load_viz_rules(refresh = TRUE)$version)),
    class = c("ap_viz_plan", "list")
  )
  td <- tempfile("autoplotr-eval-")

  mock_code <- paste(
    "p <- ggplot2::ggplot(data, ggplot2::aes(x = speed, y = distance, color = group)) +",
    "  ggplot2::geom_point() + ggplot2::theme_minimal()",
    "p",
    sep = "\n"
  )
  withr::local_options(list(AutoPlotR.mock_plotter_response = list(code = mock_code)))
  result <- ap_plot(data, plan = plan, output_dir = td, save_data = FALSE)

  expect_true(file.exists(result$manifest_path))
  expect_true(file.exists(result$trace_path))
  manifest <- jsonlite::read_json(result$manifest_path, simplifyVector = FALSE)
  expect_true(nzchar(manifest$run_id))
  expect_true(nzchar(manifest$input$data_hash))
  expect_true(nzchar(manifest$prompts$planner_hash))
  expect_true("session_info" %in% names(manifest))
  expect_true("ggplot2" %in% names(manifest$package_versions))

  trace_lines <- readLines(result$trace_path, warn = FALSE)
  expect_true(any(grepl('"event":"start"', trace_lines, fixed = TRUE)))
  expect_true(any(grepl('"event":"complete"', trace_lines, fixed = TRUE)))
})

test_that("live plotting pauses for low confidence or clarification questions", {
  plan <- mock_scatter_plan()
  plan$confidence <- 0.2
  expect_true(ap_plan_needs_clarification(plan))

  plan$confidence <- 0.95
  plan$clarification_questions <- "Which grouping variable should be used?"
  expect_true(ap_plan_needs_clarification(plan))

  plan$clarification_questions <- character()
  expect_false(ap_plan_needs_clarification(plan))
})

test_that("live agent can select a detected global data frame before profiling", {
  old <- if (exists("ap_eval_mtcars", envir = globalenv(), inherits = FALSE)) {
    get("ap_eval_mtcars", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  existed <- exists("ap_eval_mtcars", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (existed) {
      assign("ap_eval_mtcars", old, envir = globalenv())
    } else if (exists("ap_eval_mtcars", envir = globalenv(), inherits = FALSE)) {
      rm("ap_eval_mtcars", envir = globalenv())
    }
  }, add = TRUE)

  assign("ap_eval_mtcars", mtcars, envir = globalenv())
  state <- ap_live_state(root = tempdir())

  selected <- ap_live_use_global_data(state, "ap_eval_mtcars")

  expect_equal(selected$data_name, "ap_eval_mtcars")
  expect_equal(selected$rows, nrow(mtcars))
  expect_equal(state$data_name, "ap_eval_mtcars")
  expect_equal(nrow(ap_live_resolve_data(state)), nrow(mtcars))
  expect_true("mpg" %in% names(ap_live_resolve_data(state)))
})

test_that("live prompt advertises use_data after detecting environment data", {
  prompt <- ap_live_system_prompt("/project")

  expect_match(prompt, "use_data")
  expect_match(prompt, "detect_data")
})
