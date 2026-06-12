test_that("setup writes AutoPlotR config without launching UI", {
  td <- tempfile("autoplotr-config-")
  dir.create(td)
  withr::local_options(list(AutoPlotR.config_dir = td))

  path <- ap_setup(
    provider = "openai",
    model = "gpt-4o-mini",
    api_key = "test-key",
    launch = FALSE,
    persist_key = FALSE,
    overwrite = TRUE
  )

  expect_true(file.exists(path))
  cfg <- ap_load_config(path)
  expect_equal(cfg$llm$default_provider, "openai")
  expect_equal(cfg$llm$default_model, "gpt-4o-mini")
  expect_equal(cfg$llm$api_key_env, "OPENAI_API_KEY")
  expect_equal(Sys.getenv("OPENAI_API_KEY"), "test-key")
})
