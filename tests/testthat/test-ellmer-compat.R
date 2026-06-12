test_that("ellmer as_json compatibility methods register and handle NULL", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("S7")

  as_json <- get("as_json", envir = asNamespace("ellmer"), inherits = FALSE)
  ns <- environment(.ap_register_ellmer_methods)
  old_registered <- get(".ap_ellmer_methods_registered", envir = ns)
  was_locked <- bindingIsLocked(".ap_ellmer_methods_registered", ns)
  if (was_locked) unlockBinding(".ap_ellmer_methods_registered", ns)
  on.exit({
    assign(".ap_ellmer_methods_registered", old_registered, envir = ns)
    if (was_locked) lockBinding(".ap_ellmer_methods_registered", ns)
  }, add = TRUE)

  assign(".ap_ellmer_methods_registered", FALSE, envir = ns)
  expect_silent(.ap_register_ellmer_methods())
  expect_true(get(".ap_ellmer_methods_registered", envir = ns))

  provider <- ellmer::Provider(
    name = "Test",
    model = "test-model",
    base_url = "https://example.com",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL
  )

  expect_null(as_json(provider, NULL))
  expect_equal(as_json(provider, list(NULL)), list())

  deepseek_provider <- get(
    "ProviderDeepSeek",
    envir = asNamespace("ellmer"),
    inherits = FALSE
  )(
    name = "DeepSeek",
    model = "deepseek-chat",
    base_url = "https://api.deepseek.com",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = function() "test-key"
  )

  expect_null(as_json(deepseek_provider, NULL))
  expect_equal(as_json(deepseek_provider, list(NULL)), list())
})
