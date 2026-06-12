#' Return the user AutoPlotR config path
#'
#' @param filename Config filename.
#' @return Path to an AutoPlotR user config file.
#' @examples
#' ap_config_path()
#' @export
ap_config_path <- function(filename = "config.yml") {
  dir <- getOption("AutoPlotR.config_dir", NULL)
  if (is.null(dir)) {
    dir <- tools::R_user_dir("AutoPlotR", "config")
  }
  file.path(dir, filename)
}

#' @noRd
#' @keywords internal
ap_agent_ids <- function() {
  c("planner", "plotter")
}

#' @noRd
#' @keywords internal
ap_default_config <- function() {
  list(
    version = 1,
    project = "AutoPlotR",
    llm = list(
      default_provider = NULL,
      default_model = NULL,
      api_key_env = NULL,
      provider_args = list()
    ),
    agents = list(
      planner = list(
        provider = NULL,
        model = NULL,
        api_key_env = NULL,
        args = list(temperature = 0, max_tokens = 2048),
        system_prompt = "prompts/planner.md"
      ),
      plotter = list(
        provider = NULL,
        model = NULL,
        api_key_env = NULL,
        args = list(temperature = 0, max_tokens = 2048),
        system_prompt = "prompts/plotter.md"
      )
    )
  )
}

#' Load AutoPlotR configuration
#'
#' @param config Config path, YAML/JSON string, list, or `NULL`.
#' @return A normalized config list.
#' @examples
#' names(ap_load_config())
#' @export
ap_load_config <- function(config = NULL) {
  if (is.list(config)) {
    cfg <- config
  } else {
    if (is.null(config)) {
      path <- ap_config_path()
      cfg <- if (file.exists(path)) yaml::read_yaml(path) else ap_default_config()
    } else {
      config <- as.character(config)[1]
      if (file.exists(config)) {
        cfg <- yaml::read_yaml(config)
      } else {
        cfg <- tryCatch(jsonlite::fromJSON(config, simplifyVector = FALSE), error = function(e) NULL)
        cfg <- cfg %||% tryCatch(yaml::yaml.load(config), error = function(e) NULL)
        if (is.null(cfg) || !is.list(cfg)) {
          ap_abort("Config must be a path, YAML/JSON text, list, or NULL.")
        }
      }
    }
  }

  defaults <- ap_default_config()
  cfg <- ap_deep_merge(defaults, cfg)
  cfg$llm$default_provider <- ap_scalar_chr(cfg$llm$default_provider)
  cfg$llm$default_model <- ap_scalar_chr(cfg$llm$default_model)
  cfg$llm$api_key_env <- ap_scalar_chr(cfg$llm$api_key_env)
  for (agent_id in ap_agent_ids()) {
    cfg$agents[[agent_id]]$provider <- ap_scalar_chr(
      cfg$agents[[agent_id]]$provider %||% cfg$llm$default_provider
    )
    cfg$agents[[agent_id]]$model <- ap_scalar_chr(
      cfg$agents[[agent_id]]$model %||% cfg$llm$default_model
    )
    cfg$agents[[agent_id]]$api_key_env <- ap_scalar_chr(
      cfg$agents[[agent_id]]$api_key_env %||% cfg$llm$api_key_env
    )
    cfg$agents[[agent_id]]$args <- ap_deep_merge(
      defaults$agents[[agent_id]]$args,
      cfg$agents[[agent_id]]$args %||% list()
    )
  }
  cfg
}

#' @noRd
#' @keywords internal
ap_write_config <- function(config, path = ap_config_path()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(config, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' List LLM providers exposed by ellmer
#'
#' @return Character vector of provider ids.
#' @examples
#' head(ap_llm_providers())
#' @export
ap_llm_providers <- function() {
  ns <- asNamespace("ellmer")
  exports <- getNamespaceExports("ellmer")
  internal <- ls(ns, all.names = TRUE)
  candidates <- unique(c(exports, internal))
  chat_funs <- grep("^chat_", candidates, value = TRUE)
  known_non_provider <- c(
    "chat_body", "chat_params", "chat_path", "chat_perform",
    "chat_perform_async_stream", "chat_perform_stream", "chat_request",
    "chat_resp_stream"
  )
  chat_funs <- setdiff(chat_funs, known_non_provider)
  chat_funs <- grep("^batch_chat_|^parallel_chat_", chat_funs, value = TRUE, invert = TRUE)
  chat_funs <- grep("_test$", chat_funs, value = TRUE, invert = TRUE)
  chat_funs <- chat_funs[vapply(chat_funs, function(x) {
    exists(x, envir = ns, inherits = FALSE) &&
      is.function(get(x, envir = ns, inherits = FALSE))
  }, logical(1))]
  sort(sub("^chat_", "", chat_funs))
}

#' @noRd
#' @keywords internal
ap_normalize_provider_id <- function(provider) {
  provider <- as.character(provider %||% "")[1]
  provider <- tolower(trimws(provider))
  provider <- sub("^chat_", "", provider)
  gsub("-", "_", provider, fixed = TRUE)
}

#' @noRd
#' @keywords internal
ap_provider_chat_function <- function(provider) {
  provider <- ap_normalize_provider_id(provider)
  fun_name <- paste0("chat_", provider)
  ns <- asNamespace("ellmer")
  if (!exists(fun_name, envir = ns, inherits = FALSE)) {
    ap_abort(
      as.character(
        glue::glue(
          "Unsupported LLM provider: {provider}. Available ellmer providers: {paste(ap_llm_providers(), collapse = ', ')}"
        )
      )
    )
  }
  get(fun_name, envir = ns, inherits = FALSE)
}

#' @noRd
#' @keywords internal
ap_provider_api_key_env <- function(provider) {
  provider <- ap_normalize_provider_id(provider)
  paste0(toupper(gsub("[^A-Za-z0-9]+", "_", provider)), "_API_KEY")
}

#' @noRd
#' @keywords internal
ap_validate_setup_provider <- function(provider) {
  provider <- ap_normalize_provider_id(provider)
  if (!nzchar(provider)) {
    ap_abort("Provider is required.")
  }
  if (!provider %in% ap_llm_providers()) {
    ap_abort(
      paste0(
        "Unsupported LLM provider: ", provider,
        ". Available ellmer providers: ", paste(ap_llm_providers(), collapse = ", ")
      )
    )
  }
  provider
}

#' @noRd
#' @keywords internal
ap_validate_setup_model <- function(model) {
  model <- ap_scalar_chr(model)
  if (is.null(model)) {
    ap_abort("Model is required.")
  }
  model
}

#' @noRd
#' @keywords internal
ap_renviron_user_path <- function() {
  path <- Sys.getenv("R_ENVIRON_USER", unset = "")
  if (nzchar(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(path.expand("~"), ".Renviron"), winslash = "/", mustWork = FALSE)
}

#' @noRd
#' @keywords internal
ap_escape_renviron_value <- function(value) {
  value <- as.character(value %||% "")[1]
  value <- gsub("\\\\", "\\\\\\\\", value)
  value <- gsub("\"", "\\\\\"", value)
  paste0("\"", value, "\"")
}

#' @noRd
#' @keywords internal
ap_save_api_key <- function(provider, api_key = NULL, persist = TRUE) {
  provider <- ap_validate_setup_provider(provider)
  api_key <- ap_scalar_chr(api_key)
  env_name <- ap_provider_api_key_env(provider)
  if (is.null(api_key)) {
    return(list(env_name = env_name, saved = FALSE, persisted = FALSE, renviron = NULL))
  }

  do.call(Sys.setenv, stats::setNames(list(api_key), env_name))
  renviron <- NULL
  persisted <- FALSE
  if (isTRUE(persist)) {
    renviron <- ap_renviron_user_path()
    dir.create(dirname(renviron), recursive = TRUE, showWarnings = FALSE)
    existing <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
    pattern <- paste0("^\\s*", env_name, "\\s*=")
    existing <- existing[!grepl(pattern, existing)]
    writeLines(c(existing, paste0(env_name, "=", ap_escape_renviron_value(api_key))), renviron, useBytes = TRUE)
    persisted <- TRUE
  }
  list(env_name = env_name, saved = TRUE, persisted = persisted, renviron = renviron)
}

#' @noRd
#' @keywords internal
ap_user_config_template <- function(provider = NULL, model = NULL) {
  cfg <- ap_default_config()
  cfg$llm$default_provider <- provider %||% NULL
  cfg$llm$default_model <- model %||% NULL
  cfg$llm$api_key_env <- if (!is.null(provider)) ap_provider_api_key_env(provider) else NULL
  for (agent_id in ap_agent_ids()) {
    cfg$agents[[agent_id]]$provider <- NULL
    cfg$agents[[agent_id]]$model <- NULL
    cfg$agents[[agent_id]]$api_key_env <- NULL
    cfg$agents[[agent_id]]$args <- list()
  }
  cfg
}

#' @noRd
#' @keywords internal
ap_open_file_in_rstudio <- function(path) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && isTRUE(rstudioapi::isAvailable())) {
    rstudioapi::navigateToFile(path)
    return(TRUE)
  }
  FALSE
}

#' @noRd
#' @keywords internal
ap_setup_app <- function(config, path) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    return(FALSE)
  }
  providers <- ap_llm_providers()
  if (!length(providers)) {
    ap_abort("No callable LLM providers found in ellmer. Install or update ellmer, then run ap_setup() again.")
  }
  provider_choices <- c(stats::setNames("", ""), stats::setNames(providers, providers))
  default_provider <- config$llm$default_provider %||% ""
  default_model <- config$llm$default_model %||% ""
  default_temp <- config$agents$planner$args$temperature %||% NULL
  default_seed <- config$agents$planner$args$seed %||% NULL

  ui <- shiny::fluidPage(
    shiny::tags$h3("AutoPlotR LLM Setup"),
    shiny::selectInput("default_provider", "Provider", choices = provider_choices, selected = default_provider, selectize = FALSE),
    shiny::textInput("default_model", "Model", value = default_model, placeholder = "Exact model name, e.g. gpt-4o-mini"),
    shiny::passwordInput("api_key", "API key", value = ""),
    shiny::checkboxInput("persist_key", paste0("Save API key to ", ap_renviron_user_path(), " for future R sessions"), TRUE),
    shiny::tags$hr(),
    shiny::tags$details(
      shiny::tags$summary("Advanced model parameters"),
      shiny::tags$div(style = "margin-left: 20px; margin-top: 10px;",
        shiny::numericInput("temperature", "Temperature", value = default_temp, min = 0, max = 2, step = 0.1),
        shiny::tags$small("Leave blank for provider default. 0 = deterministic, higher = more random."),
        shiny::tags$br(),
        shiny::numericInput("seed", "Seed", value = default_seed, min = 1, step = 1),
        shiny::tags$small("Optional. Set for reproducible outputs across calls.")
      )
    ),
    shiny::tags$hr(),
    shiny::checkboxInput("same", "Use same provider and model for all agents", TRUE),
    shiny::uiOutput("agent_fields"),
    shiny::tags$hr(),
    shiny::actionButton("save", "Save config"),
    shiny::actionButton("yaml", "Advanced: edit YAML")
  )

  server <- function(input, output, session) {

    .ap_read_temperature <- function(val) {
      if (is.null(val) || is.na(val) || !nzchar(as.character(val))) return(NULL)
      as.numeric(val)
    }
    .ap_read_seed <- function(val) {
      if (is.null(val) || is.na(val) || !nzchar(as.character(val))) return(NULL)
      as.integer(val)
    }
    .ap_build_agent_args <- function(temp, seed) {
      args <- list()
      t <- .ap_read_temperature(temp)
      s <- .ap_read_seed(seed)
      if (!is.null(t)) args$temperature <- t
      if (!is.null(s)) args$seed <- s
      args
    }

    output$agent_fields <- shiny::renderUI({
      if (isTRUE(input$same)) {
        return(NULL)
      }
      shiny::tagList(lapply(ap_agent_ids(), function(agent_id) {
        shiny::fluidRow(
          shiny::column(3, shiny::strong(agent_id)),
          shiny::column(3, shiny::selectInput(paste0(agent_id, "_provider"), "Provider", choices = provider_choices, selected = input$default_provider, selectize = FALSE)),
          shiny::column(3, shiny::textInput(paste0(agent_id, "_model"), "Model", value = input$default_model %||% "")),
          shiny::column(3, shiny::numericInput(paste0(agent_id, "_temp"), "Temp", value = input$temperature, min = 0, max = 2, step = 0.1))
        )
      }))
    })

    shiny::observeEvent(input$save, {
      provider <- ap_validate_setup_provider(input$default_provider)
      model <- ap_validate_setup_model(input$default_model)
      out <- ap_user_config_template(provider = provider, model = model)
      out$llm$api_key_env <- ap_provider_api_key_env(provider)
      default_args <- .ap_build_agent_args(input$temperature, input$seed)
      if (!isTRUE(input$same)) {
        for (agent_id in ap_agent_ids()) {
          out$agents[[agent_id]]$provider <- ap_validate_setup_provider(input[[paste0(agent_id, "_provider")]])
          out$agents[[agent_id]]$model <- ap_validate_setup_model(input[[paste0(agent_id, "_model")]])
          out$agents[[agent_id]]$api_key_env <- ap_provider_api_key_env(out$agents[[agent_id]]$provider)
          out$agents[[agent_id]]$args <- .ap_build_agent_args(
            input[[paste0(agent_id, "_temp")]], input$seed
          )
        }
      } else {
        for (agent_id in ap_agent_ids()) {
          out$agents[[agent_id]]$args <- default_args
        }
      }
      ap_write_config(out, path)
      key_result <- ap_save_api_key(provider, api_key = input$api_key, persist = isTRUE(input$persist_key))
      shiny::stopApp(list(
        config = normalizePath(path, mustWork = FALSE),
        provider = provider,
        model = model,
        api_key_env = key_result$env_name,
        api_key_saved = key_result$saved,
        api_key_persisted = key_result$persisted,
        renviron = key_result$renviron
      ))
    })

    shiny::observeEvent(input$yaml, {
      ap_write_config(config, path)
      ap_open_file_in_rstudio(path)
    })
  }

  viewer <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE) && isTRUE(rstudioapi::isAvailable())) {
    viewer <- rstudioapi::viewer
  }
  shiny::runApp(shiny::shinyApp(ui, server), launch.browser = viewer)
  TRUE
}

#' Configure AutoPlotR LLM provider and API key
#'
#' @param provider Provider id matching an `ellmer::chat_*()` provider.
#' @param model Exact model name for the selected provider.
#' @param api_key Optional API key.
#' @param path Config path.
#' @param launch Whether to launch the setup UI.
#' @param persist_key Whether to save the API key to `.Renviron`.
#' @param overwrite Whether to overwrite an existing config.
#' @param temperature Optional default temperature for LLM calls.
#' @param seed Optional random seed for reproducible LLM calls.
#' @return Config path, invisibly.
#' @examplesIf interactive()
#' ap_setup(provider = "openai", model = "gpt-4o-mini")
#' @export
ap_setup <- function(provider = NULL, model = NULL, api_key = NULL,
                     path = ap_config_path(), launch = interactive(),
                     persist_key = TRUE, overwrite = FALSE,
                     temperature = NULL, seed = NULL) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  config_exists <- file.exists(path)

  if (!isTRUE(launch) && (!is.null(provider) || !is.null(model) || !is.null(api_key))) {
    if (is.null(provider) || is.null(model)) {
      ap_abort("When launch = FALSE, provide both provider and model.")
    }
    provider <- ap_validate_setup_provider(provider)
    model <- ap_validate_setup_model(model)
    cfg <- ap_user_config_template(provider = provider, model = model)
    cfg$llm$api_key_env <- ap_provider_api_key_env(provider)
    args <- list()
    if (!is.null(temperature) && is.finite(as.numeric(temperature))) args$temperature <- as.numeric(temperature)
    if (!is.null(seed) && !is.na(as.integer(seed))) args$seed <- as.integer(seed)
    if (length(args) > 0L) {
      for (agent_id in ap_agent_ids()) {
        cfg$agents[[agent_id]]$args <- args
      }
    }
    out <- ap_write_config(cfg, path)
    ap_save_api_key(provider, api_key = api_key, persist = persist_key)
    return(invisible(out))
  }

  if (config_exists && !isTRUE(overwrite)) {
    message("AutoPlotR config already exists: ", path)
    if (isTRUE(launch)) {
      existing_config <- ap_load_config(path)
      opened_app <- FALSE
      if (requireNamespace("shiny", quietly = TRUE)) {
        opened_app <- isTRUE(ap_setup_app(existing_config, path))
      }
      if (!opened_app) {
        ap_open_file_in_rstudio(path)
      }
    }
    return(invisible(path))
  }

  config <- ap_user_config_template(provider = provider, model = model)
  ap_write_config(config, path)
  message("AutoPlotR config created: ", path)

  if (isTRUE(launch)) {
    opened_app <- FALSE
    if (requireNamespace("shiny", quietly = TRUE)) {
      opened_app <- isTRUE(ap_setup_app(config, path))
    }
    if (!opened_app) {
      ap_open_file_in_rstudio(path)
    }
  }

  invisible(path)
}
