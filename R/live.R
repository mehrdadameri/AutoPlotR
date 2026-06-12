# Live chat interface for AutoPlotR --------------------------------------------

#' @noRd
#' @keywords internal
ap_live_auto_save <- function(state) {
  output_rds <- getOption("AutoPlotR.live_output_rds", NULL)
  if (is.null(output_rds) || is.null(state$data)) return(invisible())
  tryCatch(
    {
      if (!is.null(state$data_name)) {
        attr(state$data, "autoplotr_data_name") <- state$data_name
      }
      saveRDS(state$data, output_rds)
    },
    error = function(e) invisible()
  )
  invisible()
}

#' @noRd
#' @keywords internal
ap_live_system_prompt <- function(root = getwd()) {
  paste0(
    "You are the AutoPlotR Plotting Assistant. You help users explore data ",
    "and create visualizations through conversation. ",
    "Working directory: ", normalizePath(root, winslash = "/", mustWork = FALSE), ". ",
    "You have tools to: ",
    "(1) list_files - scan for CSV/Excel/RDS/RData files in the working directory, ",
    "(2) load_data - read a file into memory, ",
    "(3) detect_data - list all data frames from the background process and from the main RStudio session (via ap_push_env()), ",
    "(4) refresh_env - re-read datasets pushed from the main RStudio session (ask user to run ap_push_env() in RStudio first if they report newly loaded data), ",
    "(5) use_data - select one detected data frame by exact name, ",
    "(6) profile - summarize columns, types, and ranges of the loaded data, ",
    "(7) plot - generate a ggplot2 visualization from a natural language request. ",
    "Workflow: when a user mentions a file, list the directory first. ",
    "When a user mentions a dataset name, call detect_data first to see what is available. ",
    "If the dataset is not found by detect_data, ask the user to load it in RStudio and run ap_push_env(), then call refresh_env. ",
    "If the requested dataset appears in detect_data, call use_data with that exact name before profile or plot. ",
    "Do not load an unrelated file when the user named a detected data frame. ",
    "Load or select data before plotting. Profile after loading or selecting so the user sees what's available. ",
    "When generating plots, explain what you created: plot type, variables, design choices. ",
    "If the request is ambiguous, ask a short clarifying question before calling a tool."
  )
}

# Shared mutable state (tools read/write this environment) --------------------

#' @noRd
#' @keywords internal
ap_live_state <- function(data = NULL, root = getwd()) {
  env <- new.env(parent = emptyenv())
  env$data <- data
  env$data_name <- if (!is.null(data)) deparse(substitute(data)) else NULL
  env$root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  env$session_script <- NULL
  env$env_data <- new.env(parent = emptyenv())
  env
}

#' @noRd
#' @keywords internal
ap_live_resolve_data <- function(state) {
  if (is.null(state$data)) {
    ap_abort("No data loaded. Use load_data to read a file, or pass a data frame to ap_live().")
  }
  state$data
}

#' @noRd
#' @keywords internal
ap_live_scalar_arg <- function(x) {
  if (is.list(x) && length(x) == 1L) {
    x <- x[[1]]
  }
  x <- as.character(x)[1]
  if (is.na(x)) "" else x
}

#' @noRd
#' @keywords internal
ap_live_resolve_file <- function(state, path) {
  root <- normalizePath(state$root, winslash = "/", mustWork = TRUE)
  path <- ap_live_scalar_arg(path)
  if (!nzchar(path)) {
    ap_abort("File path is required.")
  }
  # Resolve candidate to an absolute, canonical path.
  # normalizePath with mustWork = FALSE resolves . and .. without requiring
  # the path to exist yet. This lets us detect traversal attempts (e.g.
  # "../outside.csv" or "/etc/passwd") before checking file existence.
  candidate <- if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(root, path), winslash = "/", mustWork = FALSE)
  }
  # Path-traversal guard: candidate must be root or a descendant of root.
  root_prefix <- paste0(root, "/")
  if (!(identical(candidate, root) || startsWith(candidate, root_prefix))) {
    ap_abort("File path is outside the configured root.")
  }
  if (!file.exists(candidate)) {
    ap_abort(paste0("File not found: ", candidate))
  }
  candidate
}

#' @noRd
#' @keywords internal
ap_plan_needs_clarification <- function(plan, min_confidence = 0.6) {
  questions <- plan$clarification_questions %||% character()
  questions <- questions[nzchar(as.character(questions))]
  confidence <- suppressWarnings(as.numeric(plan$confidence %||% NA_real_))
  length(questions) > 0L || (!is.na(confidence) && confidence < min_confidence)
}

#' @noRd
#' @keywords internal
ap_live_load_pushed_data <- function(state) {
  push_path <- getOption("AutoPlotR.live_push_rds", NULL)
  if (is.null(push_path) || !file.exists(push_path)) {
    return(character())
  }
  pushed <- tryCatch(readRDS(push_path), error = function(e) NULL)
  if (!is.list(pushed) || is.data.frame(pushed)) {
    return(character())
  }
  loaded <- character()
  for (nm in names(pushed)) {
    if (ap_is_data_frame(pushed[[nm]])) {
      assign(nm, as.data.frame(pushed[[nm]]), envir = state$env_data)
      loaded <- c(loaded, nm)
    }
  }
  loaded
}

ap_live_find_data <- function(state, data_name, env = globalenv()) {
  if (!is.null(state$env_data) && exists(data_name, envir = state$env_data, inherits = FALSE)) {
    return(get(data_name, envir = state$env_data, inherits = FALSE))
  }
  if (exists(data_name, envir = env, inherits = FALSE)) {
    return(get(data_name, envir = env, inherits = FALSE))
  }
  NULL
}

ap_live_use_global_data <- function(state, data_name, env = globalenv()) {
  data_name <- ap_live_scalar_arg(data_name)
  if (!nzchar(data_name)) {
    ap_abort("Data frame name is required.")
  }
  data <- ap_live_find_data(state, data_name, env = env)
  if (is.null(data)) {
    ap_abort(paste0("Data frame not found in the global environment: ", data_name))
  }
  if (!ap_is_data_frame(data)) {
    ap_abort(paste0("Object exists but is not a data frame: ", data_name))
  }
  state$data <- as.data.frame(data)
  state$data_name <- data_name
  ap_live_auto_save(state)
  list(
    source = "global_environment",
    data_name = data_name,
    rows = nrow(state$data),
    columns = ncol(state$data),
    column_names = names(state$data)
  )
}

# Tool: list_files -------------------------------------------------------------

#' @noRd
#' @keywords internal
ap_tool_list_files <- function(state) {
  ap_tool(
    name = "list_files",
    description = paste0(
      "List CSV, Excel, RDS, and RData files in the working directory. ",
      "Use this when the user mentions a file or asks what data is available."
    ),
    args_props = list(
      pattern = ellmer::type_string(
        "Optional file pattern, e.g. '*.csv' or 'sales'. Leave blank to list all supported files.",
        required = FALSE
      )
    ),
    fun = function(pattern = NULL) {
      root <- state$root
      dir.create(root, recursive = TRUE, showWarnings = FALSE)
      all_files <- list.files(root, recursive = TRUE, full.names = TRUE)
      exts <- "\\.(csv|tsv|txt|xlsx?|rds|rdata|rda)$"
      supported <- all_files[grepl(exts, all_files, ignore.case = TRUE)]
      if (!is.null(pattern) && nzchar(as.character(pattern)[1])) {
        supported <- supported[grepl(ap_live_scalar_arg(pattern), basename(supported), ignore.case = TRUE)]
      }
      list(
        root = root,
        count = length(supported),
        files = supported
      )
    }
  )
}

# Tool: load_data --------------------------------------------------------------

#' @noRd
#' @keywords internal
ap_tool_load_data <- function(state) {
  ap_tool(
    name = "load_data",
    description = paste0(
      "Load a data file (CSV, TSV, Excel, RDS, RData) into the analysis session. ",
      "The data becomes available for profiling and plotting. ",
      "Optionally also assigns it to the RStudio global environment."
    ),
    args_props = list(
      path = ellmer::type_string(
        "Path to the data file. Must be inside the configured working directory."
      ),
      assign_to_env = ellmer::type_string(
        "Set to 'true' to also load the data into the RStudio global environment. Default 'false'.",
        required = FALSE
      )
    ),
    fun = function(path, assign_to_env = FALSE) {
      path <- ap_live_resolve_file(state, path)
      ext <- tolower(tools::file_ext(path))
      df <- switch(
        ext,
        csv = utils::read.csv(path, stringsAsFactors = FALSE),
        tsv = utils::read.delim(path, stringsAsFactors = FALSE),
        txt = utils::read.delim(path, stringsAsFactors = FALSE),
        xlsx = ,
        xls = {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            ap_abort("Package 'readxl' is required to read Excel files. Install it with install.packages('readxl').")
          }
          readxl::read_excel(path)
        },
        rds = readRDS(path),
        rdata = ,
        rda = {
          e <- new.env(parent = emptyenv())
          load(path, envir = e)
          dfs <- eapply(e, function(x) x, all.names = TRUE)
          dfs <- dfs[vapply(dfs, is.data.frame, logical(1))]
          if (length(dfs) == 0) ap_abort("No data frames found in RData file.")
          dfs[[1]]
        },
        ap_abort(paste0("Unsupported file type: ", ext))
      )
      if (!ap_is_data_frame(df)) {
        ap_abort("Loaded object is not a data frame.")
      }
      state$data <- as.data.frame(df)
      state$data_name <- basename(path)
      ap_live_auto_save(state)
      # assign_to_env may arrive as logical or character from ellmer
      push <- isTRUE(assign_to_env) || is.character(assign_to_env) && tolower(assign_to_env) == "true"
      assigned_name <- NULL
      if (push) {
        assigned_name <- make.names(tools::file_path_sans_ext(basename(path)))
        # Data is assigned to .GlobalEnv on chat exit via ap_live() on.exit handler.
        # Deferred assignment is intentional -- Shiny's reactive cycle prevents
        # RStudio from refreshing the Environment pane while chat is running.
      }
      profile <- ap_profile_data(state$data)
      list(
        path = normalizePath(path, winslash = "/", mustWork = FALSE),
        rows = nrow(state$data),
        columns = ncol(state$data),
        column_names = names(state$data),
        assigned_to_env = push,
        env_name = assigned_name %||% ""
      )
    }
  )
}

# Tool: detect_data ------------------------------------------------------------

#' @noRd
#' @keywords internal
ap_tool_detect_data <- function(state) {
  ap_tool(
    name = "detect_data",
    description = paste0(
      "List all data frames currently available. Checks: (1) the background ",
      "process's own global environment, (2) data frames pushed from the main ",
      "RStudio session via ap_push_env(). Use this when the user mentions a ",
      "dataset name. Returns each data frame's name, dimensions, and column names."
    ),
    args_props = list(),
    fun = function() {
      dfs <- list()
      # 1. Data frames pushed from the main RStudio session into runtime state
      if (!is.null(state$env_data)) {
        for (nm in ls(envir = state$env_data, all.names = FALSE)) {
          x <- tryCatch(get(nm, envir = state$env_data, inherits = FALSE), error = function(e) NULL)
          if (ap_is_data_frame(x)) {
            dfs[[nm]] <- list(rows = nrow(x), columns = ncol(x), column_names = names(x))
          }
        }
      }
      # 2. Data frames in the background process's own .GlobalEnv
      obs <- ls(envir = .GlobalEnv, all.names = FALSE)
      for (nm in obs) {
        x <- tryCatch(get(nm, envir = .GlobalEnv, inherits = FALSE), error = function(e) NULL)
        if (ap_is_data_frame(x)) {
          dfs[[nm]] <- list(rows = nrow(x), columns = ncol(x), column_names = names(x))
        }
      }
      # 3. Data frames pushed from the main RStudio session via ap_push_env()
      push_path <- getOption("AutoPlotR.live_push_rds", NULL)
      if (!is.null(push_path) && file.exists(push_path)) {
        pushed <- tryCatch(readRDS(push_path), error = function(e) NULL)
        if (is.list(pushed) && !is.data.frame(pushed)) {
          for (nm in names(pushed)) {
            if (!nm %in% names(dfs) && ap_is_data_frame(pushed[[nm]])) {
              dfs[[nm]] <- list(
                rows = nrow(pushed[[nm]]),
                columns = ncol(pushed[[nm]]),
                column_names = names(pushed[[nm]])
              )
            }
          }
        }
      }
      if (length(dfs) == 0) {
        return(list(
          found = FALSE, data_frames = list(),
          hint = "No data frames found. Use list_files and load_data to load a file, or load data in RStudio and run ap_push_env() to sync it here."
        ))
      }
      list(found = TRUE, count = length(dfs), data_frames = dfs)
    }
  )
}

# Tool: refresh_env ------------------------------------------------------------

#' @noRd
#' @keywords internal
ap_tool_refresh_env <- function(state) {
  ap_tool(
    name = "refresh_env",
    description = paste0(
      "Re-read data frames from the main RStudio session. ",
      "Use this when the user says they just loaded or created a dataset in RStudio ",
      "and it doesn't appear in detect_data results yet. ",
      "This re-reads the shared push file that ap_push_env() writes to."
    ),
    args_props = list(
      dummy = ellmer::type_string("Ignored. Call with empty string.", required = FALSE)
    ),
    fun = function(dummy = NULL) {
      push_path <- getOption("AutoPlotR.live_push_rds", NULL)
      if (is.null(push_path) || !file.exists(push_path)) {
        return(list(
          refreshed = FALSE,
          message = "No push data available. Ask the user to run ap_push_env() in their RStudio console after loading data."
        ))
      }
      pushed <- tryCatch(readRDS(push_path), error = function(e) NULL)
      if (!is.list(pushed) || is.data.frame(pushed)) {
        return(list(refreshed = FALSE, message = "Push file is empty or invalid."))
      }
      loaded <- ap_live_load_pushed_data(state)
      list(
        refreshed = TRUE,
        datasets_available = loaded,
        count = length(loaded),
        message = if (length(loaded) > 0L) {
          paste0("Synced ", length(loaded), " dataset(s): ", paste(loaded, collapse = ", "), ". Use detect_data to list them all.")
        } else {
          "No datasets found in push file. Ask the user to load data in RStudio and run ap_push_env()."
        }
      )
    }
  )
}

# Tool: use_data ---------------------------------------------------------------

#' @noRd
#' @keywords internal
ap_tool_use_data <- function(state) {
  ap_tool(
    name = "use_data",
    description = paste0(
      "Select a data frame that already exists in the RStudio/global R environment. ",
      "Use this after detect_data when the user asks to work with a named dataset ",
      "such as mtcars. The selected data becomes available for profile and plot."
    ),
    args_props = list(
      data_name = ellmer::type_string(
        "Exact data frame name returned by detect_data, e.g. 'mtcars'."
      )
    ),
    fun = function(data_name) {
      ap_live_use_global_data(state, data_name)
    }
  )
}

# Tool: profile ----------------------------------------------------------------

#' @noRd
#' @keywords internal
ap_tool_profile <- function(state) {
  ap_tool(
    name = "profile",
    description = paste0(
      "Summarize the currently loaded data: row count, column count, ",
      "column names, types (numeric, categorical, datetime, text), ",
      "missing counts, distinct value counts, and ranges."
    ),
    args_props = list(),
    fun = function() {
      data <- ap_live_resolve_data(state)
      profile <- ap_profile_data(data, max_sample_rows = 5)
      columns <- lapply(profile$column_profiles, function(col) {
        list(
          name = col$name, type = col$type, missing = col$missing,
          distinct = col$distinct,
          range = if (is.null(col$range)) character() else col$range,
          sample = col$sample_values
        )
      })
      list(
        rows = profile$rows,
        columns = profile$columns,
        column_summaries = unname(columns)
      )
    }
  )
}

# Tool: plot -------------------------------------------------------------------

#' @noRd
#' @keywords internal
ap_tool_plot_live <- function(state, config = NULL) {
  ap_tool(
    name = "plot",
    description = paste0(
      "Generate a ggplot2 visualization from a natural language request. ",
      "Creates a visualization plan, generates editable R code, renders the plot, ",
      "and saves all artifacts. The R script is written to disk and can be ",
      "edited and re-run independently. Returns plot path, script path, and summary."
    ),
    args_props = list(
      request = ellmer::type_string(
        "Natural language description of the plot, e.g. 'scatter plot of mpg vs wt colored by cyl'"
      )
    ),
    fun = function(request) {
      data <- ap_live_resolve_data(state)
      plan <- ap_plan(data, as.character(request)[1], config = config)
      if (ap_plan_needs_clarification(plan)) {
        return(list(
          status = "needs_clarification",
          plot_created = FALSE,
          confidence = plan$confidence,
          clarification_questions = plan$clarification_questions %||% character(),
          message = "AutoPlotR needs clarification before rendering this plot."
        ))
      }
      output_dir <- file.path(state$root, "autoplotr-output")
      result <- ap_plot(data, plan = plan, output_dir = output_dir, config = config)
      # Append to session script
      if (is.null(state$session_script)) {
        state$session_script <- result$script_path
      }
      list(
        plot_type = plan$plot_type,
        title = plan$labels$title %||% as.character(request)[1],
        subtitle = plan$labels$subtitle %||% "",
        x_label = plan$labels$x %||% "",
        y_label = plan$labels$y %||% "",
        plot_path = result$plot_path,
        script_path = result$script_path,
        output_dir = result$output_dir,
        design_rationale = plan$design_rationale %||% "No rationale recorded",
        confidence = plan$confidence,
        accessibility = list(
          palette = plan$accessibility$palette,
          colorblind_safe = plan$accessibility$colorblind_safe
        ),
        attempts = length(result$attempts)
      )
    }
  )
}

# Runtime builder --------------------------------------------------------------

#' Build an AutoPlotR live runtime
#'
#' Creates an ellmer chat with file, data, and plotting tools registered.
#'
#' @param data Optional data frame. If NULL, user must load data via tools.
#' @param root Working directory for file scanning and artifact output.
#' @param config Optional AutoPlotR config.
#' @param provider Optional provider override.
#' @param model Optional model override.
#' @param api_key Optional API key.
#' @param connect Whether to create the live ellmer chat.
#' @return A runtime list.
#' @noRd
#' @keywords internal
ap_live_runtime <- function(data = NULL, root = getwd(), config = NULL,
                            provider = NULL, model = NULL, api_key = NULL,
                            connect = TRUE) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  cfg <- ap_load_config(config)
  provider <- ap_scalar_chr(provider %||% cfg$llm$default_provider)
  model <- ap_scalar_chr(model %||% cfg$llm$default_model)

  if (!is.null(data) && !ap_is_data_frame(data)) {
    ap_abort("`data` must be a data frame or NULL.")
  }

  if (isTRUE(connect) && (is.null(provider) || is.null(model))) {
    ap_abort(
      "No LLM provider or model configured. Run ap_setup() or pass provider and model."
    )
  }

  state <- ap_live_state(data = data, root = root)
  ap_live_load_pushed_data(state)

  chat <- NULL
  chat_error <- NULL
  if (isTRUE(connect) && requireNamespace("ellmer", quietly = TRUE)) {
    chat <- tryCatch(
      {
        ch <- ap_create_chat(
          agent = "planner",
          provider = provider,
          model = model,
          api_key = api_key,
          config = cfg
        )
        sys_prompt <- ap_live_system_prompt(root)
        if ("set_system_prompt" %in% names(ch)) {
          ch$set_system_prompt(sys_prompt)
        }

        ap_add_tool(ch, ap_tool_list_files(state))
        ap_add_tool(ch, ap_tool_load_data(state))
        ap_add_tool(ch, ap_tool_detect_data(state))
        ap_add_tool(ch, ap_tool_refresh_env(state))
        ap_add_tool(ch, ap_tool_use_data(state))
        ap_add_tool(ch, ap_tool_profile(state))
        ap_add_tool(ch, ap_tool_plot_live(state, config = cfg))

        # Greeting skipped: some providers (DeepSeek) fail to serialise
        # turn pairs with empty user content. The agent works fine without it.
        ch
      },
      error = function(e) e
    )
    if (inherits(chat, "error")) {
      chat_error <- chat$message
      chat <- NULL
    }
  }

  profile <- if (!is.null(data)) ap_profile_data(data) else NULL

  runtime <- list(
    data = data,
    data_name = state$data_name,
    state = state,
    root = root,
    profile = profile,
    config = cfg,
    provider = provider,
    model = model,
    chat = chat,
    chat_error = chat_error,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  class(runtime) <- c("ap_live_runtime", "list")
  runtime
}

# Collect all data frames from an environment (default: .GlobalEnv) into a named
# list suitable for serialisation. Used to export the main R session's datasets
# into the background process so detect_data / use_data can find them.
#' @noRd
#' @keywords internal
ap_export_global_data_frames <- function(env = globalenv()) {
  nms <- ls(envir = env, all.names = FALSE)
  dfs <- list()
  for (nm in nms) {
    x <- tryCatch(get(nm, envir = env, inherits = FALSE), error = function(e) NULL)
    if (ap_is_data_frame(x)) {
      dfs[[nm]] <- as.data.frame(x)
    }
  }
  dfs
}

# Background launcher ----------------------------------------------------------

#' @noRd
#' @keywords internal
ap_live_background_script <- function(data_path, root, config_path,
                                       provider, model, api_key, output_rds,
                                       pkg_path = NULL, env_data_path = NULL,
                                       env_push_path = NULL) {
  c(
    if (is.null(pkg_path)) {
      'suppressPackageStartupMessages(library(AutoPlotR))'
    } else {
      c(
        paste0('if (!requireNamespace("pkgload", quietly = TRUE)) {'),
        '  install.packages("pkgload", repos = "https://cran.r-project.org")',
        '}',
        paste0('pkgload::load_all(', deparse(pkg_path), ', quiet = TRUE)')
      )
    },
    '',
    paste0('.data_path <- ', if (is.null(data_path)) 'NULL' else deparse(data_path)),
    paste0('.root <- ', deparse(normalizePath(root, winslash = "/", mustWork = FALSE))),
    paste0('.config <- ', if (is.null(config_path)) 'NULL' else deparse(config_path)),
    paste0('.provider <- ', deparse(provider)),
    paste0('.model <- ', deparse(model)),
    paste0('.api_key <- ', deparse(api_key)),
    paste0('.output_rds <- ', deparse(output_rds)),
    paste0('.env_data_path <- ', if (is.null(env_data_path)) 'NULL' else deparse(env_data_path)),
    paste0('.env_push_path <- ', if (is.null(env_push_path)) 'NULL' else deparse(env_push_path)),
    '',
    'options(AutoPlotR.live_output_rds = .output_rds)',
    'options(AutoPlotR.live_push_rds = .env_push_path)',
    '',
    '# Write startup marker so ap_fetch_data() knows the job ran',
    'saveRDS(list(status = "running"), .output_rds)',
    '',
    '.data <- if (!is.null(.data_path) && file.exists(.data_path)) readRDS(.data_path) else NULL',
    '',
    'runtime <- tryCatch(',
    '  ap_live_runtime(',
    '    data = .data,',
    '    root = .root,',
    '    config = .config,',
    '    provider = .provider,',
    '    model = .model,',
    '    api_key = .api_key,',
    '    connect = TRUE',
    '  ),',
    '  error = function(e) {',
    '    saveRDS(list(error = conditionMessage(e)), .output_rds)',
    '    stop("AutoPlotR runtime error: ", conditionMessage(e))',
    '  }',
    ')',
    '',
    'if (!is.null(runtime$chat_error)) {',
    '  saveRDS(list(error = runtime$chat_error), .output_rds)',
    '  stop("Chat error: ", runtime$chat_error)',
    '}',
    '',
    'on.exit({',
    '  state <- runtime$state',
    '  if (!is.null(state$data)) {',
    '    saveRDS(state$data, .output_rds)',
    '    message("AutoPlotR: data saved to ", .output_rds)',
    '  } else {',
    '    saveRDS(list(status = "no_data"), .output_rds)',
    '    message("AutoPlotR: no data to save")',
    '  }',
    '}, add = TRUE)',
    '',
    'message("AutoPlotR: chat starting. Output RDS: ", .output_rds)',
    'ap_launch_live_ui(runtime$chat, launch_browser = TRUE)'
  )
}

#' Push RStudio data frames to a running background chat
#'
#' After starting a background `ap_live()` session, load or create data frames
#' in your RStudio console, then call `ap_push_env()` to make them visible
#' to the chat agent's `detect_data` and `use_data` tools.
#'
#' @param output_rds Path to the push RDS file. If NULL, uses the
#'   most recent background session's push path.
#' @return The push path, invisibly.
#' @export
ap_push_env <- function(output_rds = NULL) {
  if (is.null(output_rds)) {
    output_rds <- getOption("AutoPlotR.last_push_rds", NULL)
    if (is.null(output_rds)) {
      output_rds <- getOption("AutoPlotR.live_push_rds", NULL)
    }
  }
  if (is.null(output_rds)) {
    ap_abort(
      "No active background chat found. Start a chat with ap_live(launch_browser = TRUE) first."
    )
  }
  dfs <- ap_export_global_data_frames(globalenv())
  saveRDS(dfs, output_rds)
  n <- length(dfs)
  packageStartupMessage(
    "AutoPlotR: pushed ", n, " data frame", if (n != 1L) "s" else "",
    " to background chat (", paste(names(dfs), collapse = ", "), ")."
  )
  invisible(output_rds)
}

#' Fetch data modified during a background AutoPlotR chat session
#'
#' After a background `ap_live()` session completes, call this function
#' to load the modified data into your global environment.
#'
#' @param output_rds Path to the output RDS file. If NULL, uses the
#'   most recent background session output.
#' @param assign_to_env If TRUE, assign the data to .GlobalEnv.
#' @return The data frame, invisibly.
#' @export
ap_fetch_data <- function(output_rds = NULL, assign_to_env = TRUE) {
  if (is.null(output_rds)) {
    output_rds <- getOption("AutoPlotR.last_output_rds", NULL)
    # Also check for live session mid-chat
    if (is.null(output_rds)) {
      output_rds <- getOption("AutoPlotR.live_output_rds", NULL)
    }
  }
  if (is.null(output_rds) || !file.exists(output_rds)) {
    ap_abort(paste0(
      "No background session output found. ",
      "If chat is still running, load data first via chat, then try again. ",
      "Check the Background Jobs pane in RStudio - job may have failed. ",
      "Pass the output_rds path if you have it."
    ))
  }
  data <- readRDS(output_rds)

  if (is.list(data) && !is.data.frame(data)) {
    if (!is.null(data$error)) {
      ap_abort("Background chat failed: ", data$error)
    }
    if (!is.null(data$status) && data$status == "running") {
      message("AutoPlotR: chat is still running in background. No data loaded yet.")
      message("Load data via the chat (list_files + load_data), then run ap_fetch_data() again.")
      return(invisible(NULL))
    }
    if (!is.null(data$status) && data$status == "no_data") {
      ap_abort(
        "Background chat exited but no data was loaded or modified. ",
        "If you loaded data in the chat, the session may have crashed."
      )
    }
  }

  if (!ap_is_data_frame(data)) {
    ap_abort("Output RDS does not contain a data frame.")
  }

  data_name <- attr(data, "autoplotr_data_name", exact = TRUE)
  if (is.null(data_name) || !nzchar(data_name)) {
    data_name <- "autoplotr_data"
  }
  if (isTRUE(assign_to_env)) {
    base::assign(data_name, data, envir = globalenv())
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        isTRUE(try(rstudioapi::isAvailable(), silent = TRUE))) {
      try(rstudioapi::executeCommand("refreshEnvironment"), silent = TRUE)
    }
    packageStartupMessage("AutoPlotR: '", data_name, "' assigned to .GlobalEnv (", nrow(data), " rows).")
  }
  invisible(data)
}

# One-call launcher ------------------------------------------------------------

#' Launch the AutoPlotR live chat interface
#'
#' Opens an interactive chat session for data exploration and visualization.
#' The agent can scan directories, load data files, detect datasets in your
#' RStudio environment, profile columns, and generate ggplot2 plots -- all
#' through conversation.
#'
#' When `launch_browser = TRUE`, the chat runs in a **background R process**
#' so your R console stays free. The chat opens in your web browser and you
#' can continue running code in RStudio while chatting. After the chat ends,
#' call [ap_fetch_data()] to load modified data back into your environment.
#'
#' When `launch_browser = FALSE`, the chat runs in the terminal and blocks
#' the R thread until you exit (Esc / Ctrl+C).
#'
#' @param data Optional data frame. If NULL, load data via chat (list_files ->
#'   load_data) or let the agent detect data in your RStudio environment.
#' @param root Working directory for file scanning and plot output.
#'   Defaults to `getwd()`.
#' @param config Optional AutoPlotR config path, text, or list.
#' @param provider Optional LLM provider override.
#' @param model Optional LLM model override.
#' @param api_key Optional API key for this session.
#' @param launch Whether to launch the chat UI.
#' @param launch_browser Use background process + browser (non-blocking).
#'   Set to FALSE for terminal mode (blocks R thread until exit).
#' @return An `ap_live_runtime` list, invisibly. For background mode,
#'   returns a list with background job details.
#' @examplesIf interactive()
#' ap_live(mtcars)
#' ap_live(root = "~/project")  # scan directory, load files, plot
#' @export
ap_live <- function(data = NULL, root = getwd(), config = NULL,
                    provider = NULL, model = NULL, api_key = NULL,
                    launch = interactive(), launch_browser = interactive()) {

  if (isTRUE(launch) && isTRUE(launch_browser)) {
    return(ap_live_nonblocking(
      data = data, root = root, config = config,
      provider = provider, model = model, api_key = api_key
    ))
  }

  runtime <- ap_live_runtime(
    data = data,
    root = root,
    config = config,
    provider = provider,
    model = model,
    api_key = api_key,
    connect = TRUE
  )

  if (!is.null(runtime$chat_error)) {
    ap_abort(paste0(
      "Failed to create live chat: ", runtime$chat_error, ". ",
      "Run ap_setup() to configure an LLM provider, or check your API key."
    ))
  }

  if (isTRUE(launch)) {
    if (is.null(runtime$chat)) {
      ap_abort(
        "No live chat available. Run ap_setup() or check that ellmer is installed."
      )
    }
    on.exit({
      state <- runtime$state
      if (!is.null(state$data) && !is.null(state$data_name)) {
        nm <- make.names(state$data_name)
        base::assign(nm, state$data, envir = globalenv())
        if (requireNamespace("rstudioapi", quietly = TRUE) &&
            isTRUE(try(rstudioapi::isAvailable(), silent = TRUE))) {
          try(rstudioapi::executeCommand("refreshEnvironment"), silent = TRUE)
        }
        packageStartupMessage("AutoPlotR: '", nm, "' is in .GlobalEnv (", nrow(state$data), " rows).")
      }
    }, add = TRUE)

    ap_launch_live_ui(runtime$chat, launch_browser = launch_browser)
  }

  invisible(runtime)
}

# Non-blocking launcher (background process) -----------------------------------

#' @noRd
#' @keywords internal
ap_live_nonblocking <- function(data, root, config, provider, model, api_key) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  # Detect if package is properly installed or just loaded via load_all().
  # load_all() registers the namespace, so requireNamespace returns TRUE either
  # way. Check if the package path lives outside standard library locations.
  pkg_path <- NULL
  pkg_dir <- tryCatch(find.package("AutoPlotR"), error = function(e) NULL)
  if (!is.null(pkg_dir)) {
    in_lib <- any(vapply(.libPaths(), function(lib) {
      startsWith(normalizePath(pkg_dir, mustWork = FALSE),
                 normalizePath(lib, mustWork = FALSE))
    }, logical(1)))
    if (!in_lib) {
      pkg_path <- pkg_dir
    }
  } else {
    pkg_path <- getwd()
  }

  # Save data to temp RDS so background process can read it
  data_path <- NULL
  if (!is.null(data)) {
    data_path <- tempfile("autoplotr-live-data-", fileext = ".rds")
    saveRDS(as.data.frame(data), data_path)
  }

  # Export data frames from .GlobalEnv so detect_data / use_data work in the
  # background process (which has its own separate .GlobalEnv).
  env_data_path <- NULL
  env_dfs <- ap_export_global_data_frames(globalenv())
  if (length(env_dfs) > 0L) {
    env_data_path <- tempfile("autoplotr-live-env-", fileext = ".rds")
    saveRDS(env_dfs, env_data_path)
  }

  # Resolve config to a file path the background process can read
  config_path <- NULL
  if (is.character(config) && length(config) == 1L && file.exists(config)) {
    config_path <- config
  } else if (!is.null(config)) {
    config_path <- tempfile("autoplotr-live-config-", fileext = ".yml")
    cfg <- ap_load_config(config)
    dir.create(dirname(config_path), recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(cfg, config_path)
  }

  # Output RDS for data flow-back from background process
  output_rds <- tempfile("autoplotr-live-output-", fileext = ".rds")

  # Push RDS for live env sync: main session writes via ap_push_env(),
  # background process reads it in detect_data / refresh_env tools.
  env_push_path <- tempfile("autoplotr-live-push-", fileext = ".rds")
  # Initialise with current data frames so detect_data works immediately
  saveRDS(env_dfs, env_push_path)

  # Build self-contained R script
  script_lines <- ap_live_background_script(
    data_path = data_path,
    root = root,
    config_path = config_path,
    provider = provider,
    model = model,
    api_key = api_key,
    output_rds = output_rds,
    pkg_path = pkg_path,
    env_data_path = env_data_path,
    env_push_path = env_push_path
  )
  script <- tempfile("autoplotr-live-", fileext = ".R")
  writeLines(script_lines, script)

  # Store paths so ap_fetch_data() / ap_push_env() can find them
  options(AutoPlotR.last_output_rds = output_rds)
  options(AutoPlotR.live_output_rds = output_rds)
  options(AutoPlotR.last_push_rds = env_push_path)
  options(AutoPlotR.live_push_rds = env_push_path)

  # Launch in background
  in_rstudio <- requireNamespace("rstudioapi", quietly = TRUE) &&
    isTRUE(try(rstudioapi::isAvailable(), silent = TRUE)) &&
    exists("jobRunScript", where = asNamespace("rstudioapi"), inherits = FALSE)

  if (in_rstudio) {
    job_id <- tryCatch(
      rstudioapi::jobRunScript(
        path = script,
        name = "AutoPlotR Chat",
        workingDir = root,
        importEnv = FALSE
      ),
      error = function(e) NULL
    )
    if (!is.null(job_id)) {
      message("AutoPlotR chat started as RStudio background job.")
      message("Chat opens in your browser. R console is free -- run any code.")
      message("When chat ends, run ap_fetch_data() to load data back.")
      return(invisible(list(
        mode = "background",
        job_id = job_id,
        script = script,
        output_rds = output_rds
      )))
    }
  }

  # Fallback: callr or system()
  if (requireNamespace("callr", quietly = TRUE)) {
    job <- callr::r_bg(function(script_path) {
      source(script_path, echo = FALSE)
    }, args = list(script_path = script))
    message("AutoPlotR chat started in background process (PID: ", job$get_pid(), ").")
    message("Chat opens in your browser. R console is free -- run any code.")
    message("When chat ends, run ap_fetch_data() to load data back.")
    return(invisible(list(
      mode = "background",
      pid = job$get_pid(),
      script = script,
      output_rds = output_rds
    )))
  }

  # Last resort: system Rscript
  system(paste("Rscript", shQuote(script)), wait = FALSE)
  message("AutoPlotR chat started via Rscript in background.")
  message("Chat opens in your browser. R console is free -- run any code.")
  message("When chat ends, run ap_fetch_data() to load data back.")
  invisible(list(
    mode = "background",
    script = script,
    output_rds = output_rds
  ))
}

#' Print an AutoPlotR live runtime
#'
#' @param x An `ap_live_runtime`.
#' @param ... Unused.
#' @export
print.ap_live_runtime <- function(x, ...) {
  cat("AutoPlotR live runtime\n")
  cat("Root: ", x$root, "\n", sep = "")
  if (!is.null(x$profile)) {
    cat("Data: ", x$data_name, " (", x$profile$rows, " rows, ", x$profile$columns, " cols)\n", sep = "")
  } else {
    cat("Data: not loaded\n")
  }
  cat("Provider: ", x$provider, "\n", sep = "")
  cat("Model: ", x$model, "\n", sep = "")
  cat("Chat: ", if (is.null(x$chat)) "not connected" else "connected", "\n", sep = "")
  invisible(x)
}
