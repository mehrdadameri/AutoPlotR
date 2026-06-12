#' Generate and render a plot from a plan or natural language request
#'
#' @param data A data frame.
#' @param request Optional natural language request. Required when `plan` is
#'   `NULL`.
#' @param plan Optional `ap_viz_plan`.
#' @param output_dir Directory where artifacts are written.
#' @param filename Output filename stem.
#' @param save_data Whether to save the input data as `data.rds`.
#' @param install Missing package policy: `"ask"`, `"never"`, or `"always"`.
#' @param retries Number of code execution repair retries.
#' @param config Optional AutoPlotR config.
#' @param provider Optional provider override.
#' @param model Optional model override.
#' @param api_key Optional API key override.
#' @return An `ap_plot_result` list.
#' @examplesIf interactive()
#' result <- ap_plot(mtcars, "scatter plot of mpg by wt", output_dir = tempdir())
#' file.exists(result$plot_path)
#' @export
ap_plot <- function(data, request = NULL, plan = NULL,
                    output_dir = "autoplotr-output", filename = NULL,
                    save_data = TRUE, install = c("ask", "never", "always"),
                    retries = 2, config = NULL, provider = NULL,
                    model = NULL, api_key = NULL) {
  install <- match.arg(install)
  if (!ap_is_data_frame(data)) {
    ap_abort("`data` must be a data frame.")
  }
  if (is.null(plan)) {
    if (is.null(request)) {
      ap_abort("Provide `request` when `plan` is NULL.")
    }
    plan <- ap_plan(data, request, config = config, provider = provider, model = model, api_key = api_key)
  } else {
    profile_for_validation <- ap_profile_data(data)
    ap_validate_plan(plan, profile_for_validation)
  }

  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  stem <- ap_safe_filename(filename %||% tools::file_path_sans_ext(plan$output$filename), "autoplotr-plot")
  plot_path <- file.path(output_dir, paste0(stem, ".png"))
  script_path <- file.path(output_dir, paste0(stem, ".R"))
  data_path <- file.path(output_dir, "data.rds")
  profile_path <- file.path(output_dir, "profile.json")
  plan_path <- file.path(output_dir, "plan.json")
  diagnostics_path <- file.path(output_dir, "diagnostics.json")
  manifest_path <- file.path(output_dir, "manifest.json")
  trace_path <- file.path(output_dir, "trace.jsonl")
  if (file.exists(trace_path)) {
    unlink(trace_path)
  }
  run_id <- ap_run_id()
  ap_trace_event(trace_path, "start", run_id = run_id, output_dir = output_dir)

  profile <- ap_profile_data(data)
  data_hash <- ap_hash_object(as.data.frame(data))
  execution_data <- NULL
  if (isTRUE(save_data)) {
    saveRDS(as.data.frame(data), data_path)
  } else {
    data_path <- NULL
    execution_data <- as.data.frame(data)
  }
  ap_write_json(profile, profile_path)
  ap_write_json(unclass(plan), plan_path)
  ap_trace_event(trace_path, "plan_validated", run_id = run_id, plot_type = plan$plot_type)

  required <- unique(ap_plan_required_packages(plan))
  ap_check_required_packages(required, install = install)

  rules <- ap_load_viz_rules(plan$plot_type)
  cfg <- ap_load_config(config)
  effective_provider <- ap_scalar_chr(provider %||% cfg$agents$plotter$provider %||% cfg$llm$default_provider)
  effective_model <- ap_scalar_chr(model %||% cfg$agents$plotter$model %||% cfg$llm$default_model)
  code <- ap_plotter_code(
    plan = plan,
    data_path = data_path,
    plot_path = plot_path,
    rules = rules,
    config = config,
    provider = provider,
    model = model,
    api_key = api_key
  )
  ap_validate_plot_code(code)
  writeLines(code, script_path, useBytes = TRUE)
  ap_trace_event(trace_path, "code_validated", run_id = run_id, script_path = script_path)

  diagnostics <- ap_execute_plot_script(
    script_path = script_path,
    plot_path = plot_path,
    retries = retries,
    plan = plan,
    data_path = data_path,
    rules = rules,
    config = config,
    provider = provider,
    model = model,
    api_key = api_key,
    execution_data = execution_data
  )
  ap_write_json(list(attempts = diagnostics$attempts), diagnostics_path)
  ap_trace_event(
    trace_path,
    "execution_attempt",
    run_id = run_id,
    attempts = length(diagnostics$attempts),
    ok = isTRUE(diagnostics$attempts[[length(diagnostics$attempts)]]$ok)
  )
  manifest <- list(
    run_id = run_id,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    autonomy_level = "partial",
    llm = list(
      provider = effective_provider,
      model = effective_model,
      use_agent = TRUE,
      temperature = cfg$agents$plotter$args$temperature %||% NULL,
      max_tokens = cfg$agents$plotter$args$max_tokens %||% NULL
    ),
    input = list(
      rows = nrow(data),
      columns = ncol(data),
      data_saved = isTRUE(save_data),
      data_hash = data_hash
    ),
    prompts = list(
      planner_hash = ap_hash_text(ap_agent_prompt("planner", cfg)),
      plotter_hash = ap_hash_text(ap_agent_prompt("plotter", cfg)),
      rules_hash = ap_hash_text(ap_chr_json(rules)),
      design_guide_version = plan$design_guide_version %||% NULL
    ),
    artifacts = list(
      plot = plot_path,
      script = script_path,
      data = if (isTRUE(save_data)) data_path else NULL,
      profile = profile_path,
      plan = plan_path,
      diagnostics = diagnostics_path,
      trace = trace_path
    ),
    artifact_hashes = list(
      plot = ap_hash_file(plot_path),
      script = ap_hash_file(script_path),
      data = if (isTRUE(save_data)) ap_hash_file(data_path) else NULL,
      plan = ap_hash_file(plan_path)
    ),
    package_versions = ap_package_versions(),
    session_info = utils::capture.output(utils::sessionInfo())
  )
  ap_write_json(manifest, manifest_path)
  ap_trace_event(trace_path, "complete", run_id = run_id, manifest_path = manifest_path, plot_path = plot_path)

  result <- list(
    plot = diagnostics$plot,
    plan = plan,
    output_dir = output_dir,
    plot_path = plot_path,
    script_path = script_path,
    data_path = if (isTRUE(save_data)) data_path else NULL,
    profile_path = profile_path,
    plan_path = plan_path,
    diagnostics_path = diagnostics_path,
    manifest_path = manifest_path,
    trace_path = trace_path,
    run_id = run_id,
    attempts = diagnostics$attempts
  )
  class(result) <- c("ap_plot_result", "list")
  result
}

#' @noRd
#' @keywords internal
ap_plan_required_packages <- function(plan) {
  plan$package %||% "ggplot2"
}

#' @noRd
#' @keywords internal
ap_check_required_packages <- function(packages, install = c("ask", "never", "always")) {
  install <- match.arg(install)
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) {
    return(invisible(TRUE))
  }
  if (identical(install, "never") || (!interactive() && identical(install, "ask"))) {
    ap_abort(paste0("Missing required package(s): ", paste(missing, collapse = ", ")))
  }
  if (identical(install, "ask")) {
    answer <- utils::askYesNo(paste0("Install missing package(s): ", paste(missing, collapse = ", "), "?"))
    if (!isTRUE(answer)) {
      ap_abort("Missing package installation was not approved.")
    }
  }
  utils::install.packages(missing)
  invisible(TRUE)
}

#' @noRd
#' @keywords internal
ap_plotter_code <- function(plan, data_path, plot_path, rules,
                            config = NULL, provider = NULL, model = NULL,
                            api_key = NULL, previous_error = NULL) {
  mock <- getOption("AutoPlotR.mock_plotter_response", NULL)
  if (!is.null(mock)) {
    return(mock$code %||% as.character(mock)[1])
  }
  chat <- ap_create_chat("plotter", provider = provider, model = model, api_key = api_key, config = config)
  prompt <- ap_build_plotter_prompt(plan, data_path, plot_path, rules)
  if (!is.null(previous_error)) {
    prompt <- paste(prompt, "Previous execution error:", previous_error, sep = "\n\n")
  }
  response <- ap_chat_structured(chat, prompt, type = ap_plotter_schema())
  code <- response$code %||% NULL
  if (is.null(code) || !nzchar(code)) {
    ap_abort("Plotter Agent did not return code.")
  }
  code
}

#' @noRd
#' @keywords internal
ap_execute_plot_script <- function(script_path, plot_path, retries, plan,
                                   data_path, rules, config,
                                   provider, model, api_key,
                                   execution_data = NULL) {
  attempts <- list()
  current_script <- script_path
  max_attempts <- max(1L, as.integer(retries)[1] + 1L)
  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    env <- new.env(parent = globalenv())
    if (!is.null(execution_data)) {
      env$data <- execution_data
    }
    result <- tryCatch(
      {
        sys.source(current_script, envir = env, keep.source = FALSE)
        p <- get("p", envir = env, inherits = FALSE)
        # For ggplot2 plans, auto-save if the script didn't already
        is_ggplot2 <- identical(plan$package, "ggplot2")
        if (is_ggplot2 && inherits(p, "ggplot") && !file.exists(plot_path)) {
          ggplot2::ggsave(plot_path, plot = p,
                          width = plan$output$width %||% 7,
                          height = plan$output$height %||% 5,
                          dpi = plan$output$dpi %||% 300)
        }
        list(ok = TRUE, plot = p, error = NULL)
      },
      error = function(e) list(ok = FALSE, plot = NULL, error = conditionMessage(e))
    )
    attempts[[attempt]] <- list(ok = result$ok, error = result$error)
    if (isTRUE(result$ok)) {
      return(list(plot = result$plot, attempts = attempts))
    }
    last_error <- result$error
    if (attempt < max_attempts) {
      repaired <- ap_plotter_code(
        plan = plan,
        data_path = data_path,
        plot_path = plot_path,
        rules = rules,
        config = config,
        provider = provider,
        model = model,
        api_key = api_key,
        previous_error = last_error
      )
      ap_validate_plot_code(repaired)
      writeLines(repaired, current_script, useBytes = TRUE)
    }
  }
  ap_abort(paste0("Generated plot code failed: ", last_error))
}

#' @noRd
#' @keywords internal
ap_validate_plot_code <- function(code) {
  exprs <- tryCatch(
    parse(text = code, keep.source = FALSE),
    error = function(e) ap_abort(paste0("Generated plot code could not be parsed: ", conditionMessage(e)))
  )
  disallowed <- c(
    "system", "system2", "shell", "pipe",
    "source", "sys.source", "eval", "parse",
    "install.packages", "remove.packages",
    "download.file", "url", "file", "file.remove", "file.rename",
    "file.copy", "unlink", "setwd",
    "load", "save", "saveRDS",
    "readLines", "writeLines", "write.csv", "write.table",
    "assign", "q", "quit"
  )
  found <- unique(unlist(lapply(as.list(exprs), ap_find_calls), use.names = FALSE))
  bad <- intersect(found, disallowed)
  if (length(bad)) {
    ap_abort(paste0("Disallowed call in generated plot code: ", paste(bad, collapse = ", ")))
  }
  invisible(TRUE)
}

#' @noRd
#' @keywords internal
ap_find_calls <- function(expr) {
  if (!is.call(expr)) {
    return(character())
  }
  nm <- ap_call_name(expr)
  unique(c(nm, unlist(lapply(as.list(expr)[-1], ap_find_calls), use.names = FALSE)))
}

#' @noRd
#' @keywords internal
ap_call_name <- function(expr) {
  head <- expr[[1]]
  if (is.symbol(head)) {
    name <- as.character(head)
    if (name %in% c("::", ":::") && length(expr) >= 3L) {
      return(as.character(expr[[3]]))
    }
    return(name)
  }
  character()
}


#' @noRd
#' @keywords internal
ap_parse_transformation <- function(transformation) {
  transformation <- trimws(as.character(transformation)[1])
  if (!nzchar(transformation)) {
    return(NULL)
  }
  assignment <- regexpr("\\s*(<-|(?<![!<>=])=(?!=))\\s*", transformation, perl = TRUE)
  if (assignment < 0L) {
    ap_abort(paste0("Invalid transformation: ", transformation))
  }
  assignment_end <- assignment + attr(assignment, "match.length") - 1L
  name <- trimws(substr(transformation, 1L, assignment - 1L))
  expr <- trimws(substr(transformation, assignment_end + 1L, nchar(transformation)))
  if (!grepl("^[A-Za-z_.][A-Za-z0-9_.]*$", name)) {
    ap_abort(paste0("Invalid transformation output column: ", name))
  }
  if (!nzchar(expr)) {
    ap_abort(paste0("Invalid transformation expression for: ", name))
  }
  list(name = name, expr = expr)
}

#' Print an AutoPlotR plot result
#'
#' @param x An `ap_plot_result`.
#' @param ... Unused.
#' @export
print.ap_plot_result <- function(x, ...) {
  cat("AutoPlotR plot result\n")
  cat("Plot: ", x$plot_path, "\n", sep = "")
  cat("Script: ", x$script_path, "\n", sep = "")
  invisible(x)
}

#' Print an AutoPlotR visualization plan
#'
#' @param x An `ap_viz_plan`.
#' @param ... Unused.
#' @export
print.ap_viz_plan <- function(x, ...) {
  cat("AutoPlotR visualization plan\n")
  cat("Plot type: ", x$plot_type, "\n", sep = "")
  cat("Package: ", x$package, "\n", sep = "")
  cat("Design guide: ", x$design_guide_version, "\n", sep = "")
  invisible(x)
}
