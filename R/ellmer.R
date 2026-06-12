#' @noRd
#' @keywords internal
ap_type_object <- function(props) {
  out <- try(do.call(ellmer::type_object, props), silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  out <- try(ellmer::type_object(props), silent = TRUE)
  if (!inherits(out, "try-error")) return(out)
  ap_abort("Could not create ellmer structured type object.")
}

#' @noRd
#' @keywords internal
ap_read_prompt <- function(path) {
  prompt_path <- ap_package_file(path)
  if (file.exists(prompt_path)) {
    return(ap_read_text(prompt_path))
  }
  switch(
    basename(path),
    "planner.md" = "You are the AutoPlotR Planner Agent. Return only structured visualization plans.",
    "plotter.md" = "You are the AutoPlotR Plotter Agent. Return only structured R plotting code.",
    "You are an AutoPlotR agent."
  )
}

#' @noRd
#' @keywords internal
ap_agent_prompt <- function(agent_id, config) {
  agent_cfg <- config$agents[[agent_id]]
  base <- ap_read_prompt(agent_cfg$system_prompt)
  guide <- ap_load_viz_guide(compact = TRUE)
  paste(base, guide, sep = "\n\n")
}

#' Create an ellmer chat for an AutoPlotR agent
#'
#' @param agent Agent id: `"planner"` or `"plotter"`.
#' @param provider Optional provider override.
#' @param model Optional model override.
#' @param api_key Optional API key for this session.
#' @param config Config path, text, list, or `NULL`.
#' @return An `ellmer` chat object.
#' @examplesIf interactive()
#' chat <- ap_create_chat("planner", provider = "openai", model = "gpt-4o-mini")
#' @export
ap_create_chat <- function(agent = c("planner", "plotter"), provider = NULL,
                           model = NULL, api_key = NULL, config = NULL) {
  agent <- match.arg(agent)
  cfg <- ap_load_config(config)
  agent_cfg <- cfg$agents[[agent]]
  provider <- ap_scalar_chr(provider %||% agent_cfg$provider %||% cfg$llm$default_provider)
  model <- ap_scalar_chr(model %||% agent_cfg$model %||% cfg$llm$default_model)
  if (is.null(provider)) {
    ap_abort("No LLM provider configured. Run `ap_setup()` or pass `provider`.")
  }
  if (is.null(model)) {
    ap_abort("No LLM model configured. Run `ap_setup()` or pass `model`.")
  }

  chat_fun <- ap_provider_chat_function(provider)
  provider_id <- ap_normalize_provider_id(provider)
  key_env <- agent_cfg$api_key_env %||% cfg$llm$api_key_env %||% ap_provider_api_key_env(provider_id)
  key <- api_key %||% Sys.getenv(key_env, unset = "")
  if (!nzchar(key)) key <- NULL

  args <- ap_prepare_chat_args(
    chat_fun = chat_fun,
    agent_args = agent_cfg$args %||% list(),
    system_prompt = ap_agent_prompt(agent, cfg),
    model = model,
    api_key = key
  )
  do.call(chat_fun, args)
}

#' @noRd
#' @keywords internal
ap_prepare_chat_args <- function(chat_fun, agent_args, system_prompt, model, api_key = NULL) {
  args <- agent_args %||% list()
  allowed <- names(formals(chat_fun))
  decoding_args <- c("temperature", "max_tokens", "seed", "top_p")
  if ("params" %in% allowed) {
    params <- args$params %||% list()
    for (nm in intersect(decoding_args, names(args))) {
      params[[nm]] <- args[[nm]]
    }
    args[intersect(decoding_args, names(args))] <- NULL
    args$params <- params
  }
  args$system_prompt <- system_prompt
  args$model <- model
  if ("credentials" %in% allowed && !is.null(api_key)) {
    args$credentials <- function() api_key
  } else if ("api_key" %in% allowed && !is.null(api_key)) {
    args$api_key <- api_key
  }
  if ("..." %in% allowed) {
    return(args)
  }
  args[names(args) %in% allowed]
}

#' @noRd
#' @keywords internal
ap_chat_structured <- function(chat, prompt, type) {
  if (!"chat_structured" %in% names(chat)) {
    ap_abort("The installed ellmer chat object does not support `chat_structured()`.")
  }

  result <- tryCatch(
    chat$chat_structured(prompt, type = type),
    error = function(e) e
  )

  if (!inherits(result, "error")) {
    return(result)
  }

  # Fallback for providers that don't support json_schema structured output
  # (e.g., DeepSeek API returns HTTP 400 "response_format type is unavailable").
  # Use tool calling to extract structured data instead.
  err_msg <- conditionMessage(result)
  if (grepl("400|Bad Request", err_msg, ignore.case = TRUE)) {
    return(ap_chat_structured_via_tool(chat, prompt, type))
  }

  stop(result)
}

#' @noRd
#' @keywords internal
ap_chat_structured_via_tool <- function(chat, prompt, type) {
  # Convert the ellmer TypeObject's properties into tool arguments.
  # ellmer S7 classes use fully qualified names (ellmer::TypeObject, etc.)
  props <- tryCatch(type@properties, error = function(e) NULL)
  if (is.null(props)) {
    ap_abort("Cannot convert structured output type to tool arguments.")
  }

  # Build a handler function whose formals match the argument names.
  # ellmer >= 0.4.0 requires argument names to match fun formals.
  # Use NULL defaults so the handler tolerates omitted optional fields.
  arg_names <- names(props)
  dyn_fun <- ap_make_tool_handler(arg_names, defaults = TRUE)

  tool_name <- "submit_structured_output"
  tool_def <- ap_tool(
    name = tool_name,
    description = paste0(
      "Submit the structured output data. ",
      "You MUST call this tool with exactly the data fields requested."
    ),
    args_props = props,
    fun = dyn_fun
  )

  ap_add_tool(chat, tool_def)

  # Send prompt; model should call the tool in response.
  chat$chat(prompt)

  # Extract tool call arguments from the last assistant turn.
  # S7 class names include the namespace prefix.
  turns <- chat$get_turns()
  for (i in rev(seq_along(turns))) {
    turn <- turns[[i]]
    if (is.null(turn) || !inherits(turn, "ellmer::AssistantTurn")) next

    contents <- tryCatch(turn@contents, error = function(e) NULL)
    if (is.null(contents)) next
    for (content in contents) {
      if (is.null(content)) next
      cname <- tryCatch(content@name, error = function(e) NULL)
      if (!inherits(content, "ellmer::ContentToolRequest") ||
          !identical(cname, tool_name)) next
      cargs <- tryCatch(content@arguments, error = function(e) NULL)
      if (!is.null(cargs)) return(cargs)
    }
  }

  ap_abort(paste0(
    "Tool-based structured output failed: model did not call the required tool. ",
    "This may indicate an ellmer version mismatch. ",
    "Installed ellmer: ", as.character(utils::packageVersion("ellmer"))
  ))
}

# Build a function with named formals matching the argument list.
# ellmer >= 0.4.0 validates that tool argument names match handler formals.
# When defaults = TRUE, all arguments default to NULL so the handler tolerates
# omitted optional fields.
#' @noRd
#' @keywords internal
ap_make_tool_handler <- function(arg_names, defaults = FALSE) {
  body <- quote(list())
  for (nm in arg_names) {
    body[[nm]] <- as.name(nm)
  }
  default_val <- if (isTRUE(defaults)) quote(NULL) else quote(expr = )
  formals_list <- stats::setNames(
    lapply(arg_names, function(x) default_val),
    arg_names
  )
  as.function(c(formals_list, body))
}

# Plan schema definition.
# KEEP IN SYNC with ap_normalize_plan() in R/planner.R.
# Every top-level field added here MUST have a default or %||% list() fallback there.
#' @noRd
#' @keywords internal
ap_plan_schema <- function() {
  ap_type_object(list(
    plot_type = ellmer::type_string("Plot type, e.g. scatter, line, bar, histogram"),
    package = ellmer::type_string("R plotting package, usually ggplot2"),
    mappings = ap_type_object(list(
      x = ellmer::type_string("x variable", required = FALSE),
      y = ellmer::type_string("y variable", required = FALSE),
      color = ellmer::type_string("color variable", required = FALSE),
      fill = ellmer::type_string("fill variable", required = FALSE),
      group = ellmer::type_string("group variable", required = FALSE)
    )),
    transformations = ellmer::type_array(
      "Transformations or summaries used",
      items = ellmer::type_string("transformation"),
      required = FALSE
    ),
    facets = ap_type_object(list(
      wrap = ellmer::type_string("facet wrap variable", required = FALSE)
    )),
    labels = ap_type_object(list(
      title = ellmer::type_string("plot title", required = FALSE),
      subtitle = ellmer::type_string("plot subtitle", required = FALSE),
      x = ellmer::type_string("x label", required = FALSE),
      y = ellmer::type_string("y label", required = FALSE),
      color = ellmer::type_string("color legend title", required = FALSE),
      fill = ellmer::type_string("fill legend title", required = FALSE)
    )),
    theme = ap_type_object(list(
      base_size = ellmer::type_number("base font size", required = FALSE)
    )),
    accessibility = ap_type_object(list(
      palette = ellmer::type_string("palette name", required = FALSE),
      colorblind_safe = ellmer::type_boolean("whether palette is colorblind safe", required = FALSE),
      alt_text = ellmer::type_string("short alt text", required = FALSE)
    )),
    output = ap_type_object(list(
      width = ellmer::type_number("plot width in inches", required = FALSE),
      height = ellmer::type_number("plot height in inches", required = FALSE),
      dpi = ellmer::type_number("plot resolution", required = FALSE),
      filename = ellmer::type_string("output filename", required = FALSE)
    )),
    design_rationale = ellmer::type_string("short explanation of visualization choices", required = FALSE),
    clarification_questions = ellmer::type_array(
      "Short clarification questions",
      items = ellmer::type_string("question"),
      required = FALSE
    ),
    confidence = ellmer::type_number("confidence from 0 to 1", required = FALSE)
  ))
}

#' @noRd
#' @keywords internal
ap_plotter_schema <- function() {
  ap_type_object(list(
    code = ellmer::type_string("Complete R plotting script"),
    required_packages = ellmer::type_array(
      "Required R packages",
      items = ellmer::type_string("package"),
      required = FALSE
    ),
    notes = ellmer::type_array(
      "Short implementation notes",
      items = ellmer::type_string("note"),
      required = FALSE
    )
  ))
}

# Version-tolerant ellmer tool helpers ----------------------------------------

#' Create an ellmer tool in a version-tolerant way
#'
#' Tries multiple API signatures to support ellmer >= 0.3.0 through >= 0.4.0.
#' @param name Tool name.
#' @param description Tool description.
#' @param args_props Named list of ellmer type_*() argument definitions.
#' @param fun The handler function.
#' @return An ellmer tool definition.
#' @noRd
#' @keywords internal
ap_tool <- function(name, description, args_props, fun) {
  args_list <- args_props %||% list()
  args_typed <- ap_type_object(args_props %||% list())

  worked <- function(x) !inherits(x, "try-error") && !is.null(x)

  # ellmer >= 0.4.0: arguments = named list of types
  res <- try(ellmer::tool(
    fun = fun, description = description,
    name = name, arguments = args_list
  ), silent = TRUE)
  if (worked(res)) return(res)

  # ellmer >= 0.3.0 < 0.4.0: arguments = type_object(...)
  res <- try(ellmer::tool(
    fun = fun, description = description,
    name = name, arguments = args_typed
  ), silent = TRUE)
  if (worked(res)) return(res)

  # ellmer < 0.3.0: types passed as ... to tool()
  res <- try(do.call(
    ellmer::tool,
    c(list(fun = fun, description = description, name = name), args_list)
  ), silent = TRUE)
  if (worked(res)) return(res)

  # Fallback: no args type
  res <- try(ellmer::tool(
    fun = fun, description = description, name = name
  ), silent = TRUE)
  if (worked(res)) return(res)

  # Old API: handler + parameters
  res <- try(ellmer::tool(
    name = name, description = description,
    parameters = args_typed, handler = fun
  ), silent = TRUE)
  if (worked(res)) return(res)

  res <- try(ellmer::tool(
    name = name, description = description, handler = fun
  ), silent = TRUE)
  if (worked(res)) return(res)

  ap_abort(paste0(
    "Failed to register tool '", name, "' with ellmer ",
    utils::packageVersion("ellmer")
  ))
}

#' Add a tool to an ellmer chat (version-tolerant)
#' @param chat An ellmer chat object.
#' @param tool_def A tool definition from ap_tool().
#' @return The chat, invisibly.
#' @noRd
#' @keywords internal
ap_add_tool <- function(chat, tool_def) {
  ok <- try({ if ("register_tool" %in% names(chat)) chat$register_tool(tool_def) }, silent = TRUE)
  if (!inherits(ok, "try-error")) return(invisible(chat))

  ok2 <- try({ if ("register_tools" %in% names(chat)) chat$register_tools(list(tool_def)) }, silent = TRUE)
  if (!inherits(ok2, "try-error")) return(invisible(chat))

  if (exists("add_tool", where = asNamespace("ellmer"), inherits = FALSE)) {
    ok3 <- try(get("add_tool", envir = asNamespace("ellmer"))(chat, tool_def), silent = TRUE)
    if (!inherits(ok3, "try-error")) return(invisible(chat))
  }

  ap_abort("Failed to add tool to chat with current ellmer version.")
}

#' Launch an ellmer live UI
#'
#' Browser mode runs the chat in a browser. In-process browser mode still
#' blocks the R thread (Shiny occupies it). For truly non-blocking chat,
#' use `ap_live(launch_browser = TRUE)` which spawns a background process.
#'
#' @param chat An ellmer chat object.
#' @param launch_browser Prefer browser mode over console.
#' @return The chat, invisibly.
#' @noRd
#' @keywords internal
ap_launch_live_ui <- function(chat, launch_browser = interactive()) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    ap_abort("Package 'ellmer' is required to run the live agent.")
  }

  errors <- character()

  # Browser mode
  if (isTRUE(launch_browser)) {
    if (!requireNamespace("shiny", quietly = TRUE)) {
      ap_abort(
        "Package 'shiny' is required for browser-based live chat. ",
        "Install it with install.packages('shiny'), or use launch_browser = FALSE for terminal mode."
      )
    }

    if (exists("live_browser", where = asNamespace("ellmer"), inherits = FALSE)) {
      live_browser <- get("live_browser", envir = asNamespace("ellmer"))
      res <- tryCatch(live_browser(chat), error = function(e) e)
      if (!inherits(res, "error")) return(invisible(chat))
      errors <- c(errors, paste0("live_browser(): ", conditionMessage(res)))

      res2 <- tryCatch(live_browser(chat, quiet = TRUE), error = function(e) e)
      if (!inherits(res2, "error")) return(invisible(chat))
      errors <- c(errors, paste0("live_browser(quiet=TRUE): ", conditionMessage(res2)))
    } else {
      errors <- c(errors, "live_browser() not found in installed ellmer version")
    }
  }

  # Console mode
  if (exists("live_console", where = asNamespace("ellmer"), inherits = FALSE)) {
    res_console <- tryCatch(
      get("live_console", envir = asNamespace("ellmer"))(chat),
      error = function(e) e
    )
    if (!inherits(res_console, "error")) return(invisible(chat))
    errors <- c(errors, paste0("live_console(): ", conditionMessage(res_console)))
  } else {
    errors <- c(errors, "live_console() not found in installed ellmer version")
  }

  ap_abort(c(
    "Failed to start the live UI.",
    "i" = paste("ellmer version:", as.character(utils::packageVersion("ellmer"))),
    "i" = paste("Shiny installed:", as.character(requireNamespace("shiny", quietly = TRUE))),
    "i" = paste("RStudio available:", as.character(
      requireNamespace("rstudioapi", quietly = TRUE) &&
        isTRUE(try(rstudioapi::isAvailable(), silent = TRUE))
    )),
    "x" = paste("Errors:", paste(errors, collapse = "; "))
  ))
}

#' Add an initial assistant message to an ellmer chat (best-effort)
#' @param chat An ellmer chat object.
#' @param content Message content.
#' @return The chat, invisibly.
#' @noRd
#' @keywords internal
ap_add_assistant_message <- function(chat, content) {
  if ("add_turn" %in% names(chat) && requireNamespace("ellmer", quietly = TRUE)) {
    ok <- try({
      chat$add_turn(
        ellmer::UserTurn(contents = list(ellmer::ContentText(" "))),
        ellmer::AssistantTurn(contents = list(ellmer::ContentText(content))),
        log_tokens = FALSE
      )
    }, silent = TRUE)
    if (!inherits(ok, "try-error")) return(invisible(chat))
  }

  if ("set_turns" %in% names(chat) && "get_turns" %in% names(chat) && requireNamespace("ellmer", quietly = TRUE)) {
    ok <- try({
      turns <- chat$get_turns()
      turns <- c(turns, list(ellmer::AssistantTurn(contents = list(ellmer::ContentText(content)))))
      chat$set_turns(turns)
    }, silent = TRUE)
    if (!inherits(ok, "try-error")) return(invisible(chat))
  }

  invisible(chat)
}
