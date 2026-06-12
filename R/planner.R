#' Build the Planner Agent prompt
#'
#' @param data_profile Data profile from [ap_profile_data()].
#' @param request User plotting request.
#' @param rules Visualization rules from [ap_load_viz_rules()].
#' @return A prompt string.
#' @examples
#' profile <- ap_profile_data(mtcars, max_sample_rows = 2)
#' prompt <- ap_build_planner_prompt(profile, "scatter plot of mpg by wt")
#' substr(prompt, 1, 60)
#' @export
ap_build_planner_prompt <- function(data_profile, request, rules = ap_load_viz_rules()) {
  paste(
    "AutoPlotR Planner Agent input.",
    "",
    ap_load_viz_guide(compact = TRUE),
    "",
    "Visualization rules:",
    ap_chr_json(rules),
    "",
    "Data profile:",
    ap_chr_json(data_profile),
    "",
    "User request:",
    as.character(request)[1],
    "",
    "IMPORTANT - Handling computed columns:",
    "If the visualization requires columns that don't exist in the data",
    "(e.g., -log10(padj), log(mpg), significance categories, binned values,",
    "ratios, or any derived quantity), you MUST populate the 'transformations'",
    "field with R code snippets that create those columns.",
    "",
    "Each transformation string should be a complete R assignment, e.g.:",
    '  "neg_log10_padj = -log10(padj)"',
    '  "log_mpg = log(mpg)"',
    '  "significance = ifelse(padj < 0.05 & abs(log2FoldChange) > 1, \\"Significant\\", \\"Not Significant\\")"',
    '  "wt_kg = wt * 0.453592"',
    "",
    "The mapping fields (x, y, color, fill, group) should reference the",
    "OUTPUT column name (the left-hand side) of the transformation.",
    "Only use raw column names from the data profile if no transformation",
    "is needed. The plotter agent will run your transformation code",
    "to create the new columns before plotting.",
    "Do not invent columns, units, thresholds, statistical tests, or group meanings.",
    "State assumptions in design_rationale when choosing a standard interpretation.",
    "",
    "Return only structured data matching the provided schema.",
    "Use readable labels and record the design guide version in the final plan.",
    sep = "\n"
  )
}

#' @noRd
#' @keywords internal
ap_build_plotter_prompt <- function(plan, data_path, plot_path, rules) {
  # Build transformation instructions if the plan has computed columns
  tx <- plan$transformations
  tx_block <- if (length(tx) > 0L && any(nzchar(as.character(tx)))) {
    paste(
      "",
      "Data preprocessing - computed columns:",
      "The following columns do NOT exist in the raw data. You MUST create",
      "them before building the ggplot by running these R assignments on the",
      "data frame:",
      "",
      paste("  data$", as.character(tx), sep = "", collapse = "\n"),
      "",
      "Wrap the full workflow:",
      "  data <- readRDS(path)",
      "  # Apply the transformations above to create computed columns",
      "  # Build ggplot using the augmented data",
      "  p <- ggplot(data, ...)",
      sep = "\n"
    )
  } else {
    ""
  }

  paste(
    "AutoPlotR Plotter Agent input.",
    "",
    ap_load_viz_guide(compact = TRUE),
    "",
    "Visualization rules:",
    ap_chr_json(rules),
    "",
    "Validated visualization plan:",
    ap_chr_json(plan),
    tx_block,
    "",
    "Code constraints:",
    "- Read the data from this RDS path:",
    data_path,
    "- Validate required columns before plotting.",
    "- Create a ggplot object named p.",
    "- Save the plot to this path using ggplot2::ggsave():",
    plot_path,
    "- Do not install packages, download data, delete files, change the working directory, or call shell/system functions.",
    "- Return complete R code only in the structured code field.",
    sep = "\n"
  )
}

#' Generate a design-aware visualization plan
#'
#' @param data A data frame.
#' @param request Natural language plotting request.
#' @param config Optional AutoPlotR config.
#' @param max_sample_rows Maximum sample rows in the data profile.
#' @param max_columns Maximum columns in the data profile.
#' @param provider Optional provider override.
#' @param model Optional model override.
#' @param api_key Optional API key override.
#' @return An `ap_viz_plan` list.
#' @examplesIf interactive()
#' ap_plan(mtcars, "scatter plot of mpg by wt")
#' @export
ap_plan <- function(data, request, config = NULL, max_sample_rows = 10,
                    max_columns = Inf,
                    provider = NULL, model = NULL, api_key = NULL) {
  profile <- ap_profile_data(data, max_sample_rows = max_sample_rows, max_columns = max_columns)
  rules <- ap_load_viz_rules(refresh = FALSE)
  prompt <- ap_build_planner_prompt(profile, request, rules)

  chat <- NULL
  last_error <- NULL
  for (attempt in seq_len(2L)) {
    raw_plan <- ap_next_planner_response(
      prompt = if (is.null(last_error)) prompt else paste(prompt, "Previous validation error:", last_error, sep = "\n\n"),
      chat = chat,
      provider = provider,
      model = model,
      api_key = api_key,
      config = config
    )
    plan <- ap_normalize_plan(raw_plan, profile = profile, request = request, rules = rules)
    ok <- tryCatch(
      {
        ap_validate_plan(plan, profile)
        TRUE
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        FALSE
      }
    )
    if (isTRUE(ok)) {
      return(plan)
    }
  }
  ap_abort(paste0("Planner response failed validation: ", last_error))
}

#' @noRd
#' @keywords internal
ap_next_planner_response <- function(prompt, chat = NULL, provider = NULL,
                                     model = NULL, api_key = NULL, config = NULL) {
  mocks <- getOption("AutoPlotR.mock_planner_responses", NULL)
  if (!is.null(mocks)) {
    idx <- as.integer(getOption("AutoPlotR.mock_planner_response_index", 0L)) + 1L
    options(AutoPlotR.mock_planner_response_index = idx)
    if (idx > length(mocks)) {
      ap_abort("No mock planner response available for retry.")
    }
    return(mocks[[idx]])
  }
  mock <- getOption("AutoPlotR.mock_planner_response", NULL)
  if (!is.null(mock)) {
    return(mock)
  }
  if (is.null(chat)) {
    chat <- ap_create_chat("planner", provider = provider, model = model, api_key = api_key, config = config)
  }
  ap_chat_structured(chat, prompt, type = ap_plan_schema())
}

# Normalize and apply defaults to a planner response.
# KEEP IN SYNC with ap_plan_schema() in R/ellmer.R.
# Every top-level schema field must have a default or %||% list() fallback here.
#' @noRd
#' @keywords internal
ap_normalize_plan <- function(plan, profile, request, rules) {
  if (!is.list(plan)) {
    ap_abort("Planner response must be a list.")
  }
  defaults <- rules$defaults %||% list()
  plan$plot_type <- tolower(ap_scalar_chr(plan$plot_type) %||% "scatter")
  plan$package <- ap_scalar_chr(plan$package) %||% defaults$package %||% "ggplot2"
  plan$mappings <- plan$mappings %||% list()
  plan$transformations <- plan$transformations %||% list()
  plan$facets <- plan$facets %||% list()
  plan$labels <- plan$labels %||% list()
  plan$theme <- plan$theme %||% list()
  plan$accessibility <- plan$accessibility %||% list()
  plan$output <- plan$output %||% list()
  plan$output$width <- as.numeric(plan$output$width %||% defaults$width %||% 7)
  plan$output$height <- as.numeric(plan$output$height %||% defaults$height %||% 5)
  plan$output$dpi <- as.integer(plan$output$dpi %||% defaults$dpi %||% 300)
  plan$output$filename <- ap_scalar_chr(plan$output$filename) %||% "autoplotr-plot.png"
  plan$theme$base_size <- as.numeric(plan$theme$base_size %||% defaults$base_size %||% 12)
  plan$accessibility$palette <- ap_scalar_chr(plan$accessibility$palette) %||% defaults$palette %||% "okabe_ito"
  plan$accessibility$colorblind_safe <- isTRUE(plan$accessibility$colorblind_safe %||% TRUE)
  plan$clarification_questions <- plan$clarification_questions %||% character()
  plan$confidence <- as.numeric(plan$confidence %||% NA_real_)
  plan$request <- as.character(request)[1]
  plan$design_guide_version <- rules$version %||% "unknown"
  plan$data_columns <- profile$column_names
  class(plan) <- c("ap_viz_plan", "list")
  plan
}

#' @noRd
#' @keywords internal
ap_validate_plan <- function(plan, profile) {
  rules <- ap_load_viz_rules(refresh = FALSE)

  # Package-aware validation: for known packages (ggplot2), enforce plot-type
  # rules from the viz rules YAML. For unknown packages, skip plot-type checks
  # but still validate column references.
  is_known_package <- identical(plan$package, "ggplot2")

  if (is_known_package) {
    supported <- names(rules$plot_types)
    if (!plan$plot_type %in% supported) {
      ap_abort(paste0("Unsupported plot type: ", plan$plot_type))
    }
  }

  # Collect computed column names from transformations.
  # Transformation strings like "new_col = f(old_col)" define new column names
  # that won't exist in the raw data but will be created by the plotter.
  computed_cols <- ap_parse_transformation_columns(plan$transformations)
  known_cols <- union(profile$column_names, computed_cols)

  mapped <- unlist(plan$mappings, use.names = TRUE)
  mapped <- mapped[!is.na(mapped) & nzchar(as.character(mapped))]
  unknown <- setdiff(as.character(mapped), known_cols)
  if (length(unknown)) {
    ap_abort(paste0("Unknown variable in plan: ", paste(unknown, collapse = ", ")))
  }
  facet_vars <- unlist(plan$facets, use.names = FALSE)
  facet_vars <- facet_vars[!is.na(facet_vars) & nzchar(as.character(facet_vars))]
  unknown_facets <- setdiff(as.character(facet_vars), known_cols)
  if (length(unknown_facets)) {
    ap_abort(paste0("Unknown variable in facets: ", paste(unknown_facets, collapse = ", ")))
  }
  if (is_known_package) {
    ap_validate_plot_type_variables(plan, profile, computed_cols)
  }
  invisible(TRUE)
}

# Parse transformation strings to extract the output column name (LHS of = or <-).
# Example inputs: "neg_log10_padj = -log10(padj)", "sig <- ifelse(...)"
# Returns a character vector of computed column names.
#' @noRd
#' @keywords internal
ap_parse_transformation_columns <- function(transformations) {
  if (is.null(transformations) || length(transformations) == 0L) {
    return(character())
  }
  cols <- character()
  for (t in as.character(transformations)) {
    if (!nzchar(trimws(t))) next
    parsed <- ap_parse_transformation(t)
    lhs <- if (is.null(parsed)) "" else parsed$name
    if (nzchar(lhs) && grepl("^[A-Za-z_.][A-Za-z0-9_.]*$", lhs)) {
      cols <- c(cols, lhs)
    }
  }
  unique(cols)
}

#' @noRd
#' @keywords internal
ap_validate_plot_type_variables <- function(plan, profile, computed_cols = character()) {
  m <- plan$mappings
  type <- function(var) ap_profile_column_type(profile, var)
  numeric_like <- function(var) isTRUE(type(var) %in% c("numeric", "datetime"))
  numeric_only <- function(var) isTRUE(type(var) %in% "numeric")
  categorical_like <- function(var) isTRUE(type(var) %in% c("categorical", "logical", "text"))

  # When a mapping variable is computed (from transformations), we can't
  # predict its post-transformation type. Also skip when the variable has no
  # type info in the profile (e.g., mock data or missing column_profiles).
  skip <- function(var) var %in% computed_cols || is.null(type(var))

  switch(
    plan$plot_type,
    scatter = {
      if (is.null(m$x) || is.null(m$y)) {
        ap_abort("Scatter plans require x and y variables.")
      }
      if (!skip(m$x) && !numeric_like(m$x)) {
        ap_abort("Scatter x must be numeric or datetime.")
      }
      if (!skip(m$y) && !numeric_only(m$y)) {
        ap_abort("Scatter y must be numeric.")
      }
    },
    line = {
      if (is.null(m$x) || is.null(m$y)) {
        ap_abort("Line plans require x and y variables.")
      }
      if (!skip(m$x) && !numeric_like(m$x)) {
        ap_abort("Line x must be numeric or datetime.")
      }
      if (!skip(m$y) && !numeric_only(m$y)) {
        ap_abort("Line y must be numeric.")
      }
    },
    histogram = {
      if (is.null(m$x)) ap_abort("Histogram plans require numeric x.")
      if (!skip(m$x) && !numeric_only(m$x)) ap_abort("Histogram x must be numeric.")
    },
    density = {
      if (is.null(m$x)) ap_abort("Density plans require numeric x.")
      if (!skip(m$x) && !numeric_only(m$x)) ap_abort("Density x must be numeric.")
    },
    bar = {
      if (is.null(m$x)) ap_abort("Bar plans require categorical x.")
      if (!skip(m$x) && !categorical_like(m$x)) ap_abort("Bar x must be categorical.")
    },
    boxplot = {
      if (is.null(m$x) || is.null(m$y)) ap_abort("Boxplot plans require x and y variables.")
      if (!skip(m$x) && !categorical_like(m$x)) ap_abort("Boxplot x must be categorical.")
      if (!skip(m$y) && !numeric_only(m$y)) ap_abort("Boxplot y must be numeric.")
    },
    violin = {
      if (is.null(m$x) || is.null(m$y)) ap_abort("Violin plans require x and y variables.")
      if (!skip(m$x) && !categorical_like(m$x)) ap_abort("Violin x must be categorical.")
      if (!skip(m$y) && !numeric_only(m$y)) ap_abort("Violin y must be numeric.")
    },
    tile = if (is.null(m$x) || is.null(m$y) || is.null(m$fill)) ap_abort("Tile plans require x, y, and fill mappings."),
    heatmap = if (is.null(m$x) || is.null(m$y) || is.null(m$fill)) ap_abort("Heatmap plans require x, y, and fill mappings."),
    invisible(TRUE)
  )
  invisible(TRUE)
}
