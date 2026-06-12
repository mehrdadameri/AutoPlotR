ap_rule_cache <- new.env(parent = emptyenv())

#' Return the visualization rule path
#'
#' @param scope One of `"package"`, `"user"`, or `"project"`.
#' @return Path to a visualization rules file.
#' @examples
#' ap_viz_rules_path()
#' @export
ap_viz_rules_path <- function(scope = c("package", "user", "project")) {
  scope <- match.arg(scope)
  switch(
    scope,
    package = ap_package_file("knowledge", "visualization-rules.yml"),
    user = {
      path <- ap_config_path("visualization-rules.yml")
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      path
    },
    project = file.path(getwd(), "autoplotr-viz-rules.yml")
  )
}

#' @noRd
#' @keywords internal
ap_viz_guide_path <- function() {
  ap_package_file("knowledge", "visualization-design.md")
}

#' Load the AutoPlotR visualization design guide
#'
#' @param compact Whether to return the compact prompt-ready portion.
#' @return A character scalar.
#' @examples
#' guide <- ap_load_viz_guide()
#' substr(guide, 1, 40)
#' @export
ap_load_viz_guide <- function(compact = TRUE) {
  path <- ap_viz_guide_path()
  if (!file.exists(path)) {
    ap_abort("Visualization design guide was not found in the package.")
  }
  guide <- ap_read_text(path)
  if (!isTRUE(compact)) {
    return(guide)
  }
  lines <- strsplit(guide, "\n", fixed = TRUE)[[1]]
  keep <- seq_len(min(length(lines), 40L))
  paste(lines[keep], collapse = "\n")
}

#' @noRd
#' @keywords internal
ap_read_rules_file <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  rules <- yaml::read_yaml(path)
  if (is.null(rules)) list() else rules
}

#' @noRd
#' @keywords internal
ap_load_all_viz_rules <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && exists("rules", envir = ap_rule_cache, inherits = FALSE)) {
    return(get("rules", envir = ap_rule_cache, inherits = FALSE))
  }
  package_rules <- ap_read_rules_file(ap_viz_rules_path("package"))
  user_rules <- ap_read_rules_file(ap_viz_rules_path("user"))
  project_rules <- ap_read_rules_file(ap_viz_rules_path("project"))
  rules <- ap_deep_merge(package_rules, user_rules)
  rules <- ap_deep_merge(rules, project_rules)
  assign("rules", rules, envir = ap_rule_cache)
  rules
}

#' Load visualization rules
#'
#' @param plot_type Optional plot type such as `"scatter"` or `"histogram"`.
#' @param refresh Whether to refresh the internal rules cache.
#' @return A list of visualization rules.
#' @examples
#' names(ap_load_viz_rules())
#' ap_load_viz_rules("scatter")$plot_type
#' @export
ap_load_viz_rules <- function(plot_type = NULL, refresh = FALSE) {
  rules <- ap_load_all_viz_rules(refresh = refresh)
  plot_type <- ap_scalar_chr(plot_type)
  if (is.null(plot_type)) {
    return(rules)
  }
  plot_type <- tolower(plot_type)
  plot_rules <- rules$plot_types[[plot_type]]
  if (is.null(plot_rules)) {
    ap_abort(paste0("Unknown plot type in visualization rules: ", plot_type))
  }
  list(
    version = rules$version,
    core = rules$core,
    defaults = rules$defaults,
    plot_type = plot_type,
    rules = plot_rules
  )
}

#' @noRd
#' @keywords internal
ap_rules_for_prompt <- function(plot_type = NULL) {
  rules <- ap_load_viz_rules(plot_type = plot_type)
  ap_chr_json(rules)
}
