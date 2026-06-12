#' Profile a data frame for AutoPlotR agents
#'
#' @param data A data frame.
#' @param max_sample_rows Maximum sample rows to include.
#' @param max_columns Maximum columns to include in the prompt-facing profile.
#' @return A compact list describing the data.
#' @examples
#' profile <- ap_profile_data(mtcars, max_sample_rows = 2)
#' profile$rows
#' @export
ap_profile_data <- function(data, max_sample_rows = 10, max_columns = Inf) {
  if (!ap_is_data_frame(data)) {
    ap_abort("`data` must be a data frame.")
  }
  max_sample_rows <- max(0L, as.integer(max_sample_rows)[1])
  max_columns <- suppressWarnings(as.integer(max_columns)[1])
  if (is.na(max_columns) || max_columns < 0L) {
    max_columns <- Inf
  }
  data <- as.data.frame(data)
  all_column_names <- names(data)
  kept_names <- utils::head(all_column_names, max_columns)
  omitted_names <- setdiff(all_column_names, kept_names)
  columns <- lapply(kept_names, function(nm) ap_profile_column(data[[nm]], nm))
  names(columns) <- kept_names
  sample_rows <- utils::head(data[kept_names], max_sample_rows)
  sample_rows <- lapply(sample_rows, function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      as.character(x)
    } else {
      x
    }
  })
  list(
    rows = nrow(data),
    columns = ncol(data),
    profiled_columns = length(kept_names),
    column_names = kept_names,
    omitted_columns = omitted_names,
    column_profiles = columns,
    sample_rows = sample_rows
  )
}

#' @noRd
#' @keywords internal
ap_profile_column <- function(x, name) {
  non_missing <- x[!is.na(x)]
  type <- ap_column_type(x)
  sample_values <- unique(utils::head(as.character(non_missing), 8L))
  out <- list(
    name = name,
    class = class(x),
    type = type,
    missing = sum(is.na(x)),
    distinct = length(unique(non_missing)),
    sample_values = sample_values
  )
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    rng <- range(x, na.rm = TRUE)
    out$range <- as.character(rng)
  } else if (is.numeric(x) && length(non_missing)) {
    out$range <- as.numeric(range(x, na.rm = TRUE))
  }
  out
}

#' @noRd
#' @keywords internal
ap_column_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("datetime")
  if (is.numeric(x) || is.integer(x)) return("numeric")
  if (is.logical(x)) return("logical")
  if (is.factor(x)) return("categorical")
  distinct <- length(unique(x[!is.na(x)]))
  if (is.character(x) && distinct <= max(20L, floor(length(x) * 0.2))) {
    return("categorical")
  }
  if (is.character(x)) return("text")
  "unknown"
}

#' @noRd
#' @keywords internal
ap_profile_column_type <- function(profile, variable) {
  col <- profile$column_profiles[[variable]]
  if (is.null(col)) NULL else col$type
}
