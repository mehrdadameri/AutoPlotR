#' @noRd
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

#' @noRd
#' @keywords internal
ap_abort <- function(message, class = "autoplotr_error") {
  rlang::abort(message, class = class)
}

#' @noRd
#' @keywords internal
ap_scalar_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NULL)
  }
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(trimws(x))) {
    return(NULL)
  }
  trimws(x)
}

#' @noRd
#' @keywords internal
ap_deep_merge <- function(base, override, depth = 0L) {
  if (depth > 20L) {
    ap_abort("Deep merge exceeded maximum depth. Possible circular reference in config.")
  }
  if (!is.list(base) || !is.list(override)) {
    return(override)
  }
  out <- base
  for (nm in names(override)) {
    if (nm %in% names(out) && is.list(out[[nm]]) && is.list(override[[nm]])) {
      out[[nm]] <- ap_deep_merge(out[[nm]], override[[nm]], depth = depth + 1L)
    } else {
      out[[nm]] <- override[[nm]]
    }
  }
  out
}

#' @noRd
#' @keywords internal
ap_package_file <- function(...) {
  installed <- system.file(..., package = "AutoPlotR")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }
  source_path <- file.path(getwd(), "inst", ...)
  if (file.exists(source_path)) {
    return(normalizePath(source_path, winslash = "/", mustWork = FALSE))
  }
  source_path
}

#' @noRd
#' @keywords internal
ap_write_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

#' @noRd
#' @keywords internal
ap_read_text <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

#' @noRd
#' @keywords internal
ap_safe_filename <- function(x, fallback = "autoplotr-plot") {
  x <- ap_scalar_chr(x) %||% fallback
  x <- gsub("[^A-Za-z0-9_-]+", "-", x)
  x <- gsub("(^-+|-+$)", "", x)
  if (!nzchar(x)) fallback else tolower(x)
}

#' @noRd
#' @keywords internal
ap_chr_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

#' @noRd
#' @keywords internal
ap_hash_file <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  unname(tools::md5sum(path))
}

#' @noRd
#' @keywords internal
ap_hash_text <- function(text) {
  path <- tempfile("autoplotr-hash-")
  on.exit(unlink(path), add = TRUE)
  writeLines(as.character(text %||% ""), path, useBytes = TRUE)
  ap_hash_file(path)
}

#' @noRd
#' @keywords internal
ap_hash_object <- function(x) {
  path <- tempfile("autoplotr-hash-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path)
  ap_hash_file(path)
}

#' @noRd
#' @keywords internal
ap_run_id <- function() {
  paste0(
    "run_",
    format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"),
    "_",
    substr(ap_hash_text(paste(Sys.time(), tempfile(), Sys.getpid())), 1, 8)
  )
}

#' @noRd
#' @keywords internal
ap_trace_event <- function(path, event, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  entry <- c(
    list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      event = event
    ),
    list(...)
  )
  cat(
    jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null"),
    "\n",
    file = path,
    append = TRUE,
    sep = ""
  )
  invisible(path)
}

#' @noRd
#' @keywords internal
ap_package_versions <- function(packages = c("AutoPlotR", "ellmer", "ggplot2", "jsonlite", "yaml")) {
  out <- list()
  for (pkg in packages) {
    out[[pkg]] <- if (requireNamespace(pkg, quietly = TRUE)) {
      as.character(utils::packageVersion(pkg))
    } else {
      NULL
    }
  }
  out
}

#' @noRd
#' @keywords internal
ap_is_data_frame <- function(x) {
  is.data.frame(x) || inherits(x, "tbl_df")
}

#' @noRd
#' @keywords internal
ap_label_for <- function(x) {
  x <- gsub("[_.]+", " ", x)
  paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = "")
}

#' @noRd
#' @keywords internal
ap_escape_r_string <- function(x) {
  paste0("\"", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", as.character(x))), "\"")
}
