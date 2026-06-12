# Package startup hooks ---------------------------------------------------------

# Registered in .onLoad so the method is available even if ellmer is loaded after.
.ap_ellmer_methods_registered <- FALSE

#' @noRd
#' @keywords internal
.ap_register_ellmer_methods <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(invisible())
  if (!requireNamespace("S7", quietly = TRUE)) return(invisible())
  if (.ap_ellmer_methods_registered) return(invisible())

  tryCatch(
    {
      # ellmer's as_json generic is NOT exported. Access it via the namespace.
      # All classes used in signatures ARE exported (ellmer::Provider,
      # S7::class_list, S7::class_any).
      as_json_gen <- get("as_json", envir = asNamespace("ellmer"), inherits = FALSE)

      # Override 1: null-safe class_list iteration.
      # The original method does compact(lapply(x, as_json, ...)). If list x
      # contains a NULL element, lapply calls as_json(provider, NULL) which
      # fails S7 dispatch. This override skips NULL elements before dispatch.
      suppressMessages({
        S7::method(
          as_json_gen,
          list(ellmer::Provider, S7::class_list)
        ) <- function(provider, x, ...) {
          out <- lapply(x, function(el) {
            if (is.null(el)) return(NULL)
            as_json_gen(provider, el, ...)
          })
          out[!vapply(out, is.null, logical(1))]
        }
      })

      # Override 2: catch-all for NULL x and unknown content types.
      # DeepSeek's as_json(Turn) handler returns NULL for empty assistant
      # turns. This NULL can reach dispatch through other code paths not
      # covered by the class_list override. Without this, S7 errors with
      # "Can't find method for generic as_json(provider, x)".
      suppressMessages({
        S7::method(
          as_json_gen,
          list(ellmer::Provider, S7::class_any)
        ) <- function(provider, x, ...) {
          if (is.null(x)) {
            return(NULL)
          }
          tryCatch(
            jsonlite::unbox(paste0("[unserializable: ", class(x)[1], "]")),
            error = function(e) NULL
          )
        }
      })

      .ap_ellmer_methods_registered <<- TRUE
    },
    error = function(e) {
      packageStartupMessage("AutoPlotR: could not register ellmer S7 methods: ",
                            conditionMessage(e))
    }
  )

  invisible()
}

#' @noRd
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  .ap_register_ellmer_methods()
  invisible()
}

#' @noRd
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  if (requireNamespace("ellmer", quietly = TRUE)) {
    ver <- tryCatch(
      as.character(utils::packageVersion("ellmer")),
      error = function(e) NULL
    )
    if (!is.null(ver)) {
      # Known-compatible ellmer version ranges.
      # AutoPlotR accesses S7 slots (@properties, @contents, @name, @arguments)
      # which may change across ellmer releases. Warn on untested versions.
      tested_max <- package_version("0.4.99")
      if (package_version(ver) > tested_max) {
        packageStartupMessage(
          "AutoPlotR: ellmer ", ver, " is newer than tested versions (<= ",
          as.character(tested_max), "). ",
          "If structured output or tool calling fails, please report at ",
          "https://github.com/mehrdadameri/AutoPlotR/issues"
        )
      }
    }
  }
  invisible()
}
