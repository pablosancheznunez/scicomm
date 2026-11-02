`%||%` <- function(a, b) if (is.null(a)) b else a

check_string <- function(x, name, null_ok = FALSE) {
  if (is.null(x) && null_ok) return(invisible(x))
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", name, "` must be a single non-empty string.", call. = FALSE)
  }
  invisible(x)
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

regex_escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

# One row of an indicator table.
indicator <- function(dimension, name, value, target = NA_character_, flag = NA,
                      detail = NA_character_) {
  data.frame(dimension = dimension, indicator = name,
             value = if (is.null(value) || length(value) == 0L) NA_real_ else round(as.numeric(value), 2),
             target = target, flag = as.logical(flag), detail = detail,
             stringsAsFactors = FALSE)
}

new_indicators <- function(rows, version) {
  out <- do.call(rbind, rows)
  out <- cbind(version = version$id, audience = version$audience, out,
               stringsAsFactors = FALSE)
  rownames(out) <- NULL
  structure(out, class = c("sc_indicators", "data.frame"))
}

#' @export
print.sc_indicators <- function(x, ...) {
  if (!all(c("indicator", "value", "flag") %in% names(x))) return(NextMethod())
  for (v in unique(x$version)) {
    rows <- x[x$version == v, , drop = FALSE]
    cat("<", v, " (", rows$audience[1], ")>\n", sep = "")
    for (i in seq_len(nrow(rows))) {
      mark <- if (isTRUE(rows$flag[i])) "!" else " "
      val <- if (is.na(rows$value[i])) "" else format(rows$value[i])
      tgt <- if (is.na(rows$target[i])) "" else paste0(" (target ", rows$target[i], ")")
      det <- if (is.na(rows$detail[i])) "" else paste0(": ", rows$detail[i])
      cat(sprintf(" %s %-13s %-28s %s%s%s\n", mark, rows$dimension[i], rows$indicator[i],
                  val, tgt, det))
    }
  }
  cat("\n! = flagged. Automatic indicators are proxies; see ?sc_assess.\n")
  invisible(x)
}
