#' Assess the versions of a finding
#'
#' Runs every indicator on every version: clarity ([sc_clarity()]),
#' accessibility ([sc_accessibility()]), uncertainty ([sc_uncertainty()])
#' and fidelity to the finding ([sc_fidelity()]).
#'
#' Three kinds of evidence are needed to judge science communication, and
#' this function provides only the first:
#'
#' 1. **Automatic indicators**: transparent proxies that point to passages
#'    worth reading, such as causal wording or a missing limitation.
#' 2. **Expert judgement** of rigour, structure and narrative.
#' 3. **Studies with readers**, the only way to show that a text is
#'    understood.
#'
#' The indicators are reported one by one. There is no composite score.
#'
#' @param finding The `sc_finding` being communicated.
#' @param versions An `sc_version` or a list of them.
#' @param targets Targets by audience, as returned by [sc_targets()].
#'
#' @return An object of class `sc_assessment`: a list with `finding`,
#'   `versions`, `indicators` (an `sc_indicators` data frame) and `fidelity`
#'   (an `sc_fidelity` data frame).
#' @export
#'
#' @examples
#' ex <- sc_example()
#' a <- sc_assess(ex$finding, ex$versions)
#' a
sc_assess <- function(finding, versions, targets = sc_targets(finding$lang)) {
  stopifnot(inherits(finding, "sc_finding"))
  versions <- as_versions(versions)
  langs <- unique(vapply(versions, `[[`, "", "lang"))
  if (!all(langs == finding$lang)) {
    warning("Some versions are not in the language of the finding (", finding$lang,
            "); numbers and limitations may not be matched.", call. = FALSE)
  }
  ind <- do.call(rbind, lapply(versions, function(v) {
    rbind(sc_clarity(v, targets), sc_accessibility(v, finding, targets),
          sc_uncertainty(v, finding))
  }))
  fid <- do.call(rbind, lapply(versions, sc_fidelity, finding = finding))
  rownames(ind) <- NULL
  rownames(fid) <- NULL
  class(ind) <- c("sc_indicators", "data.frame")
  class(fid) <- c("sc_fidelity", "data.frame")
  structure(list(finding = finding, versions = versions, indicators = ind, fidelity = fid),
            class = "sc_assessment")
}

#' @export
print.sc_assessment <- function(x, ...) {
  cat("<sc_assessment: ", length(x$versions), " version(s)>\n\n", sep = "")
  print(sc_compare(x))
  flagged <- x$indicators[isTRUE_vec(x$indicators$flag), , drop = FALSE]
  if (nrow(flagged)) {
    cat("\nFlagged indicators:\n")
    for (i in seq_len(nrow(flagged))) {
      cat(sprintf("  %-11s %-30s %s%s\n", flagged$version[i], flagged$indicator[i],
                  format(flagged$value[i]),
                  if (is.na(flagged$detail[i])) "" else paste0("  ", flagged$detail[i])))
    }
  }
  cat("\nUse $indicators and $fidelity for the details, and sc_report() for a report.\n")
  invisible(x)
}

isTRUE_vec <- function(x) !is.na(x) & x

#' Compare how each audience's version keeps the finding
#'
#' A table with one row per element of the finding and one column per
#' version, showing whether the element is kept, lost or overstated as the
#' message is adapted to each audience. Reading ease is added as the last
#' row, so that simplification and fidelity can be read together.
#'
#' @param finding An `sc_finding`, or an `sc_assessment` from [sc_assess()].
#' @param versions An `sc_version` or a list of them. Not needed if
#'   `finding` is an `sc_assessment`.
#'
#' @return A data frame of class `sc_comparison`.
#' @export
#'
#' @examples
#' ex <- sc_example()
#' sc_compare(ex$finding, ex$versions)
sc_compare <- function(finding, versions = NULL) {
  a <- if (inherits(finding, "sc_assessment")) finding else sc_assess(finding, versions)
  fid <- a$fidelity
  ids <- names(a$versions)
  elements <- unique(fid$element)
  out <- data.frame(element = elements, stringsAsFactors = FALSE)
  for (id in ids) {
    f <- fid[fid$version == id, , drop = FALSE]
    out[[id]] <- f$status[match(elements, f$element)]
  }
  ease <- a$indicators[a$indicators$indicator == "reading_ease", , drop = FALSE]
  ease_row <- data.frame(element = "reading_ease", stringsAsFactors = FALSE)
  for (id in ids) ease_row[[id]] <- format(ease$value[ease$version == id])
  out <- rbind(out, ease_row)
  audiences <- vapply(a$versions, `[[`, "", "audience")
  structure(out, class = c("sc_comparison", "data.frame"), audiences = audiences)
}

#' @export
print.sc_comparison <- function(x, ...) {
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  attr(df, "audiences") <- NULL
  mark <- function(s) ifelse(s %in% c("missing", "changed", "overstated"), paste0(s, " !"), s)
  for (j in seq_along(df)[-1]) df[[j]] <- mark(df[[j]])
  print(df, row.names = FALSE, right = FALSE)
  cat("! = lost or overstated relative to the finding\n")
  invisible(x)
}
