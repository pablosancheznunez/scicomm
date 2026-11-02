fidelity_statuses <- c("preserved", "partial", "missing", "changed", "overstated", "not_applicable")

#' Fidelity of a text to the finding
#'
#' Checks, element by element, whether a text keeps what the evidence
#' supports. The checks follow the kinds of exaggeration documented in
#' health news and press releases (Sumner et al., 2014,
#' \doi{10.1136/bmj.g7015}): causal claims from correlational designs,
#' results from animals or cells presented as if they applied to people,
#' and advice that goes beyond the study.
#'
#' | Element | What is checked |
#' |---|---|
#' | `causal_language` | Causal wording ("causes", "leads to") when the design does not support it |
#' | `population` | Whether the population studied is mentioned |
#' | `nonhuman` | For animal and in vitro studies, whether this is said |
#' | `direction` | Whether the direction of the effect is kept, or reversed |
#' | `numbers` | Which key numbers of the finding are kept (rounding is accepted) |
#' | `magnitude` | Intensifiers ("dramatically", "much worse") |
#' | `certainty` | Boosters ("proves", "clearly") when certainty is not high |
#' | `limitations` | Which limitations are mentioned |
#'
#' Each element is `preserved`, `partial`, `missing`, `changed`,
#' `overstated` or `not_applicable`. The checks are lexical, so they point
#' to passages for a person to read; they do not prove that a text is
#' faithful or unfaithful.
#'
#' @param version An `sc_version`.
#' @param finding The `sc_finding` the text communicates.
#'
#' @return A data frame of class `sc_fidelity` with columns `version`,
#'   `audience`, `element`, `status` and `detail`.
#' @export
#'
#' @examples
#' ex <- sc_example()
#' sc_fidelity(ex$versions$public, ex$finding)
sc_fidelity <- function(version, finding) {
  stopifnot(inherits(version, "sc_version"), inherits(finding, "sc_finding"))
  lang <- version$lang
  text <- version$text
  out <- list()
  add <- function(element, status, detail = NA_character_) {
    out[[length(out) + 1L]] <<- data.frame(element = element, status = status,
                                           detail = detail, stringsAsFactors = FALSE)
  }
  show <- function(x) paste(names(x), collapse = ", ")

  causal <- find_terms(text, lang, "causal", negated = FALSE)
  assoc <- find_terms(text, lang, "associative")
  if (finding$causal) {
    add("causal_language", "not_applicable", "the design supports causal claims")
  } else if (length(causal)) {
    add("causal_language", "overstated", paste("causal wording:", show(causal)))
  } else {
    add("causal_language", "preserved",
        if (length(assoc)) paste("associative wording:", show(assoc)) else "no causal wording")
  }

  pop <- overlap(finding$population, text)
  add("population", if (isTRUE(pop >= 0.34)) "preserved" else "missing",
      sprintf("%.0f%% of the population's key words appear", 100 * (pop %||% 0)))

  if (finding$design %in% c("animal", "in_vitro")) {
    nh <- find_terms(text, lang, "nonhuman")
    add("nonhuman", if (length(nh)) "preserved" else "changed",
        if (length(nh)) show(nh) else "the text does not say the study was not in people")
  } else {
    add("nonhuman", "not_applicable")
  }

  up <- find_terms(text, lang, "increase")
  down <- find_terms(text, lang, "decrease")
  none <- find_terms(text, lang, "no_effect")
  dir <- switch(finding$direction,
    increase = if (length(up)) "preserved" else if (length(down)) "changed" else "missing",
    decrease = if (length(down)) "preserved" else if (length(up)) "changed" else "missing",
    no_effect = if (length(none)) "preserved" else if (length(up) || length(down)) "changed" else "missing",
    mixed = "not_applicable")
  add("direction", dir, paste(c(if (length(up)) paste("up:", show(up)),
                                if (length(down)) paste("down:", show(down)),
                                if (length(none)) paste("none:", show(none))), collapse = "; "))

  if (length(finding$numbers)) {
    found <- numbers_in(text, lang)
    kept <- vapply(finding$numbers, function(x) {
      any(abs(found - x) < 1e-9) || any(abs(found - round(x)) < 1e-9 & round(x) != 0)
    }, logical(1))
    status <- if (all(kept)) "preserved" else if (any(kept)) "partial" else "missing"
    context <- if (is.null(finding$effect)) numeric(0) else numbers_in(finding$effect, finding$lang)
    extra <- setdiff(found, c(finding$numbers, round(finding$numbers), context))
    extra <- extra[extra > 12 & !(extra >= 1900 & extra <= 2100)]
    add("numbers", status, paste0(sum(kept), " of ", length(kept), " kept",
                                  if (any(!kept)) paste0(" (missing: ", paste(finding$numbers[!kept], collapse = ", "), ")"),
                                  if (length(extra)) paste0("; other numbers: ", paste(extra, collapse = ", "))))
  } else {
    add("numbers", "not_applicable", "no key numbers in the finding")
  }

  intens <- find_terms(text, lang, "intensifier", negated = FALSE)
  add("magnitude", if (length(intens)) "overstated" else "preserved",
      if (length(intens)) show(intens) else NA_character_)

  boosters <- find_terms(text, lang, "booster", negated = FALSE)
  add("certainty", if (finding$certainty != "high" && length(boosters)) "overstated" else "preserved",
      paste0("certainty of the evidence: ", finding$certainty,
             if (length(boosters)) paste0("; boosters: ", show(boosters))))

  if (length(finding$limitations)) {
    lim <- limitations_found(text, finding, lang)
    status <- if (all(lim$hit)) "preserved" else if (any(lim$hit) || lim$markers) "partial" else "missing"
    add("limitations", status, lim$detail)
  } else {
    add("limitations", "not_applicable", "no limitations in the finding")
  }

  res <- do.call(rbind, out)
  res <- cbind(version = version$id, audience = version$audience, res, stringsAsFactors = FALSE)
  structure(res, class = c("sc_fidelity", "data.frame"))
}

#' @export
print.sc_fidelity <- function(x, ...) {
  if (!all(c("element", "status") %in% names(x))) return(NextMethod())
  for (v in unique(x$version)) {
    rows <- x[x$version == v, , drop = FALSE]
    cat("<fidelity: ", v, " (", rows$audience[1], ")>\n", sep = "")
    for (i in seq_len(nrow(rows))) {
      mark <- if (rows$status[i] %in% c("missing", "changed", "overstated")) "!" else " "
      det <- if (is.na(rows$detail[i]) || !nzchar(rows$detail[i])) "" else paste0("  ", rows$detail[i])
      cat(sprintf(" %s %-16s %-15s%s\n", mark, rows$element[i], rows$status[i], det))
    }
  }
  invisible(x)
}
