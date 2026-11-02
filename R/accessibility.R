#' Accessibility indicators
#'
#' Whether a text suits its intended audience: reading ease against the
#' audience's target, technical terms used without explanation, and how risk
#' is expressed. Relative risks without absolute risks ("twice as likely")
#' and percentages instead of natural frequencies ("3 in 100") are harder
#' for non-specialists to interpret correctly.
#'
#' @param version An `sc_version`.
#' @param finding Optional `sc_finding`, whose `terms` are checked.
#' @param targets Targets by audience, as returned by [sc_targets()].
#'
#' @return An `sc_indicators` data frame.
#' @export
#'
#' @examples
#' ex <- sc_example()
#' sc_accessibility(ex$versions$student, ex$finding)
sc_accessibility <- function(version, finding = NULL, targets = sc_targets(version$lang)) {
  stopifnot(inherits(version, "sc_version"))
  lang <- version$lang
  text <- version$text
  lay <- version$audience != "specialist"
  r <- sc_readability(version)
  t_ease <- target_for(targets, version$audience, "reading_ease")

  terms <- if (is.null(finding)) character(0) else finding$terms
  used <- terms[has_terms(text, terms)]
  explained <- vapply(used, function(term) term_explained(text, term, lang), logical(1))
  unexplained <- used[!explained]

  rel <- names(find_terms(text, lang, "relative_risk"))
  abs <- names(find_terms(text, lang, "absolute_risk"))
  pct <- sum(gregexpr("\\d\\s?%", text, perl = TRUE)[[1]] > 0)

  rows <- list(
    indicator("accessibility", "reading_ease_vs_target", r$reading_ease, t_ease$text,
              flag_against(r$reading_ease, t_ease), r$band),
    indicator("accessibility", "technical_terms_unexplained", length(unexplained),
              if (lay) "0" else NA_character_, lay && length(unexplained) > 0,
              if (length(used)) sprintf("%d of %d terms used; unexplained: %s", length(used), length(terms),
                                        if (length(unexplained)) paste(unexplained, collapse = ", ") else "none")
              else NA_character_),
    indicator("accessibility", "relative_risk_without_absolute",
              as.integer(length(rel) > 0 && !length(abs)), if (lay) "0" else NA_character_,
              lay && length(rel) > 0 && !length(abs),
              if (length(rel)) paste(rel, collapse = ", ") else NA_character_),
    indicator("accessibility", "percentages", pct,
              detail = if (length(abs)) "natural frequencies also used" else NA_character_)
  )
  new_indicators(rows, version)
}

# A term counts as explained if an explanation cue or a bracket is near it.
term_explained <- function(text, term, lang) {
  t <- tolower(text)
  pos <- regexpr(term_pattern(term), t, perl = TRUE)
  if (pos < 0) return(TRUE)
  window <- substr(t, max(1, pos - 80), pos + attr(pos, "match.length") + 80)
  cues <- sc_lexicon(lang, "explanation")$term
  any(has_terms(window, cues)) ||
    grepl(paste0(regex_escape(tolower(term)), "\\s*[(:\u2014-]"), window, perl = TRUE) ||
    grepl(paste0("[(\u2014]\\s*[^)]*", regex_escape(tolower(term))), window, perl = TRUE)
}
