#' Uncertainty indicators
#'
#' How a text conveys the certainty of the evidence: hedges (such as "may"
#' or "suggests") and boosters (such as "proves" or "clearly") per 100
#' words, whether the limitations of the finding are mentioned, and whether
#' an interval or range is given. Boosters are flagged when the evidence is
#' not of high certainty.
#'
#' @param version An `sc_version`.
#' @param finding The `sc_finding` the text communicates.
#'
#' @return An `sc_indicators` data frame.
#' @export
#'
#' @examples
#' ex <- sc_example()
#' sc_uncertainty(ex$versions$public, ex$finding)
sc_uncertainty <- function(version, finding) {
  stopifnot(inherits(version, "sc_version"), inherits(finding, "sc_finding"))
  lang <- version$lang
  text <- version$text
  n <- max(1L, length(words(text)))
  hedges <- find_terms(text, lang, "hedge")
  boosters <- find_terms(text, lang, "booster", negated = FALSE)
  lim <- limitations_found(text, finding, lang)
  interval_in_finding <- !is.null(finding$effect) && has_interval(finding$effect, finding$lang)
  interval <- has_interval(text, lang)
  not_high <- finding$certainty != "high"

  rows <- list(
    indicator("uncertainty", "hedges_per_100_words", 100 * sum(hedges) / n,
              detail = if (length(hedges)) paste(names(hedges), collapse = ", ") else NA_character_),
    indicator("uncertainty", "boosters_per_100_words", 100 * sum(boosters) / n,
              if (not_high) "0" else NA_character_, not_high && length(boosters) > 0,
              if (length(boosters)) paste(names(boosters), collapse = ", ") else NA_character_),
    indicator("uncertainty", "limitations_mentioned", lim$matched,
              if (length(finding$limitations)) paste0(">= 1 of ", length(finding$limitations)) else NA_character_,
              length(finding$limitations) > 0 && lim$matched == 0 && !lim$markers,
              lim$detail),
    indicator("uncertainty", "interval_or_range", as.integer(interval),
              if (interval_in_finding && version$audience != "public") "1" else NA_character_,
              interval_in_finding && version$audience != "public" && !interval,
              if (interval_in_finding) "the finding reports an interval" else NA_character_)
  )
  new_indicators(rows, version)
}

limitations_found <- function(text, finding, lang) {
  lims <- finding$limitations
  # Words of the topic itself ("sleep", "students") do not show that a
  # limitation is mentioned.
  topic <- substr(content_words(finding$claim), 1, 5)
  hit <- vapply(lims, function(l) isTRUE(overlap(l, text, exclude = topic) >= 0.4), logical(1))
  markers <- length(find_terms(text, lang, "limitation")) > 0
  detail <- if (length(lims)) {
    paste0(sum(hit), " of ", length(lims), " specific limitations",
           if (markers) "; general limitation language present" else "")
  } else NA_character_
  list(matched = sum(hit), markers = markers, hit = hit, detail = detail)
}
