#' Readability of a text
#'
#' Flesch reading ease and Flesch-Kincaid grade for English; the
#' Szigriszt-Pazos perspicuity index for Spanish, with its INFLESZ band.
#' Syllables are counted with rules, which are approximate for English.
#'
#' @param text A character string, or an `sc_version`.
#' @param lang `"en"` or `"es"`. Taken from `text` if it is an `sc_version`.
#'
#' @return A list with `sentences`, `words`, `syllables`, `reading_ease`,
#'   `band`, and for English `grade`.
#' @export
#'
#' @examples
#' sc_readability("The cat sat on the mat. It was happy.")
#' sc_readability("El gato se sienta en la alfombra. Estaba contento.", lang = "es")
sc_readability <- function(text, lang = c("en", "es")) {
  if (inherits(text, "sc_version")) {
    lang <- text$lang
    text <- text$text
  }
  lang <- match.arg(lang)
  s <- text_stats(text, lang)
  if (!s$words || !s$sentences) {
    return(list(sentences = s$sentences, words = s$words, syllables = s$syllables,
                reading_ease = NA_real_, band = NA_character_, grade = NA_real_))
  }
  wps <- s$words / s$sentences
  spw <- s$syllables / s$words
  if (lang == "es") {
    ease <- 206.835 - 62.3 * spw - wps
    band <- cut(ease, c(-Inf, 40, 55, 65, 80, Inf), right = FALSE,
                labels = c("muy dif\u00edcil", "algo dif\u00edcil", "normal",
                           "bastante f\u00e1cil", "muy f\u00e1cil"))
    grade <- NA_real_
  } else {
    ease <- 206.835 - 1.015 * wps - 84.6 * spw
    band <- cut(ease, c(-Inf, 30, 50, 60, 70, 80, 90, Inf), right = FALSE,
                labels = c("very difficult", "difficult", "fairly difficult", "standard",
                           "fairly easy", "easy", "very easy"))
    grade <- 0.39 * wps + 11.8 * spw - 15.59
  }
  list(sentences = s$sentences, words = s$words, syllables = s$syllables,
       reading_ease = round(ease, 1), band = as.character(band), grade = round(grade, 1))
}

passive_share <- function(text, lang) {
  s <- sentences(text)
  if (!length(s)) return(NA_real_)
  pattern <- if (lang == "es") {
    "(?i)\\b(es|son|fue|fueron|ser|sido|era|eran|ser\u00e1|ser\u00e1n)\\s+\\p{L}+(ado|ada|ados|adas|ido|ida|idos|idas)\\b"
  } else {
    "(?i)\\b(is|are|was|were|be|been|being)\\s+(\\w+ly\\s+)?\\w+(ed|en)\\b"
  }
  mean(grepl(pattern, s, perl = TRUE))
}

#' Clarity indicators
#'
#' Sentence length, readability, long words, undefined acronyms and a rough
#' measure of passive voice. Clarity also depends on things no formula sees,
#' such as structure and examples; see [sc_assess()].
#'
#' @param version An `sc_version`.
#' @param targets Targets by audience, as returned by [sc_targets()].
#'
#' @return An `sc_indicators` data frame.
#' @export
#'
#' @examples
#' ex <- sc_example()
#' sc_clarity(ex$versions$public)
sc_clarity <- function(version, targets = sc_targets(version$lang)) {
  stopifnot(inherits(version, "sc_version"))
  lang <- version$lang
  st <- text_stats(version$text, lang)
  r <- sc_readability(version)
  msl <- if (st$sentences) st$words / st$sentences else NA
  t_len <- target_for(targets, version$audience, "mean_sentence_length")
  acr <- undefined_acronyms(version$text)
  t_acr <- target_for(targets, version$audience, "undefined_acronyms")
  ease_name <- if (lang == "es") "Szigriszt-Pazos (INFLESZ)" else "Flesch reading ease"
  rows <- list(
    indicator("clarity", "words", st$words),
    indicator("clarity", "mean_sentence_length", msl, t_len$text, flag_against(msl, t_len)),
    indicator("clarity", "long_sentences_pct",
              if (st$sentences) 100 * mean(st$sentence_words > 30) else NA,
              detail = "sentences over 30 words"),
    indicator("clarity", "reading_ease", r$reading_ease, detail = paste0(ease_name, ": ", r$band)),
    if (lang == "en") indicator("clarity", "grade_level", r$grade, detail = "Flesch-Kincaid"),
    indicator("clarity", "long_words_pct", if (st$words) 100 * st$long_words / st$words else NA,
              detail = if (lang == "es") "words of 5 or more syllables" else "words of 4 or more syllables"),
    indicator("clarity", "undefined_acronyms", length(acr), t_acr$text,
              flag_against(length(acr), t_acr),
              if (length(acr)) paste(acr, collapse = ", ") else NA_character_),
    indicator("clarity", "passive_sentences_pct", 100 * passive_share(version$text, lang),
              detail = "approximate")
  )
  new_indicators(Filter(Negate(is.null), rows), version)
}
