# Text processing for English and Spanish, in base R. Accented letters are
# written as Unicode escapes so that the code stays ASCII.

es_vowels <- "aeiou\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc"
es_strong <- "aeo\u00e1\u00e9\u00f3\u00ed\u00fa"   # accented i and u break diphthongs

# Abbreviations whose full stop does not end a sentence.
abbreviations <- c("e.g.", "i.e.", "et al.", "vs.", "Dr.", "Dra.", "Prof.", "Fig.", "approx.",
                   "aprox.", "p. ej.", "Sr.", "Sra.", "etc.", "No.", "n.\u00ba", "cf.")

sentences <- function(text) {
  t <- gsub("[[:space:]]+", " ", trimws(text))
  for (a in abbreviations) t <- gsub(a, gsub(".", "\001", a, fixed = TRUE), t, fixed = TRUE)
  t <- gsub("([0-9])\\.([0-9])", "\\1\001\\2", t)
  parts <- unlist(strsplit(t, "(?<=[.!?])[\"'\u201d\u00bb)]?\\s+", perl = TRUE))
  parts <- trimws(gsub("\001", ".", parts, fixed = TRUE))
  parts[grepl("\\p{L}", parts, perl = TRUE)]
}

words <- function(text) {
  m <- regmatches(text, gregexpr("\\p{L}+(?:['\u2019-]\\p{L}+)*", text, perl = TRUE))[[1]]
  m
}

syllables_en <- function(word) {
  w <- tolower(gsub("[^A-Za-z]", "", word))
  if (!nzchar(w)) return(1L)
  if (nchar(w) <= 3) return(1L)
  w <- sub("(?:[^laeiouy]es|[^laeiouy]ed|[^laeiouy]e)$", "", w, perl = TRUE)
  w <- sub("^y", "", w)
  n <- length(gregexpr("[aeiouy]+", w)[[1]])
  if (!grepl("[aeiouy]", w)) n <- 0L
  max(1L, n)
}

syllables_es <- function(word) {
  w <- tolower(word)
  # Silent u in que, qui, gue, gui
  w <- gsub("([qg])u([e\u00e9i\u00ed])", "\\1\\2", w)
  chars <- strsplit(w, "")[[1]]
  is_vowel <- vapply(chars, function(ch) grepl(ch, es_vowels, fixed = TRUE), logical(1))
  is_strong <- vapply(chars, function(ch) grepl(ch, es_strong, fixed = TRUE), logical(1))
  n <- 0L
  for (i in seq_along(chars)) {
    if (!is_vowel[i]) next
    if (i == 1L || !is_vowel[i - 1L]) {
      n <- n + 1L
    } else if (is_strong[i] && is_strong[i - 1L]) {
      n <- n + 1L                         # hiatus: two strong vowels
    }
  }
  max(1L, n)
}

syllables <- function(words, lang) {
  f <- if (lang == "es") syllables_es else syllables_en
  vapply(words, f, integer(1), USE.NAMES = FALSE)
}

text_stats <- function(text, lang) {
  s <- sentences(text)
  w <- words(text)
  syl <- syllables(w, lang)
  per_sentence <- vapply(s, function(x) length(words(x)), integer(1), USE.NAMES = FALSE)
  list(sentences = length(s), words = length(w), syllables = sum(syl),
       sentence_words = per_sentence,
       long_words = sum(syl >= if (lang == "es") 5L else 4L),
       word_list = w)
}

#' Lexicons used by the indicators
#'
#' The indicators count terms from small, documented lexicons in English and
#' Spanish: hedges and boosters (expressions that soften or strengthen a
#' claim), causal and associative language, references to non-human
#' studies, limitation markers, explicit statements of uncertainty,
#' direction words, relative and absolute risk, and explanation cues. The
#' lexicons were written for this package and are released under CC BY 4.0.
#'
#' @param lang `"en"` or `"es"`.
#' @param category Optional category to keep.
#'
#' @return A data frame with columns `lang`, `category` and `term`.
#' @export
#'
#' @examples
#' head(sc_lexicon("en", "hedge"))
#' table(sc_lexicon("es")$category)
sc_lexicon <- function(lang = c("en", "es"), category = NULL) {
  lang <- match.arg(lang)
  lex <- utils::read.csv(system.file("extdata", "lexicons.csv", package = "scicomm"),
                         encoding = "UTF-8", stringsAsFactors = FALSE)
  lex <- lex[lex$lang == lang, , drop = FALSE]
  if (!is.null(category)) lex <- lex[lex$category %in% category, , drop = FALSE]
  rownames(lex) <- NULL
  lex
}

term_pattern <- function(term) {
  paste0("(?<![\\p{L}\\p{N}])", regex_escape(tolower(term)), "(?![\\p{L}\\p{N}])")
}

negations <- list(
  en = "(?<![\\p{L}])(not|no|never|nor|cannot|can't|doesn't|don't|didn't|isn't|aren't|without)(?![\\p{L}])",
  es = "(?<![\\p{L}])(no|ni|nunca|sin)(?![\\p{L}])"
)

# Which terms of a category occur, and how often. With `negated = FALSE`,
# occurrences negated earlier in the same clause ("not proof that X causes
# Y") are not counted.
find_terms <- function(text, lang, category, negated = TRUE) {
  terms <- sc_lexicon(lang, category)$term
  t <- tolower(text)
  counts <- vapply(terms, function(term) {
    m <- gregexpr(term_pattern(term), t, perl = TRUE)[[1]]
    if (m[1] == -1) return(0L)
    if (!negated) {
      keep <- vapply(m, function(pos) {
        before <- substr(t, max(1, pos - 40), pos - 1)
        before <- sub("^.*[.;:!?]", "", before)
        !grepl(negations[[lang]], before, perl = TRUE)
      }, logical(1))
      return(sum(keep))
    }
    length(m)
  }, integer(1))
  counts[counts > 0]
}

has_terms <- function(text, terms) {
  if (!length(terms)) return(logical(0))
  t <- tolower(text)
  vapply(terms, function(term) grepl(term_pattern(term), t, perl = TRUE), logical(1))
}

numbers_in <- function(text, lang) {
  # Thousands separators: "2,400" in English, "2.400" in Spanish.
  thousands <- if (lang == "es") "(?<=\\d)\\.(?=\\d{3}(?!\\d))" else "(?<=\\d),(?=\\d{3}(?!\\d))"
  t <- gsub(thousands, "", text, perl = TRUE)
  m <- regmatches(t, gregexpr("(?<![\\p{L}\\d])\\d+(?:[.,]\\d+)?", t, perl = TRUE))[[1]]
  as.numeric(gsub(",", ".", m, fixed = TRUE))
}

# Intervals or ranges written out: "95% CI 2.1 to 6.3", "between 2 and 6", "4.2 (2.1-6.3)", "+/-".
has_interval <- function(text, lang) {
  phrases <- sc_lexicon(lang, "uncertainty_number")$term
  num <- "\\d+(?:[.,]\\d+)?"
  joiners <- if (lang == "es") "(?:a|y|-|\u2013|hasta)" else "(?:to|and|-|\u2013)"
  starts <- if (lang == "es") "(?:entre|de|ic|intervalo|rango)" else "(?:between|from|ci|interval|range)"
  pattern <- paste0("(?i)", starts, "[^.]{0,20}?", num, "\\s*", joiners, "\\s*", num)
  any(has_terms(text, phrases)) || grepl(pattern, text, perl = TRUE) ||
    grepl("\u00b1", text, fixed = TRUE) ||
    grepl(paste0("\\(", num, "\\s*(?:-|\u2013|to|a)\\s*", num, "\\)"), text, perl = TRUE)
}

# Acronyms (two or more capitals) never spelled out in brackets.
undefined_acronyms <- function(text) {
  acr <- unique(regmatches(text, gregexpr("\\b[A-Z][A-Z0-9]{1,}s?\\b", text, perl = TRUE))[[1]])
  acr <- acr[!grepl("^[0-9]+$", acr)]
  defined <- vapply(acr, function(a) {
    grepl(paste0("\\(\\s*", a, "\\s*\\)"), text, perl = TRUE) ||
      grepl(paste0("\\b", a, "\\s*\\([^)]{6,}\\)"), text, perl = TRUE)
  }, logical(1))
  acr[!defined]
}

content_words <- function(text) {
  w <- unlist(strsplit(tolower(words(text)), "['\u2019-]"))
  unique(w[nchar(w) >= 5])
}

# Share of the content words of `reference` that appear in `text`.
overlap <- function(reference, text, exclude = character(0)) {
  ref <- content_words(reference)
  stems <- setdiff(unique(substr(ref, 1, 5)), exclude)
  if (!length(stems)) return(NA_real_)
  tw <- substr(unlist(strsplit(tolower(words(text)), "['\u2019-]")), 1, 5)
  mean(stems %in% tw)
}
