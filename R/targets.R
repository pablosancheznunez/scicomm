#' Targets for each audience
#'
#' The thresholds used to flag clarity and accessibility indicators. They are
#' conventions, not evidence that a text will be understood: change them to
#' suit your audience, and validate them with readers.
#'
#' Reading ease is the Flesch reading ease for English (60 is "standard")
#' and the Szigriszt-Pazos perspicuity index for Spanish, interpreted with
#' the INFLESZ scale (55 is "normal").
#'
#' @param lang `"en"` or `"es"`.
#'
#' @return A data frame with columns `audience`, `indicator`, `min` and `max`.
#' @export
#'
#' @examples
#' sc_targets()
#' sc_targets("es")
sc_targets <- function(lang = c("en", "es")) {
  lang <- match.arg(lang)
  ease <- if (lang == "es") c(public = 55, student = 40) else c(public = 60, student = 50)
  data.frame(
    audience = c("public", "student", "public", "student", "specialist",
                 "public", "student"),
    indicator = c("reading_ease", "reading_ease", "mean_sentence_length",
                  "mean_sentence_length", "mean_sentence_length",
                  "undefined_acronyms", "undefined_acronyms"),
    min = c(ease[["public"]], ease[["student"]], NA, NA, NA, NA, NA),
    max = c(NA, NA, 20, 25, 30, 0, 0),
    stringsAsFactors = FALSE
  )
}

target_for <- function(targets, audience, name) {
  t <- targets[targets$audience == audience & targets$indicator == name, , drop = FALSE]
  if (!nrow(t)) return(list(min = NA, max = NA, text = NA_character_))
  text <- if (!is.na(t$min[1])) paste(">=", t$min[1]) else paste("<=", t$max[1])
  list(min = t$min[1], max = t$max[1], text = text)
}

flag_against <- function(value, target) {
  if (is.na(value)) return(NA)
  (!is.na(target$min) && value < target$min) || (!is.na(target$max) && value > target$max)
}
