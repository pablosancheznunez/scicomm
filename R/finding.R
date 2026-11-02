designs <- c("rct", "observational", "animal", "in_vitro", "model", "qualitative", "review")
certainties <- c("high", "moderate", "low", "very_low")
directions <- c("increase", "decrease", "no_effect", "mixed")

#' Describe the scientific result being communicated
#'
#' Every text is assessed against this structured description of the
#' evidence, so it should be written from the original study, not from a
#' summary of it.
#'
#' @param claim The result in one sentence, as the study supports it.
#' @param direction Direction of the effect or association: `"increase"`,
#'   `"decrease"`, `"no_effect"` or `"mixed"`.
#' @param design Study design: `"rct"` (randomised controlled trial),
#'   `"observational"`, `"animal"`, `"in_vitro"`, `"model"`,
#'   `"qualitative"` or `"review"`.
#' @param certainty Certainty of the evidence, on a GRADE-like scale:
#'   `"high"`, `"moderate"`, `"low"` or `"very_low"`.
#' @param population Who or what was studied, such as
#'   `"secondary school students in Spain"`.
#' @param effect Optional size of the effect, as text, such as
#'   `"4.2 points (95% CI 2.1 to 6.3)"`.
#' @param numbers Optional numeric values that a faithful text should keep,
#'   such as the effect size and the sample size.
#' @param limitations Character vector of the main limitations.
#' @param causal Whether the design supports a causal conclusion. By default
#'   `TRUE` only for randomised trials and reviews of them.
#' @param terms Optional technical terms that a text for non-specialists
#'   should explain.
#' @param lang Language of the finding and of the texts: `"en"` or `"es"`.
#'
#' @return An object of class `sc_finding`.
#' @export
#'
#' @examples
#' f <- sc_finding(
#'   claim = "Students who slept less than 7 hours had lower maths scores.",
#'   direction = "decrease", design = "observational", certainty = "low",
#'   population = "secondary school students in Spain",
#'   effect = "4.2 points lower (95% CI 2.1 to 6.3)", numbers = c(4.2, 2400),
#'   limitations = c("sleep was self-reported", "cross-sectional design")
#' )
#' f
sc_finding <- function(claim, direction = directions, design = designs,
                       certainty = certainties, population, effect = NULL,
                       numbers = NULL, limitations = character(0),
                       causal = NULL, terms = character(0), lang = c("en", "es")) {
  check_string(claim, "claim")
  direction <- match.arg(direction)
  design <- match.arg(design)
  certainty <- match.arg(certainty)
  check_string(population, "population")
  check_string(effect, "effect", null_ok = TRUE)
  lang <- match.arg(lang)
  if (!is.null(numbers) && !is.numeric(numbers)) stop("`numbers` must be numeric.", call. = FALSE)
  causal <- causal %||% (design %in% c("rct", "review"))
  structure(list(
    claim = claim, direction = direction, design = design, certainty = certainty,
    population = population, effect = effect, numbers = numbers,
    limitations = as.character(limitations), causal = isTRUE(causal),
    terms = as.character(terms), lang = lang
  ), class = "sc_finding")
}

#' @export
print.sc_finding <- function(x, ...) {
  cat("<sc_finding>\n")
  cat("  Claim:       ", x$claim, "\n", sep = "")
  cat("  Direction:   ", x$direction, "\n", sep = "")
  cat("  Design:      ", x$design, if (x$causal) " (supports causal claims)" else
        " (does not support causal claims)", "\n", sep = "")
  cat("  Certainty:   ", x$certainty, "\n", sep = "")
  cat("  Population:  ", x$population, "\n", sep = "")
  if (!is.null(x$effect)) cat("  Effect:      ", x$effect, "\n", sep = "")
  if (length(x$numbers)) cat("  Key numbers: ", paste(x$numbers, collapse = ", "), "\n", sep = "")
  if (length(x$limitations)) {
    cat("  Limitations:\n")
    for (l in x$limitations) cat("    - ", l, "\n", sep = "")
  }
  invisible(x)
}

#' A text that communicates the finding to an audience
#'
#' @param text The text: an abstract, a press release, a news story, a
#'   social media post, a lesson.
#' @param audience Intended audience: `"specialist"`, `"student"` or
#'   `"public"`.
#' @param id Optional short name, such as `"press_release"`. Defaults to
#'   the audience.
#' @param lang Language of the text: `"en"` or `"es"`.
#'
#' @return An object of class `sc_version`.
#' @export
#'
#' @examples
#' v <- sc_version("Teenagers who sleep less do worse in maths.", "public")
#' v
sc_version <- function(text, audience = c("specialist", "student", "public"),
                       id = NULL, lang = c("en", "es")) {
  check_string(text, "text")
  audience <- match.arg(audience)
  lang <- match.arg(lang)
  check_string(id, "id", null_ok = TRUE)
  structure(list(id = id %||% audience, audience = audience, text = text, lang = lang),
            class = "sc_version")
}

#' @export
print.sc_version <- function(x, ...) {
  cat("<sc_version: ", x$id, " (", x$audience, ", ", x$lang, ")>\n", sep = "")
  cat(strwrap(x$text, width = 76, indent = 2, exdent = 2), sep = "\n")
  invisible(x)
}

as_versions <- function(versions) {
  if (inherits(versions, "sc_version")) versions <- list(versions)
  if (!is.list(versions) || !length(versions) ||
      !all(vapply(versions, inherits, logical(1), "sc_version"))) {
    stop("`versions` must be an sc_version or a list of them.", call. = FALSE)
  }
  ids <- vapply(versions, `[[`, "", "id")
  if (anyDuplicated(ids)) stop("Each version needs a different `id`.", call. = FALSE)
  names(versions) <- ids
  versions
}

#' Example: one finding communicated to three audiences
#'
#' A fictitious observational study (synthetic data) on sleep and maths
#' scores, with versions for specialists, students and the general public.
#' The specialist and student versions are faithful; the public version
#' deliberately overstates the result, so the indicators have something to
#' find.
#'
#' @param lang `"en"` or `"es"`.
#'
#' @return A list with `finding` (an `sc_finding`) and `versions` (a list of
#'   three `sc_version` objects).
#' @export
#'
#' @examples
#' ex <- sc_example()
#' ex$finding
#' ex$versions$public
sc_example <- function(lang = c("en", "es")) {
  lang <- match.arg(lang)
  ex <- utils::read.csv(system.file("extdata", "example.csv", package = "scicomm"),
                        encoding = "UTF-8", stringsAsFactors = FALSE)
  ex <- ex[ex$lang == lang, , drop = FALSE]
  get <- function(field) ex$value[ex$field == field]
  finding <- sc_finding(
    claim = get("claim"), direction = "decrease", design = "observational",
    certainty = "low", population = get("population"), effect = get("effect"),
    numbers = c(4.2, 2.1, 6.3, 2400),
    limitations = get("limitation"), terms = get("term"), lang = lang
  )
  versions <- list(
    sc_version(get("specialist"), "specialist", lang = lang),
    sc_version(get("student"), "student", lang = lang),
    sc_version(get("public"), "public", lang = lang)
  )
  list(finding = finding, versions = as_versions(versions))
}
