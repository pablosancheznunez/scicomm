#' Write a self-contained HTML report
#'
#' The report shows the finding, the fidelity of each version across
#' audiences, every text with its causal wording, boosters, hedges and
#' limitation language highlighted, and all indicators. It has no external
#' resources, so it can be sent by e-mail or attached to a submission.
#'
#' @param x An `sc_assessment` from [sc_assess()], or an `sc_finding`.
#' @param versions The versions, if `x` is an `sc_finding`.
#' @param file Output HTML file. There is no default, so nothing is written
#'   unless a destination is given.
#' @param lang Language of the report: `"en"` or `"es"`. Defaults to the
#'   language of the finding.
#'
#' @return The path of the report, invisibly.
#' @export
#'
#' @examples
#' ex <- sc_example()
#' out <- file.path(tempdir(), "scicomm-report.html")
#' sc_report(ex$finding, ex$versions, file = out)
sc_report <- function(x, versions = NULL, file, lang = NULL) {
  a <- if (inherits(x, "sc_assessment")) x else sc_assess(x, versions)
  f <- a$finding
  lang <- match.arg(lang %||% f$lang, c("en", "es"))
  s <- report_strings(lang)
  e <- html_escape

  table_html <- function(df, flag_col = NULL) {
    head <- paste0("<tr>", paste0("<th>", e(names(df)), "</th>", collapse = ""), "</tr>")
    body <- vapply(seq_len(nrow(df)), function(i) {
      cells <- vapply(df[i, ], function(v) e(ifelse(is.na(v), "", as.character(v))), "")
      cls <- if (!is.null(flag_col) && isTRUE(df[[flag_col]][i])) ' class="flag"' else ""
      paste0("<tr", cls, ">", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
    }, "")
    paste0('<div class="scroll"><table><thead>', head, "</thead><tbody>",
           paste(body, collapse = ""), "</tbody></table></div>")
  }

  comp <- sc_compare(a)
  comp_df <- as.data.frame(unclass(comp), stringsAsFactors = FALSE)
  attr(comp_df, "audiences") <- NULL
  names(comp_df)[1] <- s$element
  comp_html <- table_html(comp_df)
  for (st in c("missing", "changed", "overstated")) {
    comp_html <- gsub(paste0("<td>", st, "</td>"), paste0('<td class="bad">', st, "</td>"), comp_html, fixed = TRUE)
  }
  comp_html <- gsub("<td>preserved</td>", '<td class="good">preserved</td>', comp_html, fixed = TRUE)

  finding_rows <- c(
    sprintf("<dt>%s</dt><dd>%s</dd>", e(s$claim), e(f$claim)),
    sprintf("<dt>%s</dt><dd>%s</dd>", e(s$direction), e(f$direction)),
    sprintf("<dt>%s</dt><dd>%s (%s)</dd>", e(s$design), e(f$design), e(if (f$causal) s$causal_yes else s$causal_no)),
    sprintf("<dt>%s</dt><dd>%s</dd>", e(s$certainty), e(f$certainty)),
    sprintf("<dt>%s</dt><dd>%s</dd>", e(s$population), e(f$population)),
    if (!is.null(f$effect)) sprintf("<dt>%s</dt><dd>%s</dd>", e(s$effect), e(f$effect)),
    if (length(f$limitations)) sprintf("<dt>%s</dt><dd><ul>%s</ul></dd>", e(s$limitations),
                                       paste0("<li>", e(f$limitations), "</li>", collapse = ""))
  )

  version_html <- vapply(a$versions, function(v) {
    ind <- a$indicators[a$indicators$version == v$id, c("dimension", "indicator", "value", "target", "flag", "detail")]
    names(ind) <- c(s$dimension, s$indicator, s$value, s$target, s$flagged, s$detail)
    ind[[s$flagged]] <- ifelse(isTRUE_vec(ind[[s$flagged]]), "!", "")
    paste0(sprintf('<section class="version"><h3>%s <span class="tag">%s</span></h3>', e(v$id), e(v$audience)),
           sprintf('<p class="text">%s</p>', highlight(v$text, v$lang)),
           "<details><summary>", e(s$indicators), "</summary>", table_html(ind), "</details></section>")
  }, "")

  css <- paste(
    ":root{--ink:#1d1b19;--muted:#615a53;--bg:#fbfaf8;--card:#fff;--line:#e6e1da;--accent:#9a3412;",
    "--good:#1d6b3a;--bad:#a1260d;--hl-causal:#fde2cf;--hl-hedge:#d8f0dd;--hl-limit:#d9e8fb}",
    "@media (prefers-color-scheme:dark){:root{--ink:#ece7e1;--muted:#b3aaa0;--bg:#171513;--card:#211e1b;--line:#38322c;",
    "--accent:#f29b6b;--good:#7fd19b;--bad:#ff8a73;--hl-causal:#5a2e14;--hl-hedge:#1f4329;--hl-limit:#1d3553}}",
    "body{margin:0;background:var(--bg);color:var(--ink);font:16px/1.6 system-ui,-apple-system,Segoe UI,sans-serif}",
    "main{max-width:1000px;margin:0 auto;padding:32px 16px 64px}",
    "h1{font-family:Georgia,serif;font-weight:normal;margin:0 0 4px}h2{margin:40px 0 12px;font-size:1.2em;color:var(--accent)}",
    "dl{display:grid;grid-template-columns:max-content 1fr;gap:4px 16px}dt{color:var(--muted)}dd{margin:0}dd ul{margin:0;padding-left:18px}",
    ".scroll{overflow-x:auto}table{border-collapse:collapse;width:100%;font-size:.92em;background:var(--card)}",
    "th,td{border-bottom:1px solid var(--line);padding:7px 8px;text-align:left;vertical-align:top}th{color:var(--muted);font-weight:600}",
    "td.good{color:var(--good)}td.bad{color:var(--bad);font-weight:600}",
    ".version{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:4px 16px 12px;margin:16px 0}",
    ".tag{font-size:.75em;color:var(--muted);border:1px solid var(--line);border-radius:999px;padding:1px 8px;margin-left:6px}",
    ".text{font-size:1.02em}mark{border-radius:3px;padding:0 2px;color:inherit}",
    "mark.causal{background:var(--hl-causal)}mark.hedge{background:var(--hl-hedge)}mark.limit{background:var(--hl-limit)}",
    "summary{cursor:pointer;color:var(--accent)}.caveat{border-left:4px solid var(--accent);padding:4px 14px;background:var(--card)}",
    ".muted{color:var(--muted)}footer{margin-top:48px;color:var(--muted);font-size:.85em}"
  )
  html <- c(
    "<!doctype html>", sprintf('<html lang="%s"><head><meta charset="utf-8">', lang),
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    sprintf("<title>%s</title>", e(s$title)), "<style>", css, "</style></head><body><main>",
    sprintf("<h1>%s</h1>", e(s$title)),
    sprintf('<div class="caveat"><p><strong>%s.</strong> %s</p></div>', e(s$caveat_title), e(s$caveat)),
    sprintf("<h2>%s</h2>", e(s$finding)), "<dl>", finding_rows, "</dl>",
    sprintf("<h2>%s</h2>", e(s$comparison)), comp_html,
    sprintf("<h2>%s</h2>", e(s$versions)), sprintf('<p class="muted">%s</p>', e(s$legend)),
    version_html,
    sprintf("<footer>%s %s.</footer>", e(s$generated), format(Sys.Date())),
    "</main></body></html>"
  )
  writeLines(enc2utf8(html), file, useBytes = TRUE)
  invisible(normalizePath(file))
}

report_strings <- function(lang) {
  tab <- utils::read.csv(system.file("extdata", "i18n.csv", package = "scicomm"),
                         encoding = "UTF-8", stringsAsFactors = FALSE)
  stats::setNames(as.list(tab[[lang]]), tab$key)
}

# Escape the text and mark lexicon terms. Longer terms are marked first so
# that "leads to" is not split by "to".
highlight <- function(text, lang) {
  out <- html_escape(text)
  classes <- c(causal = "causal", booster = "causal", intensifier = "causal",
               hedge = "hedge", limitation = "limit")
  lex <- sc_lexicon(lang, names(classes))
  lex <- lex[order(-nchar(lex$term)), , drop = FALSE]
  placeholders <- character(0)
  for (i in seq_len(nrow(lex))) {
    pattern <- paste0("(?i)", term_pattern(lex$term[i]))
    m <- gregexpr(pattern, out, perl = TRUE)[[1]]
    if (m[1] == -1) next
    found <- unique(regmatches(out, list(m))[[1]])
    for (w in found) {
      key <- sprintf("\002%d\003", length(placeholders) + 1L)
      placeholders[key] <- sprintf('<mark class="%s">%s</mark>', classes[[lex$category[i]]], w)
      out <- gsub(paste0("(?<![\\p{L}\\p{N}\002])", regex_escape(w), "(?![\\p{L}\\p{N}])"), key, out, perl = TRUE)
    }
  }
  for (key in rev(names(placeholders))) out <- gsub(key, placeholders[[key]], out, fixed = TRUE)
  out
}
