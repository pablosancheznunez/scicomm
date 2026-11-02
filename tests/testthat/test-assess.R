status <- function(fid, element) fid$status[fid$element == element]

test_that("sc_finding validates its inputs and infers causality from the design", {
  expect_error(sc_finding("x", design = "survey", population = "p"))
  expect_error(sc_finding("x", population = "p", numbers = "4"), "numeric")
  expect_true(sc_finding("x", design = "rct", population = "p")$causal)
  expect_false(sc_finding("x", design = "observational", population = "p")$causal)
})

test_that("version ids must be unique", {
  v <- sc_version("A text.", "public")
  expect_error(sc_assess(sc_example()$finding, list(v, v)), "different `id`")
})

for (lang in c("en", "es")) {
  test_that(paste("the example behaves as designed:", lang), {
    ex <- sc_example(lang)
    cmp <- sc_compare(ex$finding, ex$versions)
    row <- function(el) unlist(cmp[cmp$element == el, -1])
    expect_equal(unname(row("causal_language")), c("preserved", "preserved", "overstated"))
    expect_equal(unname(row("numbers")), c("preserved", "preserved", "missing"))
    expect_equal(unname(row("certainty")), c("preserved", "preserved", "overstated"))
    expect_equal(unname(row("magnitude")), c("preserved", "preserved", "overstated"))
    expect_equal(unname(row("population"))[3], "missing")
    expect_equal(unname(row("limitations"))[c(1, 3)], c("preserved", "missing"))
    ease <- as.numeric(row("reading_ease"))
    expect_true(ease[1] < ease[2] && ease[2] < ease[3])
  })
}

test_that("animal results presented as human are flagged", {
  f <- sc_finding("Mice given the compound lived longer.", direction = "increase",
                  design = "animal", certainty = "low", population = "laboratory mice")
  v1 <- sc_version("New compound helps people live longer.", "public")
  v2 <- sc_version("In mice, a new compound increased lifespan.", "public")
  expect_equal(status(sc_fidelity(v1, f), "nonhuman"), "changed")
  expect_equal(status(sc_fidelity(v2, f), "nonhuman"), "preserved")
})

test_that("a reversed direction is flagged", {
  f <- sc_finding("Exercise reduced anxiety scores.", direction = "decrease",
                  design = "rct", certainty = "moderate", population = "adults")
  v <- sc_version("Exercise increases anxiety in adults.", "public")
  expect_equal(status(sc_fidelity(v, f), "direction"), "changed")
  expect_equal(status(sc_fidelity(v, f), "causal_language"), "not_applicable")
})

test_that("rounded numbers count as kept", {
  f <- sc_finding("x", direction = "decrease", design = "observational", certainty = "low",
                  population = "students", numbers = c(4.2, 2400))
  v <- sc_version("Students scored about 4 points lower; 2,400 took part.", "student")
  expect_equal(status(sc_fidelity(v, f), "numbers"), "preserved")
})

test_that("relative risk without absolute risk is flagged for lay audiences only", {
  pub <- sc_version("Smokers are three times more likely to develop the disease.", "public")
  spe <- sc_version("Smokers are three times more likely to develop the disease.", "specialist", id = "s")
  both <- sc_version("Smokers are three times more likely to develop it: 3 out of 100 instead of 1 out of 100.", "public", id = "b")
  flag <- function(v) {
    i <- sc_accessibility(v)
    i$flag[i$indicator == "relative_risk_without_absolute"]
  }
  expect_true(flag(pub))
  expect_false(flag(spe))
  expect_false(flag(both))
})

test_that("unexplained technical terms are flagged for lay audiences", {
  f <- sc_example()$finding
  v1 <- sc_version("It was a cross-sectional study.", "public")
  v2 <- sc_version("It was a cross-sectional study, which means everything was measured once.", "public", id = "b")
  get <- function(v) sc_accessibility(v, f)
  expect_true(get(v1)$flag[get(v1)$indicator == "technical_terms_unexplained"])
  expect_false(get(v2)$flag[get(v2)$indicator == "technical_terms_unexplained"])
})

test_that("targets drive the flags and can be changed", {
  v <- sc_example()$versions$public
  strict <- sc_targets()
  strict$min[strict$audience == "public" & strict$indicator == "reading_ease"] <- 95
  i <- sc_accessibility(v, targets = strict)
  expect_true(i$flag[i$indicator == "reading_ease_vs_target"])
})

test_that("assessment, printing and reports work", {
  ex <- sc_example()
  a <- sc_assess(ex$finding, ex$versions)
  expect_s3_class(a, "sc_assessment")
  expect_s3_class(a$indicators, "sc_indicators")
  expect_output(print(a), "Flagged indicators")
  expect_output(print(sc_fidelity(ex$versions$public, ex$finding)), "overstated")
  expect_output(print(a$indicators[a$indicators$version == "public", ]), "boosters")
  f <- tempfile(fileext = ".html")
  sc_report(a, file = f)
  html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, '<mark class="causal">', fixed = TRUE)
  expect_match(html, 'class="bad"', fixed = TRUE)
  expect_false(grepl("<script|<link ", html))
  f_es <- tempfile(fileext = ".html")
  exs <- sc_example("es")
  sc_report(exs$finding, exs$versions, file = f_es)
  expect_match(paste(readLines(f_es, encoding = "UTF-8"), collapse = ""), "Fidelidad entre p\u00fablicos")
})

test_that("mixing languages warns", {
  ex <- sc_example()
  v <- sc_version("Un texto en castellano.", "public", lang = "es", id = "es")
  expect_warning(sc_assess(ex$finding, list(ex$versions$specialist, v)), "language")
})
