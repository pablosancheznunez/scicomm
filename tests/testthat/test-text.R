test_that("English syllables are counted with simple rules", {
  s <- function(w) scicomm:::syllables_en(w)
  expect_equal(s("cat"), 1)
  expect_equal(s("table"), 2)
  expect_equal(s("make"), 1)
  expect_equal(s("beautiful"), 3)
  expect_equal(s("hours"), 1)
})

test_that("Spanish syllables handle diphthongs, hiatus and silent u", {
  s <- function(w) scicomm:::syllables_es(w)
  expect_equal(s("casa"), 2)
  expect_equal(s("murci\u00e9lago"), 4)
  expect_equal(s("pa\u00eds"), 2)
  expect_equal(s("ciudad"), 2)
  expect_equal(s("poeta"), 3)
  expect_equal(s("queso"), 2)
  expect_equal(s("guitarra"), 3)
})

test_that("sentences are split without breaking abbreviations or decimals", {
  s <- scicomm:::sentences("Results (e.g. 4.2 points) were lower. Smith et al. agree! Do they?")
  expect_length(s, 3)
  expect_match(s[1], "4.2 points", fixed = TRUE)
})

test_that("thousands separators depend on the language", {
  expect_equal(scicomm:::numbers_in("2,400 students and 4.2 points", "en"), c(2400, 4.2))
  expect_equal(scicomm:::numbers_in("2.400 estudiantes y 4,2 puntos", "es"), c(2400, 4.2))
})

test_that("negated causal wording is not counted", {
  f <- scicomm:::find_terms
  expect_length(f("This does not prove that sleep causes better marks.", "en", "causal", negated = FALSE), 0)
  expect_length(f("Sleep causes better marks.", "en", "causal", negated = FALSE), 1)
  expect_length(f("No demuestra que el sue\u00f1o provoca mejores notas.", "es", "causal", negated = FALSE), 0)
})

test_that("intervals are recognised in both languages", {
  h <- scicomm:::has_interval
  expect_true(h("a difference of 4.2 (95% CI 2.1 to 6.3)", "en"))
  expect_true(h("probably between 2 and 6 points", "en"))
  expect_false(h("about 4 points lower", "en"))
  expect_true(h("probablemente entre 2 y 6 puntos", "es"))
  expect_true(h("IC del 95%: 2,1 a 6,3", "es"))
})

test_that("acronyms spelled out in brackets count as defined", {
  u <- scicomm:::undefined_acronyms
  expect_equal(u("A randomised controlled trial (RCT) showed it. The RCT was small."), character(0))
  expect_equal(u("The RCT was small."), "RCT")
})

test_that("readability is in a plausible range and has a band", {
  easy <- sc_readability("The cat sat on the mat. It was a sunny day.")
  hard <- sc_readability(paste("Notwithstanding methodological heterogeneity, the multilevel",
                               "longitudinal investigation demonstrated considerable",
                               "interindividual variability in neurocognitive performance."))
  expect_gt(easy$reading_ease, hard$reading_ease)
  expect_equal(hard$band, "very difficult")
  es <- sc_readability("El gato duerme en casa. Hace sol.", lang = "es")
  expect_true(es$band %in% c("bastante f\u00e1cil", "muy f\u00e1cil"))
})

test_that("the lexicons are complete in both languages", {
  for (lang in c("en", "es")) {
    cats <- unique(sc_lexicon(lang)$category)
    expect_true(all(c("hedge", "booster", "causal", "associative", "nonhuman", "limitation",
                      "increase", "decrease", "relative_risk", "absolute_risk",
                      "explanation", "intensifier", "no_effect") %in% cats), info = lang)
  }
})
