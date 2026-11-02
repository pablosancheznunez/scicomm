# Contributing

Thank you for helping improve scicomm. Bug reports with the text that was misjudged, new lexicon terms, translations and validation studies are especially welcome.

## Running the tests

- `R CMD build . && R CMD check --as-cran scicomm_*.tar.gz`
- or `devtools::check()`.

## Small changes (welcome as pull requests)

- Documentation, examples and the website.
- New terms in `inst/extdata/lexicons.csv`, with an example sentence in the pull request.
- Compatibility fixes, as long as every test still passes unchanged.

## Changes to discuss in an issue first

- The fidelity checks in `R/fidelity.R` and the text processing in `R/text.R`.
- Readability formulas and the default targets in `R/targets.R`.
- New dependencies: scicomm deliberately depends only on base R.

The design notes are in [`dev/DESIGN.md`](dev/DESIGN.md).

Tests are never weakened or deleted to make them pass.
