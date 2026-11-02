# scicomm <img src="man/figures/logo.png" align="right" height="139" alt="scicomm logo" />

<!-- badges: start -->
[![CI](https://github.com/pablosancheznunez/scicomm/actions/workflows/ci.yml/badge.svg)](https://github.com/pablosancheznunez/scicomm/actions/workflows/ci.yml)
[![Website](https://img.shields.io/badge/docs-pkgdown-9a3412)](https://pablosancheznunez.com/scicomm/)
<!-- badges: end -->

**Assess fidelity and audience fit in science communication.**

The same scientific result becomes an abstract for specialists, a lesson for students and a news story for the public, and at each step something can be lost or added. scicomm describes the result once, in a structured way, and checks each text against it:

- **fidelity across audiences**: whether each version keeps the direction, numbers, population and limitations of the finding, or overstates it with causal wording for a correlational design, results from mice presented as if they applied to people, intensifiers or boosters such as "proves";
- **clarity**: sentence length, reading ease (Flesch for English, Szigriszt-Pazos/INFLESZ for Spanish), long words, undefined acronyms;
- **accessibility**: reading ease against each audience's target, technical terms used without explanation, relative risks without absolute risks;
- **uncertainty**: hedges and boosters, limitations mentioned, intervals or ranges;
- a **self-contained HTML report** with the comparison and every text with the relevant wording highlighted.

Everything works in **English and Spanish**. The indicators are transparent proxies reported one by one, with no composite score: they point to passages that deserve a careful reading. Judging rigour needs an expert, and showing that readers understand a message needs a study with readers.

**Website:** <https://pablosancheznunez.com/scicomm/> · **Example report:** <https://pablosancheznunez.com/scicomm/example/report.html>

## Installation

```r
# install.packages("remotes")
remotes::install_github("pablosancheznunez/scicomm")
```

scicomm depends only on base R.

## Example

```r
library(scicomm)

finding <- sc_finding(
  claim = "Students who slept less than 7 hours had lower maths scores.",
  direction = "decrease", design = "observational", certainty = "low",
  population = "2,400 secondary school students in one region of Spain",
  effect = "4.2 points lower (95% CI 2.1 to 6.3)", numbers = c(4.2, 2400),
  limitations = c("self-reported data", "cross-sectional design")
)

press <- sc_version("Lack of sleep ruins teenagers' maths results, a new study proves.",
                    audience = "public")

sc_fidelity(press, finding)
#>  ! causal_language  overstated   causal wording: ruins
#>  ! numbers          missing      0 of 2 kept (missing: 4.2, 2400)
#>  ! certainty        overstated   certainty of the evidence: low; boosters: proves, ruins
#>  ...

# Three versions of one finding, in English or Spanish
ex <- sc_example()
sc_compare(ex$finding, ex$versions)
sc_report(ex$finding, ex$versions, file = "scicomm-report.html")
```

## Main functions

| Step | Functions |
|---|---|
| Describe | `sc_finding()`, `sc_version()`, `sc_example()` |
| Assess | `sc_assess()`, `sc_fidelity()`, `sc_clarity()`, `sc_accessibility()`, `sc_uncertainty()`, `sc_readability()` |
| Compare and report | `sc_compare()`, `sc_report()` |
| Configure | `sc_targets()`, `sc_lexicon()` |

## Roadmap

Version 0.2 will add checks of charts made with ggplot2, rating sheets for expert judgement with inter-rater agreement, and narrative structure. Version 0.3 will add tools for studies with readers: comprehension questions drafted from the finding and analysis of comprehension by version and audience.

## Citation

If you use scicomm in your research, please cite it: `citation("scicomm")` gives the reference, and the repository's [CITATION.cff](https://github.com/pablosancheznunez/scicomm/blob/main/CITATION.cff) is read by GitHub's "Cite this repository" button.

## Contributing

Bug reports, new lexicon terms and translations are welcome in the [issues](https://github.com/pablosancheznunez/scicomm/issues). See the [contributing guide](https://github.com/pablosancheznunez/scicomm/blob/main/CONTRIBUTING.md).

## License

MIT © Pablo Sánchez-Núñez. The lexicons are released under CC BY 4.0.
