---
title: "Release notes for `RCLabels`"
output: html_document
---

# RCLabels 0.1.1 (2022-03-05)

* Added backward compatibility with previous versions of R
  via reverting to the magrittr pipe (%>%) from the system pipe (|>).
* Added new notation type `first_dot_notation`.
* New tests for new capabilities.
    * Now up to 225 tests, all passing.
    * Test coverage remains at 100%.


# RCLabels 0.1.0 (2022-01-03) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5819144.svg)](https://doi.org/10.5281/zenodo.5819144)

* First version to be used by other packages, so bumping to v0.1.0.
* `get_piece(labels = NULL)` now returns `NULL`, as expected.
* All return values from `get_piece()` are now named appropriately.
* Added new options for the `piece` argument of `get_piece()`: "pps", 
  "prepositions", and "objects".
* `RCLabels::prepositions` is now a vector instead of a list,
  thereby making downstream use of the object clearer.
* Breaking changes
    - `keep_pref_suff()` --> `get_pref_suff()` to bring consistency with `get_piece()`.
    - `keep` --> `which` for argument name in `get_pref_suff()`
      to bring consistency with other functions.
* New wrapper function `get_piece()` returns requested piece of a label.
* Added note to README.Rmd about installing from CRAN.
* Added project status badge.
* Added CRAN status badge.
* New tests for new functions.
    * Now up to 216 tests, all passing.
    * Test coverage remains at 100%.


# RCLabels 0.0.4 (2021-12-06)

* New function `replace_by_pattern()`.
* New function `match_by_pattern()`.
* First CRAN release.
* New tests for new functions.
    * 187 tests, all passing.
    * Test coverage remains at 100 %.


# RCLabels 0.0.3

* Added code coverage.
* Added automated spell checking to the package.
* No new tests.
    * 156 tests, all passing.
    * Test coverage remains at 100 %.


# RCLabels 0.0.2

* First release.
* Added GitHub pages site.
* Added a vignette.
* Added extraction functions. 
* Added a `NEWS.md` file to track changes to the package.
* Refactoring many functions out of IEATools.
* Many tests for all features.
    * 156 tests, all passing.
    * Test coverage is at 100 %.


# RCLabels 0.0.1

* First commit.
