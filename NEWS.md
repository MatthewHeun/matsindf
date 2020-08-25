# matsindf 0.3.6 (2020-08-24)

* To comply with a deprecating change in `dplyr`, 
  all calls to `dplyr::group_by()` now use `.add` argument
  instead of `add` argument.


# matsindf 0.3.5 (2020-04-13)

* Now using GitHub actions for continuous integration
  and building the pkgdown site.


# matsindf 0.3.4 (2020-03-21)

* Maintenance release to prepare for `dplyr` 1.0.0.
  Several tests in `matsindf` assumed that some `dplyr` functions returned 
  `data.frame`s.
  Now that many `dplyr` functions return `tibble`s, the `matsindf` tests needed to be rewritten.
  All tests pass vs. `dplyr` 0.8.5.
  One warning remains when testing `matsindf` with `dplyr` 1.0.0,
  namely that the `add` argument of `group_buy()` is deprecated.
  When `dplyr` 1.0.0 is released to CRAN, 
  I will convert `add` to `.add`, per the `dplyr` authors' recommendation.


# matsindf 0.3.3 (2020-03-03)

* More maintenance to prepare for R4.0.0.
  Prior to R4.0.0, `data.frame()` has `stringsAsFactors = TRUE` by default.
  In R4.0.0, `stringsAsFactors = FALSE` will be the default.
  In one test ("small example works as expected"), 
  I was relying on the current behavior (`stringsAsFactors = TRUE`).
  That reliance has been removed so that this test will also pass under R4.0.0.


# matsindf 0.3.2 (2019-12-05)

* Maintenance release to prepare for R4.0.0.
  `matrix` objects now inherit from both `matrix` and `array`.
  Thus, code should no longer assume that `class(A)` returns an object of length 1 when `A` is a `matrix`.
  So, I eliminated all instances of `class(A) == "matrix"` in `if` statements
  in favor of `inherits(A, "matrix")`.
  See https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html
  for more details.


# matsindf 0.3.1 (2019-10-26)

* Refactored `group_by_everything_except()` to use a new helper function `everything_except()`
* New function `group_by_everything_except()`
* Added CRAN badge.


# matsindf 0.3.0 (2019-02-05)

* Address questions from CRAN: 
      - Title case: Matrices in Data Frames
      - `\dontrun{}` --> `\donttest{}` in Roxygen examples 
* Fully-qualified all function calls in `matsindf` functions.
* Now importing the `:=` and `.data` at a high level.
* Added TravisCI integration
* Added code coverage


# matsindf 0.2.12 (2019-01-07)

* Now importing the `magrittr` pipe at a high level.


# matsindf 0.2.11 (2018-11-13)

* Now creating README.md from README.Rmd for `pkgdown` documentation.


# matsindf 0.2.10 (2018-11-08)

* Breaking change: Default values of `rowtypes` and `coltypes` arguments to 
  `expand_to_tidy` and `collapse_to_matrices` functions are no longer `NULL`.
  Rather, they are "`rowtypes`" and "`coltypes`".


# matsindf 0.2.9 (2018-11-06)

* Extended the `matsindf_apply` primer to include sections on using `matsindf_apply` with a data frame and programming.


# matsindf 0.2.8 (2018-11-01)

* Added new vignette to documentation website, 
  which required moving from .Rnw to .Rmd file.


# matsindf 0.2.7 (2018-10-30)

* Added documentation website.


# matsindf 0.2.6 (2018-07-03)

* New function `index_column` that ratios both numbers and matrices relative to an initial time.
* Many documentation improvements.


# matsindf 0.2.5 (2018-05-23)

* `expand_to_tidy` now accepts a named list of matrices as input.


# matsindf 0.2.4 (2018-05-22)

* `matsindf_apply` now obtains named arguments from `.DF` 
  when a name in `.DF` matches a name of an argument to `FUN`.
  This behavior is overridden by supplying a string argument in `...` 
  of `matsindf_apply`.


# matsindf 0.2.3 (2018-05-22)

* `matsindf_apply` now accepts a list as input when arguments are all strings.


# matsindf 0.2.2 (2018-05-20)

* New `matsindf_apply` function.
* All functions now use `matsindf_apply`.


# matsindf 0.2.1 (2018-02-23)

* Now uses renamed `matsbyname` package.


# matsindf 0.2 (2018-02-21)

* Initial version.
