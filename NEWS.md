# matsindf 0.3.0 (2019-01-26)

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
