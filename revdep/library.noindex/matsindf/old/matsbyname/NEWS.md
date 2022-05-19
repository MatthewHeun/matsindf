---
title: "Release notes for `matsbyname`"
output: html_document
---


# matsbyname 0.5.0 (2022-04-01)

* New format for documentation pages,
  including a search function!
* New vignette "Using summarise in matsbyname"
  clarifies issues around ambiguities in functions 
  that use a `...` argument.
* `aggregation-vignette` now includes details on
  using `sum_byname(.summarise = TRUE)` 
  with `dplyr::summarise()`.
* `sum_byname()`, `matrixproduct_byname()`, 
  `hadamardproduct_byname()`, 
  `mean_byname()`, `geometricmean_byname()`, 
  `equal_byname()`, `identical_byname()`, 
  `samestructure_byname()`, and
  `and_byname()` all gain argument `.summarise`
  to signal intention to operate *down* a column
  (`.summarise = TRUE`) or 
  along a list
  (`.summarise = FALSE`).
  The default value is `.summarise = FALSE`,
  thereby maintaining previous behavior.
* New functions `agg_table_to_agg_map()` and
  `agg_map_to_agg_table()`
  assist with manipulating aggregation maps.
* New vignette `aggregation-vignette` demonstrates 
  the new aggregation functions.
* Functions `rename_to_pref_suff_byname()` and 
  `aggregate_to_pref_suff_byname()`
  now route to new functions
  `rename_to_piece_byname()` and
  `aggregate_pieces_byname()`, 
  thereby avoiding code duplication.
  This change may break some code. 
  These functions now return an empty string ("")
  when a suffix is requested and one is not found.
  Previously, these functions returned the entire
  string when a suffix was not found.
* New function `aggregate_pieces_byname()`
  brings the flexibility of the `RCLabels` 
  to `matsbyname`.
* Remove (comment for now) 
  notation functions in `notation.R` 
  that have been moved to `RCLabels`.
* New function `rename_to_piece_byname()` will assist
  with renaming and aggregating
  according to pieces of row and column names.
* New function `vec_from_store_byname()` 
  creates vectors from a matrix 
  (from which row of column names are taken)
  and a vector 
  (which acts as a store of values)
  based on matching of pieces of the labels.
  This new function is made possible by the 
  new `RCLabels` package.
* Notation code moved to new package, `RCLabels`.
* `RCLabels::make_or_pattern()` gains new `pattern_type`, "literal", which 
  returns the `row_col_names` argument unmodified.
* `trim_rows_cols()` gains a `warn_if_a_incomplete` argument. 
  When `TRUE`, a warning is issued if argument `a` is missing entries on `margin`
  that are present in `mat`.
* Many new tests for new features.
  But some functions have been moved to `RCLabels`, 
  so the total number of tests
  has gone down slightly.
    - Now at 1072 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.25 (2021-10-12) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5565352.svg)](https://doi.org/10.5281/zenodo.5565352)

* New notation functions `preposition_notation()`, `from_notation()`, and `of_notation()`.
* Many new tests for new features.
    - Now at 1077 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.24 (2021-10-01) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5545893.svg)](https://doi.org/10.5281/zenodo.5545893)

* Added a test to trigger errors when dimnames are `NULL`.
* Fixed a bug where a 0x0 matrix was not being completed 
  by another matrix.
* New function `trim_rows_cols()` eliminates rows and/or columns
  in one matrix based on another.
* Many new tests for new features.
    - Now at 1057 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.23 (2021-09-01)  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5392263.svg)](https://doi.org/10.5281/zenodo.5392263)

* `hatize_byname()` now allows a missing `keep` argument, 
  eliminating a regression in reverse dependency
  with the `matsindf` package.
* This release is for CRAN.
* One test could be deleted, because the `hatize_byname()` function is now simpler.
    - Now at 1039 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.22 (2021-07-26)  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5138201.svg)](https://doi.org/10.5281/zenodo.5138201)

* Now issuing a helpful warning when `hatize_byname()` is called with a `keep` argument that 
  is different from the structure of the vector.
  This will be a safe way to encourage callers to specify their expectations
  in the function call.
* Note this version was not released to CRAN, due to frequent revisions.
* New tests for new features.
    - Now up to 1040 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.21 (2021-07-23)  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5129356.svg)](https://doi.org/10.5281/zenodo.5129356)

* `hatize_byname()` and `hatinv_byname()` gain a new 
  argument `keep` that tells whether to keep row names or column names
  when a 1x1 matrix is supplied.
  This feature assists with code that may occasionally encounter
  1x1 vectors as input.
* Note this version was not released to CRAN, due to frequent revisions.
* New tests for new feature.
    - Now up to 1030 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.20 (2021-07-19)  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5118873.svg)](https://doi.org/10.5281/zenodo.5118873)

* New function `keep_pref_suff()` keeps prefixes or suffixes of individual strings
  or lists of strings, based on the `notation` provided.
* New tests for new functions.  
    - Now up to 1025 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.19 (2021-07-17)

* Breaking change: 
  New logic for situations where prefix or suffix is not found
  in `split_pref_suff()`. 
  Previously, `NULL` was returned for a missing prefix or suffix.
  Now, an empty string (`""`) is returned.
* New tests for whether `split_pref_suff()` works in a data frame.
* New tests for new logic.
    - Now up to 1009 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.18 (2021-06-02)

* New function `kvec_from_template_byname()` that creates a row or column
  vector from a template matrix.
* New function `create_colvec_byname()` builds on 
  `create_matrix_byname()`.
* New function `create_rowvec_byname()` builds on 
  `create_matrix_byname()`.
* New function `create_matrix_byname()` that behaves
  much like `matrix()` with "byname" characteristics.
* New tests for new functions.  
    - Now up to 1000 tests, all passing.
    - Test coverage remains at 100 %.


# matsbyname 0.4.17 (2021-04-10)

* Transition to GitHub actions for continuous integration.
* No new tests.
    * Still at 906 tests, all passing.
    * Test coverage remains at 100 %.


# matsbyname 0.4.16 (2020-11-25)

* Moved URLs to `https://` where appropriate.
* Fixed a bug in `rename_to_pref_suff_byname()` 
  where a column vector would fail with "subscript out of bounds" error.
  The fix was to wrap the return list containing "pref" and "suff" in a list, 
  as the rest of the function expected, 
  when a single row was present.
* Fixed a bug in `colsums_byname()` and `rowsums_byname()`
  where setting the `rowname` or `colname` argument to `NULL`
  did not result in an empty row name or column name.
* Fixed a bug in `hatize_byname()` where a 1x1 vector
  gave error:
  `length of 'dimnames' [1] not equal to array extent`.
  The solution is to check for 1x1 vectors and act accordingly.
* Fixed a warning emitted from `stringi`.
* New tests for bug fixes.  
  Now up to 906 tests, all passing.
* Test coverage remains at 100 %.


# matsbyname 0.4.15 (2020-05-29)

* Added additional tests for new features.
    * Now up to 900 tests, all passing.
    * Test coverage remains at 100%.
* Added `tol` argument to `clean_byname()`, allowing for 
  machine precision issues to be addressed.
* Ensured that all old functions, such as
  `rename_to_pref_suff_byname()`, 
  `aggregate_byname()`, and 
  `aggregate_to_pref_suff_byname()`
  respect notation when using notation to renaming 
  rows and columns. 
* New functions for manipulating names of rows and columns:
  `notation_vec()`, 
  `arrow_notation()`, 
  `paren_notation()`,
  `bracket_notation()`,
  `split_pref_suff()`,
  `join_pref_suff()`,
  `flip_pref_suff()`,
  `switch_notation()`, and
  `switch_notation_byname()`.
* Eliminated a warning in GitHub actions about `README.md` requiring
  a nonempty `<title>` element.    


# matsbyname 0.4.14 (2020-05-01)

* Added additional tests for bug fixes and new features.
    * Now up to 829 tests, all passing.
    * Test coverage remains at 100%.
* Enhanced `prep_vector_arg()` to duplicate matrices when present as the vector_arg.
* Better error messages for `sort_rows_cols()`. 
  Now telling which row or column names are duplicates.
* Added function `aggregate_pref_suff_byname()` that combines
  `rename_to_pref_suff_byname()` and 
  `aggregate_byname()`.
* Fixed a crashing bug that appeared when `aggregate_byname()` collapsed all rows or columns 
  into a single row or single column or both.
* Added new function `aggregate_byname()` which
  aggregates rows, columns, or both, according to an `aggregation_map`.
  I wanted to add this function for a long time, and I finally found a reason, 
  namely the need to aggregate by prefixes or suffixes in the `IEATools` package.
  Furthermore, the `aggregation_map` idea seems to be solid.
  Note that `aggregation_map = NULL` (the default) aggregates
  rows with same names and columns with same names.
* Added function `rename_rowcol_to_pref_suff_byname()` which 
  renames rows or columns to prefixes or suffixes in row and column names.
* Fixed a bug in `clean_byname()` which caused 
  a `NULL` response when unnamed rows or columns were present.
* Now using new `prepare_.FUNdots()` function in all `*apply_byname()` functions.
* Refactored new code for `unaryapply_byname()` into function `prepare_.FUNdots()`, 
  so it can be used in other `*apply_byname()` functions.


# matsbyname 0.4.13 (2020-04-17)

* Added additional tests for bug fixes.
    * Now up to 766 tests, all passing.
    * Code coverage remains at 100%.
* Fixed a bug in `unaryapply_byname()`, 
  which was not correctly handling a rectangular two-dimensional list of arguments to `FUN` supplied in  `.FUNdots`.
  A rectangular two-dimensional list of arguments in `.FUNdots` is now interpreted as follows:
  * First dimension contains named arguments to `FUN`.
  * Second dimension contains unique values of the named arguments
    to be applied along the main argument `a`.

  The length of the first dimension of `.FUNdots` is the number of arguments supplied to `FUN`.
  The length of the second dimension of `.FUNdots` must be equal to the length of `a`.


# matsbyname 0.4.12 (2020-03-21)

* Maintenance to prepare for `dplyr` 1.0.0.
  Several tests and examples in `matsbyname` needed a column of a data frame 
  constructed with `I()`.
  `dplyr::group_by()` now requires all groups to have same type, 
  but that wasn't true in some tests, as some entries were `I<list>`
  (items in groups with more than one member)
  and others were `list`
  (items in single-item groups). 
  The solution was to modify two test to
  (a) move from `data.frame` to `tibble` 
  when creating the data frames for testing and
  (b) eliminate the use of `I()`, as 
  tibble is friendly to list columns. 
* Added new function `matricize_byname()` that converts a column (or row) vector into a matrix.
  `matricize_byname()` is the inverse of `vectorize_byname()`.
* Added new function `vectorize_byname` that converts a matrix into a column vector.
* Added section to vignette about `matsindf`.
  This section could be re-added now that `matsindf` is now on CRAN.


# matsbyname 0.4.11 (2019-12-04)

* Maintenance release to get ready for R4.0.0.
  `matrix` objects now inherit from both `matrix` and `array`.
  Thus, code should no longer assume that `class(A)` returns an object of length 1 when `A` is a `matrix`.
  So, I eliminated all instances of `class(A) == "matrix"` in `if` statements
  in favor of `inherits(A, "matrix")`.
  See https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html
  for more details.


# matsbyname 0.4.10 (2019-02-16)

* Added CRAN installation instructions to README.Rmd, now that the package is on CRAN.
* Added CITATION file. `citation("matsbyname")` now gives useful information.
* Fixed a bug in `matrixproduct_byname` 
  in which row and column types were not set correctly when one
  operand was a `matrix` and the other operand was `NA`.


# matsbyname 0.4.9 (2019-01-17)

* Improved LICENSE file for submission to CRAN.
* First version to appear on CRAN.
* Added CRAN and lifecycle badges.


# matsbyname 0.4.8 (2019-01-16)

* Improved cran-comments.md for submission to CRAN.


# matsbyname 0.4.7 (2019-01-07)

* Cleaned up dependencies for testing.


# matsbyname 0.4.6 (2019-01-07)

* Now all external function calls are fully qualified. 


# matsbyname 0.4.5 (2019-01-07)

* New function `elementapply_byname()` applies a function to an element
  of a matrix specified by `row` and `col` arguments.
* Breaking changes: 
    * `elementproduct_byname()` changed to `hadamardproduct_byname()`
      to avoid name collision with `elementapply_byname()`.
    * `elementquotient_byname()` changed to `quotient_byname()`.
    * `elementpow_byname()` changed to `pow_byname()`.
    * `elementexp_byname()` changed to `exp_byname()`.


# matsbyname 0.4.4 (2019-01-02)

* Added tests to achieve 100% code coverage.


# matsbyname 0.4.3 (2019-01-02)

* `complete_rows_cols()` is now agnostic about the order of columns in `fillrow`
  and the order of rows in `fillcol`.


# matsbyname 0.4.2 (2019-01-02)

* `sort_rows_cols()` now allows entries in roworder and colorder 
  that are not presently names of rows or columns. 
  Extraneous names are silently ignored.


# matsbyname 0.4.1 (2019-01-01)

* Adding code coverage badge.


# matsbyname 0.4.0 (2018-12-27)

* Attempted first release to CRAN. (Failed.)


# matsbyname 0.3.8 (2018-12-21)

* `fractionize_byname()` now correctly handles non-square matrices.


# matsbyname 0.3.7 (2018-12-02)

* `hatinv_byname()` now handles `0` values in input vectors gracefully.
  By default, `0` values become `.Machine$double.xmax`.  
  To choose a different value, set new argument `inf_becomes` to a numerical value.
  To suppress default behavior, set `inf_becomes = NULL`.


# matsbyname 0.3.6 (2018-11-25)

* `iszero_byname()` now checks if values of `abs(a)` are `<= tol`.
   (Previously, `iszero_byname()` tested with `< tol`.)
   This change allows the zero matrix to pass the test when `tol = 0`, 
   as we would want.
* Reverted `equal_byname()` to use `isTRUE(all.equal())` when checking for equality.
* New function `identical_byname()` checks for exact equality using `identical`.
* Now up to 672 tests.


# matsbyname 0.3.5 (2018-11-18)

* Now using `identical()` instead of `isTRUE(all.equal())` for `equal_byname()` function.


# matsbyname 0.3.4 (2018-11-18)

* Added new function `hatinv_byname()`.
* Documented defaults for arguments to `count_*` functions.
* Now importing pipe operator from magrittr package at global level


# matsbyname 0.3.3 (2018-10-29)

* Fix version number on pkgdown website.
* Updated many details of pkgdown website for better user navigation.


# matsbyname 0.3.2 (2018-10-29)

* First release to CRAN didn't work.
* Added online documentation at github with pkgdown.


# matsbyname 0.3.1 (2018-08-25)

* Updated to new version of Roxygen which changed line breaks in some .Rd files.
* First release to CRAN.


# matsbyname 0.3.0 (2018-06-20)

* Removed parallelism features introduced in v0.2.6.
  Detailed timings revealed that the parallel code was slower than single-thread code.
  This topic may be revisited in the future. 
  But for now, it is best to remove the multicore code.
  So there are no longer any `mc.cores` arguments to `matsbyname` functions.


# matsbyname 0.2.9 (2018-05-24)

* Beginnings of S3 class `matbyname`. 
  Not sure if I want to keep it.
* Fixed an argument name error exposed by check.


# matsbyname 0.2.8 (2018-05-17)

* New functions `all_byname()` and `any_byname()` make logical tests easy.
* New function `replaceNaN_byname()` replaces `NaN` entries with a value (default is 0).


# matsbyname 0.2.7 (2018-04-15)

* Refactored most `*col*_byname` functions to call their respective `*row*_byname` functions
  with a transposed argument, thereby simplifying code.
* Fixed a bug caused by the above refactoring.
  In `select_cols_byname`, a `NULL` result terminated the executing thread.
* Added new function `replaceNaNWith0`.
* Added new functions `count_vals_byname`, `count_vals_inrows_byname`, and 
  `count_vals_incols_byname` that return the number of matrix entries
  that meet a criterion for the entire matrix, in each row, and in each column, respectively.
* Improvements to documentation.
* Now up to 646 passing tests.


# matsbyname 0.2.6 (2018-03-16)

* New multicore functionality available in most `*_byname` functions.
    - New functions `set_mc_cores` and `get_mc_cores` to set and get package-wide `mc.cores` variable.
      Default is `1`, so all functions work as previously unless `mc.cores` is more than `1`.
    - Alternatively, specify the `mc.cores` argument of any function 
      to specify the number of cores to be used for an individual calculation.
      Default is `get_mc_cores()`.
      A useful approach is to `set_mc_cores(detectCores(logical = FALSE))`.


* Suggested usage 
    - `sum_byname(list(1,2,3,4), list(1,2,3,4), mc.cores = 4)` to send each sum to a different core.
    - `set_mc_cores(4L); sum_byname(list(1,2,3,4), list(1,2,3,4), mc.cores = 4); set_mc_cores(1L)` 
     to do the same thing and set the package-wide value back to `1`.


# matsbyname 0.2.5 (2018-03-13)

* New `*apply_byname` functions enable API improvements
    - These are API changes, but they shouldn't affect any existing code,
      because calls to binary functions will "just work."
    - `naryapply_byname`: enables `...` arguments
    - `naryapplylogical_byname`: enables logical functions
    - Add `...` arguments for functions that deserve them.


* New functions with `...` arguments including
    - `sum_byname`
    - `matrixproduct_byname`
    - `elementproduct_byname`
    - `mean_byname`
    - `geometricmean_byname`
    - `equal_byname`
    - `samestructure_byname`


* New `and_byname(...)` function that provides logical and "by name."
    - The infrastructure is in place to 
      add other logical functions in the future: `or_byname`, `xor_byname`, and `not_byname`.


# matsbyname 0.2.4 (2018-03-08)

* Preparing for submission to CRAN. 
  Changed many variable names in the APIs to standardize on "a" and "b"
  as names for matrix or list of matrices arguments.


# matsbyname 0.2.3 (2018-03-08)

* Eliminate dependence (temporarily) on `matsindf`. 
  Doing so allows `matsbyname` to be submitted first to CRAN.
  Also, Travis builds are now successful, having eliminated the circular dependence between
  `matsbyname` and `matsindf`.


# matsbyname 0.2.2 (2018-03-02)

* New function `elementpow_byname` raises all elements of a matrix to a power.


# matsbyname 0.2.1 (2018-02-28)

* `complete_rows_cols` now accepts `fillrow` and `fillcol` arguments.
  These arguments can be used (instead of the `fill` argument) 
  to specify the values of filled rows and columns when completing a matrix.
  When conflicts arise, precedence among the `fill*` arguments is 
  `fillrow` then `fillcol` then `fill`.


# matsbyname 0.2.0 (2018-02-23)

* Name change to `matsbyname`.


# byname 0.1.9 (2018-02-14)

* Now preserving names of list items in `*apply_byname` functions.


# byname 0.1.8 (2018-02-14)

* Added `applybyname` vignette.


# byname 0.1.7 (2018-02-14)

* `unaryapply_byname` and `binaryapply_byname` now have `.FUNdots` arguments
  through which arguments to `FUN` should be passed. 
  Use of the `...` argument is no longer possible.
  `...` is reserved for future changes to allow an unlimited number of arguments
  to some functions, such as `sum_byname`.
* The implementation of the `.FUNdots` argument fixed a bug 
  where calculations were incorrect when 
  lists of matrices were stored in cells of a data frame. 
  Distribution of arguments (such as `margin = c(1, 2)`) across rows of a data frame
  was not happening properly.


# byname 0.1.6 (2018-02-08)

* New functions `cumsum_byname`, `cumprod_byname`, and `cumapply_byname`.
* Miscellaneous improvements to documentation of many functions.


# byname 0.1.5 (2018-02-01)

* New functions `elementlog_byname` and `elementexp_byname`.


# byname 0.1.4 (2018-01-31)

* New functions `unaryapply_byname` and `binaryapply_byname`.
  These functions have a `FUN` argument that allows an arbitrary function to be 
  applied `_byname` to matrices or data frames containing matrices.
   + `unaryapply_byname` is for unary functions such as `rowsums_byname`.
   + `binaryapply_byname` is for binary functions such as `sum_byname`.
* `unaryapply_byname` and `binaryapply_byname` are used by all `_byname` functions internally.
* Now conducting 412 tests across the entire package. 
  All tests are passing, indicating that the `unaryapply` and `binaryapply` functions are 
  very solid.


# byname 0.1.3 (2018-01-27)

* Fixed a vector vs. list bug that caused failure of binary `_byname` functions when one argument
  was a list and the other was a non-constant numeric vector. 
* Simplified `complete_rows_cols_byname`. It no longer takes a `names` argument.
* Various other fixes.


# byname 0.1.2 (2018-01-23)

* Added the following functions:
   + `mean_byname`: returns the arithmetic mean of corresponding entries of two matrices
   + `geometricmean_byname`: returns the geometric mean of corresponding entries of two matrices
   + `logarithmicmean_byname`: returns the logarithmic mean of corresponding entries of two matrices
* Fixed a bug whereby calling `setrownames_byname` and `setcolnames_byname` on a constant would fail.
  It now produces a 1x1 matrix with named rows or columns.
* Miscellaneous improvements to documentation of many functions.
   

# byname 0.1.1 (2018-01-21)

* Added the following functions: 
   + `rowprod_byname`: returns a column vector with row products (product of all entries in a row)
   + `colprod_byname`: returns a row vector with column products (product of all entries in a column)
   + `prodall_byname`: returns a numeric of the product of all entries in a matrix
* Miscellaneous improvements to documentation of many functions.


# byname 0.1

Initial version.
