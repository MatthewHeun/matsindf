# Collapse a "tidy" data frame to matrices in a data frame (`matsindf`)

A "tidy" data frame contains information that can be collapsed into
matrices, including columns for matrix names, row names, column names,
row types, column types, and values (entries in matrices). These column
names are specified as strings by the `matnames`, `rownames`,
`colnames`, `rowtypes`, `coltypes`, and `values` arguments to
`collapse_to_matrices()`, respectively. A `matsindf`-style matrix has
named rows and columns. In addition, `matsindf`-style matrices have
"types" for row and column information, such as "Commodities",
"Industries", "Products", or "Machines". The row and column types for
the `matsindf`-style matrices are stored as attributes on the matrix
(`rowtype` and `coltype`), which can be accessed with the functions
[`matsbyname::rowtype()`](https://matthewheun.github.io/matsbyname/reference/rowtype.html)
and
[`matsbyname::coltype()`](https://matthewheun.github.io/matsbyname/reference/coltype.html).
Row and column types are both respected and propagated by the various
`*_byname` functions of the `matsbyname` package. Use the `*_byname`
functions when you do operations on the `matsindf`-style matrices. The
`matsindf`-style matrices will be stored in a column with same name as
the incoming `values` column. This function is similar to
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html),
which stores data frames into a cell of a data frame. With
`collapse_to_matrices`, matrices are created. This function respects
groups, like
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
(In fact, calls to this function may not work properly unless grouping
is provided. Errors of the form "Error: Duplicate identifiers for rows
..." are usually fixed by grouping `.DF` prior to calling this
function.) The usual approach is to
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
the `matnames` column and any other columns to be preserved in the
output. Note that execution is halted if any of `rownames`, `colnames`,
`rowtypes`, `coltypes`, or `values` is a grouping variable in `.DF`.
`rowtypes` and `coltypes` should be the same for all rows of the same
matrix in `.DF`; execution is halted if that is not the case.
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)ing
the output by `matnames` may be necessary before calculations are done
on the collapsed matrices. See the example.

## Usage

``` r
collapse_to_matrices(
  .DF,
  matnames = "matnames",
  matvals = "matvals",
  rownames = "rownames",
  colnames = "colnames",
  rowtypes = if ("rowtypes" %in% names(.DF)) "rowtypes" else NULL,
  coltypes = if ("coltypes" %in% names(.DF)) "coltypes" else NULL,
  matrix.class = lifecycle::deprecated(),
  matrix_class = c("matrix", "Matrix")
)
```

## Arguments

- .DF:

  the "tidy" data frame

- matnames:

  A string identifying the column in `.DF` containing matrix names for
  matrices to be created. Default is "matnames".

- matvals:

  A string identifying the column in `.DF` containing values to be
  inserted into the matrices to be created. This will also be the name
  of the column in the output containing matrices formed from the data
  in the `matvals` column. Default is "matvals".

- rownames:

  A string identifying the column in `.DF` containing row names for
  matrices to be created. Default is "rownames".

- colnames:

  A string identifying the column in `.DF` containing column names for
  matrices to be created. Default is "colnames".

- rowtypes:

  An optional string identifying the column in `.DF` containing the type
  of values in rows of the matrices to be created. Default is
  `if ("rowtypes" %in% names(.DF)) "rowtypes" else NULL`, so that
  failure to set the rowtypes argument will give `NULL`, as appropriate.

- coltypes:

  An optional string identifying the column in `.DF` containing the type
  of values in columns of the matrices to be created Default is
  `if ("coltypes" %in% names(.DF)) "rowtypes" else NULL`, so that
  failure to set the coltypes argument will give `NULL`, as appropriate.

- matrix.class:

  **\[deprecated\]** Use `matrix_class` instead.

- matrix_class:

  One of "matrix" or "Matrix". "matrix" creates a
  [`base::matrix`](https://rdrr.io/r/base/matrix.html) object with the
  [`matrix()`](https://rdrr.io/r/base/matrix.html) function. "Matrix"
  creates a
  [`Matrix::Matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html) object
  using the
  [`matsbyname::Matrix()`](https://matthewheun.github.io/matsbyname/reference/Matrix.html)
  function. This could be a sparse matrix. Default is "matrix".

## Value

A data frame with matrices in the `matvals` column.

## Details

Groups are not preserved on output.

Note that two types of matrices can be created, a `matrix` or a
`Matrix`. `Matrix` has the advantage of representing sparse matrices
with less memory (and disk space). `Matrix` objects are created by
[`matsbyname::Matrix()`](https://matthewheun.github.io/matsbyname/reference/Matrix.html).

## See also

[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html) and
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
library(tibble)
ptype <- "Products"
itype <- "Industries"
tidy <- data.frame(Country = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",
                                "US",  "US",  "US",  "US", "GH", "US"),
                  Year    = c(  1971,  1971,  1971,  1971,  1971,  1971,  1971,
                                1980,  1980,  1980,  1980, 1971, 1980),
                  matrix  = c(   "U",   "U",   "E",   "E",   "E",   "V",   "V",
                                 "U",   "U",   "E",   "E", "eta", "eta"),
                  row     = c( "c 1", "c 2", "c 1", "c 2", "c 2", "i 1", "i 2",
                               "c 1", "c 1", "c 1", "c 2", NA, NA),
                  col     = c( "i 1", "i 2", "i 1", "i 2", "i 3", "c 1", "c 2",
                               "i 1", "i 2", "i 1", "i 2", NA, NA),
                  rowtypes = c( ptype, ptype, ptype, ptype, ptype, itype, itype,
                                ptype, ptype, ptype, ptype, NA, NA),
                  coltypes = c( itype, itype, itype, itype, itype, ptype, ptype,
                                itype, itype, itype, itype, NA, NA),
                  vals  = c(    11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,
                                11 ,   12 ,   11 ,   22,   0.2, 0.3)
) %>% group_by(Country, Year, matrix)
mats <- collapse_to_matrices(tidy, matnames = "matrix", matvals = "vals",
                             rownames = "row", colnames = "col",
                             rowtypes = "rowtypes", coltypes = "coltypes")
mats %>% pivot_wider(names_from = matrix, values_from = vals)
#> # A tibble: 2 × 6
#>   Country  Year E             U             V             eta      
#>   <chr>   <dbl> <list>        <list>        <list>        <list>   
#> 1 GH       1971 <dbl [2 × 3]> <dbl [2 × 2]> <dbl [2 × 2]> <dbl [1]>
#> 2 US       1980 <dbl [2 × 2]> <dbl [1 × 2]> <NULL>        <dbl [1]>
```
