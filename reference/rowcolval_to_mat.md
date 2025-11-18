# Collapse a tidy data frame into a matrix with named rows and columns

Columns not specified in one of `rownames`, `colnames`, `rowtype`,
`coltype`, or `values` are silently dropped. `rowtypes` and `coltypes`
are added as attributes to the resulting matrix (via
[`matsbyname::setrowtype()`](https://matthewheun.github.io/matsbyname/reference/setrowtype.html)
and
[`matsbyname::setcoltype()`](https://matthewheun.github.io/matsbyname/reference/setcoltype.html).
The resulting matrix is a (under the hood) a data frame. If both
`rownames` and `colnames` columns of `.DF` contain `NA`, it is assumed
that this is a single value, not a matrix, in which case the value in
the `values` column is returned.

## Usage

``` r
rowcolval_to_mat(
  .DF,
  matvals = "matvals",
  rownames = "rownames",
  colnames = "colnames",
  rowtypes = "rowtypes",
  coltypes = "coltypes",
  fill = 0,
  matrix.class = lifecycle::deprecated(),
  matrix_class = c("matrix", "Matrix"),
  i_colname = "i",
  j_colname = "j"
)
```

## Arguments

- .DF:

  A tidy data frame containing columns for row names, column names, and
  values.

- matvals:

  The name of the column in `.DF` containing values with which to fill
  the matrix (a string). Default is "matvals".

- rownames:

  The name of the column in `.DF` containing row names (a string).
  Default is "rownames".

- colnames:

  The name of the column in `.DF` containing column names (a string).
  Default is "colnames".

- rowtypes:

  An optional string identifying the types of information found in rows
  of the matrix to be constructed. Default is "rowtypes".

- coltypes:

  An optional string identifying the types of information found in
  columns of the matrix to be constructed. Default is "coltypes".

- fill:

  The value for missing entries in the resulting matrix. default is `0`.

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

- i_colname, j_colname:

  Names of index columns used internally. Defaults are "i" and "j".

## Value

A matrix with named rows and columns and, optionally, row and column
types.

## Details

Note that two types of matrices can be created, a `matrix` or a
`Matrix`. `Matrix` has the advantage of representing sparse matrices
with less memory (and disk space). `Matrix` objects are created by
[`matsbyname::Matrix()`](https://matthewheun.github.io/matsbyname/reference/Matrix.html).

## Examples

``` r
library(matsbyname)
library(dplyr)
data <- data.frame(Country  = c("GH", "GH", "GH"),
                   rows = c( "c 1",  "c 1", "c 2"),
                   cols = c( "i 1",  "i 2", "i 2"),
                   vals = c(   11  ,   12,    22 ))
A <- rowcolval_to_mat(data, rownames = "rows", colnames = "cols", matvals = "vals")
A
#>     i 1 i 2
#> c 1  11  12
#> c 2   0  22
#> attr(,"rowtype")
#> [1] "rowtypes"
#> attr(,"coltype")
#> [1] "coltypes"
rowtype(A) # NULL, because types not set
#> [1] "rowtypes"
coltype(A) # NULL, because types not set
#> [1] "coltypes"
B <- rowcolval_to_mat(data, rownames = "rows", colnames = "cols", matvals = "vals",
                            rowtypes  = "Commodities", coltypes  = "Industries")
B
#>     i 1 i 2
#> c 1  11  12
#> c 2   0  22
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
C <- data %>% bind_cols(data.frame(rt = c("Commodities", "Commodities", "Commodities"),
                                   ct = c("Industries", "Industries", "Industries"))) %>%
  rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
                   rowtypes = "rt", coltypes = "ct")
C
#>     i 1 i 2
#> c 1  11  12
#> c 2   0  22
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
# Also works for single values if both the rownames and colnames columns contain NA
data2 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA),
  rowtypes = c(NA), coltypes = c(NA), vals = c(2))
data2 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
  rowtypes = "rowtypes", coltypes = "coltypes")
#> [1] 2
data3 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA), vals = c(2))
data3 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals")
#> [1] 2
# Fails when rowtypes or coltypes not all same. In data3, column rt is not all same.
data4 <- data %>% bind_cols(data.frame(rt = c("Commodities", "Industries", "Commodities"),
                                       ct = c("Industries", "Industries", "Industries")))
if (FALSE) rowcolval_to_mat(data4, rownames = "rows", colnames = "cols",
                          matvals = "vals", rowtypes = "rt", coltypes = "ct") # \dontrun{}
```
