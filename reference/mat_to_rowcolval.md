# Convert a matrix to a data frame with rows, columns, and values.

This function "expands" a matrix into a tidy data frame with a values
column and factors for row names, column names, row types, and column
types. Optionally, values can be dropped.

## Usage

``` r
mat_to_rowcolval(
  .matrix,
  matvals = "matvals",
  rownames = "rownames",
  colnames = "colnames",
  rowtypes = "rowtypes",
  coltypes = "coltypes",
  drop = NA
)
```

## Arguments

- .matrix:

  The IO-style matrix to be converted to a data frame with rows,
  columns, and values.

- matvals:

  A string for the name of the output column containing values. Default
  is "matvals".

- rownames:

  A string for the name of the output column containing row names.
  Default is "rownames".

- colnames:

  A string for the name of the output column containing column names.
  Default is "colnames".

- rowtypes:

  A string for the name of the output column containing row types.
  Default is "rowtypes".

- coltypes:

  A string for the name of the output column containing column types.
  Default is "coltypes".

- drop:

  If specified, the value to be dropped from output. Default is `NA`.
  For example, `drop = 0` will cause `0` entries in the matrices to be
  deleted from output. If `NA`, no values are dropped from output.

## Value

A data frame with rows, columns, and values.

## Examples

``` r
library(matsbyname)
data <- data.frame(Country  = c("GH", "GH", "GH"),
                   rows = c( "c1",  "c1", "c2"),
                   cols = c( "i1",  "i2", "i2"),
                   rt = c("Commodities", "Commodities", "Commodities"),
                   ct = c("Industries", "Industries", "Industries"),
                   vals = c(  11  ,  12,   22 ))
data
#>   Country rows cols          rt         ct vals
#> 1      GH   c1   i1 Commodities Industries   11
#> 2      GH   c1   i2 Commodities Industries   12
#> 3      GH   c2   i2 Commodities Industries   22
A <- data %>%
  rowcolval_to_mat(rownames = "rows", colnames = "cols",
                   rowtypes = "rt",   coltypes = "ct", matvals = "vals")
A
#>    i1 i2
#> c1 11 12
#> c2  0 22
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
mat_to_rowcolval(A, rownames = "rows", colnames = "cols",
                 rowtypes = "rt", coltypes = "ct", matvals = "vals")
#>   rows cols vals          rt         ct
#> 1   c1   i1   11 Commodities Industries
#> 2   c2   i1    0 Commodities Industries
#> 3   c1   i2   12 Commodities Industries
#> 4   c2   i2   22 Commodities Industries
mat_to_rowcolval(A, rownames = "rows", colnames = "cols",
                 rowtypes = "rt", coltypes = "ct", matvals = "vals", drop = 0)
#>   rows cols vals          rt         ct
#> 1   c1   i1   11 Commodities Industries
#> 3   c1   i2   12 Commodities Industries
#> 4   c2   i2   22 Commodities Industries
# This also works for single values
mat_to_rowcolval(2, matvals = "vals",
                 rownames = "rows", colnames = "cols",
                 rowtypes = "rt", coltypes = "ct")
#>   rows cols vals rt ct
#> 1   NA   NA    2 NA NA
mat_to_rowcolval(0, matvals = "vals",
                 rownames = "rows", colnames = "cols",
                 rowtypes = "rt", coltypes = "ct", drop = 0)
#> [1] rows cols vals rt   ct  
#> <0 rows> (or 0-length row.names)
```
