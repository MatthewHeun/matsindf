# Expand a `matsindf` data frame

Any tidy data frame of matrices (in which each row represents one matrix
observation) can also be represented as a tidy data frame with each
non-zero matrix entry as an observation on its own row. This function
and
[`collapse_to_matrices()`](https://matthewheun.github.io/matsindf/reference/collapse_to_matrices.md)
convert between the two representations.

## Usage

``` r
expand_to_tidy(
  .DF,
  matnames = "matnames",
  matvals = "matvals",
  rownames = "rownames",
  colnames = "colnames",
  rowtypes = "rowtypes",
  coltypes = "coltypes",
  drop = NA
)
```

## Arguments

- .DF:

  The data frame containing matsindf-style matrices. (`.DF` may also be
  a named list of matrices, in which case names of the matrices are
  taken from the names of items in the list and list items are expected
  to be matrices.)

- matnames:

  The name of the column in `.DF` containing matrix names (a string).
  Default is "matnames".

- matvals:

  The name of the column in `.DF` containing IO-style matrices or
  constants (a string). This will also be the name of the column
  containing matrix entries in the output data frame. Default is
  "matvals".

- rownames:

  The name for the output column of row names (a string). Default is
  "rownames".

- colnames:

  The name for the output column of column names (a string). Default is
  "colnames".

- rowtypes:

  An optional name for the output column of row types (a string).
  Default is "rowtypes".

- coltypes:

  The optional name for the output column of column types (a string).
  Default is "coltypes".

- drop:

  If specified, the value to be dropped from output, For example,
  `drop = 0` will cause `0` entries in the matrices to be deleted from
  output. If `NA`, no values are dropped from output. Default is `NA`.

## Value

A tidy data frame containing expanded matsindf-style matrices.

## Details

Names for output columns are specified in the `rownames`, `colnames`,
`rowtypes`, and `coltypes`, arguments. The entries of the matsindf-style
matrices are stored in an output column named `values.`

## Examples

``` r
library(dplyr)
library(matsbyname)
ptype <- "Products"
itype <- "Industries"
tidy <- data.frame(Country  = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",
                                 "US",  "US",  "US",  "US", "GH", "US"),
                  Year      = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,
                                 1980,  1980,  1980,  1980, 1971, 1980),
                  matrix    = c(  "U",   "U",   "Y",   "Y",   "Y",   "V",   "V",
                                  "U",   "U",   "Y",   "Y", "eta", "eta"),
                  row       = c(  "c1",  "c2",  "c1",  "c2",  "c2",  "i1",  "i2",
                                  "c1",  "c1",  "c1",  "c2", NA, NA),
                  col       = c(  "i1",  "i2",  "i1",  "i2",  "i3",  "c1",  "c2",
                                  "i1",  "i2",  "i1",  "i2", NA, NA),
                  rowtypes  = c( ptype, ptype, ptype, ptype, ptype, itype, itype,
                                 ptype, ptype, ptype, ptype, NA, NA),
                  coltypes  = c( itype, itype, itype, itype, itype, ptype, ptype,
                                 itype, itype, itype, itype, NA, NA),
                  vals      = c(11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,
                                11 ,   12 ,   11 ,   22,   0.2, 0.3)) %>%
  group_by(Country, Year, matrix)
mats <- collapse_to_matrices(tidy, matnames = "matrix", rownames = "row", colnames = "col",
                             rowtypes = "rowtypes", coltypes = "coltypes",
                             matvals = "vals") %>%
  ungroup()
expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
                     rownames = "rows", colnames = "cols",
                     rowtypes = "rt",   coltypes = "ct")
#> # A tibble: 22 × 8
#>    Country  Year matrix rows  cols   vals rt         ct        
#>    <chr>   <dbl> <chr>  <chr> <chr> <dbl> <chr>      <chr>     
#>  1 GH       1971 U      c1    i1       11 Products   Industries
#>  2 GH       1971 U      c2    i1        0 Products   Industries
#>  3 GH       1971 U      c1    i2        0 Products   Industries
#>  4 GH       1971 U      c2    i2       22 Products   Industries
#>  5 GH       1971 V      i1    c1       11 Industries Products  
#>  6 GH       1971 V      i2    c1        0 Industries Products  
#>  7 GH       1971 V      i1    c2        0 Industries Products  
#>  8 GH       1971 V      i2    c2       22 Industries Products  
#>  9 GH       1971 Y      c1    i1       11 Products   Industries
#> 10 GH       1971 Y      c2    i1        0 Products   Industries
#> # ℹ 12 more rows
expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
                     rownames = "rows", colnames = "cols",
                     rowtypes = "rt",   coltypes = "ct", drop = 0)
#> # A tibble: 13 × 8
#>    Country  Year matrix rows  cols   vals rt         ct        
#>    <chr>   <dbl> <chr>  <chr> <chr> <dbl> <chr>      <chr>     
#>  1 GH       1971 U      c1    i1     11   Products   Industries
#>  2 GH       1971 U      c2    i2     22   Products   Industries
#>  3 GH       1971 V      i1    c1     11   Industries Products  
#>  4 GH       1971 V      i2    c2     22   Industries Products  
#>  5 GH       1971 Y      c1    i1     11   Products   Industries
#>  6 GH       1971 Y      c2    i2     22   Products   Industries
#>  7 GH       1971 Y      c2    i3     23   Products   Industries
#>  8 GH       1971 eta    NA    NA      0.2 NA         NA        
#>  9 US       1980 U      c1    i1     11   Products   Industries
#> 10 US       1980 U      c1    i2     12   Products   Industries
#> 11 US       1980 Y      c1    i1     11   Products   Industries
#> 12 US       1980 Y      c2    i2     22   Products   Industries
#> 13 US       1980 eta    NA    NA      0.3 NA         NA        
```
