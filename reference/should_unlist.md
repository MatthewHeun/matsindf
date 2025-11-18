# Tell whether a column can be unlisted

When evaluating each row of a data frame in
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
the result will be a `tibble` with list columns. This function tells
whether a column can be unlisted. This is internal helper function and
should not be called externally.

## Usage

``` r
should_unlist(this_col)
```

## Arguments

- this_col:

  The column to be checked. Or a `data.frame`, in which case every
  column is checked.

## Value

A boolean. `TRUE` if the column can be unlisted, `FALSE` otherwise. When
`this_col` is a `data.frame`, a named boolean vector, one entry for each
column.
