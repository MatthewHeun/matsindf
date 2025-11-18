# Gracefully handle `NULL` arguments

When `NULL` is passed as an element of the `.dat` or `...` arguments to
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
special care must be taken. This function helps in those situations.

## Usage

``` r
handle_null_args(.arg)
```

## Arguments

- .arg:

  One of `.dat` or `...` (as a list) arguments to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

## Value

A list representation of `.arg` with `NULL` values handled
appropriately.
