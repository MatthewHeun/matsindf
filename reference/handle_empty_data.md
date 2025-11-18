# Gracefully handle empty data

When empty data are provided to
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
care must be take with the return value. This function assembles the
correct zero-row data frame or zero-length lists.

## Usage

``` r
handle_empty_data(.dat = NULL, FUN, DF, types)
```

## Arguments

- .dat:

  The `.dat` argument to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

- FUN:

  The `FUN` argument to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

- DF:

  The assembled `DF` inside
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

- types:

  The `types` object assembled inside
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

## Value

The appropriate return value from
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
either a zero-length list or a zero-row data frame.
