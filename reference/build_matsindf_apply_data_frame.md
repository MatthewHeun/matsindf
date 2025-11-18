# Create a data frame consisting of the input data for matsindf_apply()

This is an internal helper function that takes the types list and
creates a data frame from which calculations can proceed.

## Usage

``` r
build_matsindf_apply_data_frame(
  .dat = NULL,
  FUN,
  ...,
  types = matsindf_apply_types(.dat, FUN = FUN, ... = ...)
)
```

## Arguments

- .dat:

  The value of the `.dat` argument to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
  as a list or a data frame.

- FUN:

  The function supplied to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

- ...:

  The `...` argument supplied to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

- types:

  The types for
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).
  Supply if already calculated externally. Default is
  `types = matsindf_apply_types(.dat, FUN = FUN, ... = ...)`.

## Value

A data frame (actually, a `tibble`) with columns from `dots`, `.dat`,
and the default values to `FUN`, according to precedence rules for
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

## Details

This function enforces the precedence rules for
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
namely that variables found in `...` take priority over variables found
in `.dat`, which take priority over variables found in the default
values of `FUN`.
