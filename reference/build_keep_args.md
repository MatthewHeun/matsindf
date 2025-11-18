# Build a list of arguments to keep

In the process of building data frames of arguments to `FUN`, we need to
decide which arguments to keep from each source, `...`, `.dat`, and
defaults to `FUN`. This function does that work in one place.

## Usage

``` r
build_keep_args(where_to_find_args)
```

## Arguments

- where_to_find_args:

  A list created by
  [`where_to_get_args()`](https://matthewheun.github.io/matsindf/reference/where_to_get_args.md).

## Value

A list with names `.dat`, `dots`, and `FUN` which gives items to keep
from each source.
