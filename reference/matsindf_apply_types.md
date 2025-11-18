# Determine types of `.dat` and `...` arguments for matsindf_apply()

This is a convenience function that returns a list for the types of
`.dat` and `...` as well as names in `.dat` and `...`, with components
named `.dat_null`, `.dat_df`, `.dat_list`, `.dat_names`,
`FUN_arg_all_names`, `FUN_arg_default_names`, `FUN_arg_default_values`,
`dots_present`, `all_dots_num`, `all_dots_mats`, `all_dots_list`,
`all_dots_vect`, `all_dots_char`, `all_dots_longer_than_1`,
`dots_names`, and `keep_args`.

## Usage

``` r
matsindf_apply_types(.dat = NULL, FUN, ..., .warn_missing_FUN_args = TRUE)
```

## Arguments

- .dat:

  The `.dat` argument to be checked.

- FUN:

  The function sent to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

- ...:

  The list of arguments to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
  to be checked.

- .warn_missing_FUN_args:

  A boolean that tells whether to warn of missing arguments to `FUN`.
  Default is `TRUE`.

## Value

A logical list with components named `.dat_null`, `.dat_df`,
`.dat_list`, `.dat_names`, `FUN_arg_all_names`, `FUN_arg_default_names`,
`FUN_arg_default_values`, `dots_present`, `all_dots_num`,
`all_dots_mats`, `all_dots_list`, `all_dots_vect`, `all_dots_char`,
`all_dots_longer_than_1`, `dots_names`, and `keep_args`.

## Details

When `.dat` is a `data.frame`, both `.dat_list` and `.dat_df` are
`TRUE`.

When arguments are present in `...`, `dots_present` is `TRUE` but
`FALSE` otherwise. When all items in `...` are single numbers,
`all_dots_num` is `TRUE` and all other list members are `FALSE`. When
all items in `...` are matrices, `all_dots_mats` is `TRUE` and all other
list members are `FALSE`. When all items in `...` are lists,
`all_dots_list` is `TRUE` and all other list members are `FALSE`. When
all items in `...` are vectors (including lists), `all_dots_vect` is
`TRUE`. When all items in `...` have length \> 1,
`all_dots_longer_than_1` is `TRUE`. When all items in `...` are
character strings, `all_dots_char` is `TRUE` and all other list members
are `FALSE`.

The various `FUN_arg_*` components give information about the arguments
to `FUN`. `FUN_arg_all_names` gives the names of all arguments to `FUN`,
regardless of whether they have default values. `FUN_arg_default_names`
gives the names of only those arguments with default values.
`FUN_arg_default_values` gives the values of the default arguments,
already [`eval()`](https://rdrr.io/r/base/eval.html)ed in the global
environment. When there are no values in a category, `NULL` is returned.
thus, if `FUN` has no arguments with default values assigned in the
signature of the function, both `FUN_arg_default_names` and
`FUN_arg_default_values` will be `NULL`. If `FUN` has no arguments, all
of `FUN_arg_all_names`, `FUN_arg_default_names` and
`FUN_arg_default_values` will be `NULL`.

`keep_args` is a named [`list()`](https://rdrr.io/r/base/list.html) of
arguments, which indicates which arguments to keep from which source
(`...`, `.dat`, or default args to `FUN`) by order of preference, `...`
over `.dat` over default arguments to `FUN`. Arguments not used by `FUN`
are kept, again according to the rules of preference.

## Examples

``` r
identity_fun <- function(a, b) {list(a = a, b = b)}
matsindf_apply_types(.dat = NULL, FUN = identity_fun, a = 1, b = 2)
#> $.dat_null
#> [1] TRUE
#> 
#> $.dat_df
#> [1] FALSE
#> 
#> $.dat_list
#> [1] FALSE
#> 
#> $.dat_names
#> NULL
#> 
#> $FUN_arg_all_names
#> [1] "a" "b"
#> 
#> $FUN_arg_default_names
#> NULL
#> 
#> $FUN_arg_default_values
#> NULL
#> 
#> $dots_present
#> [1] TRUE
#> 
#> $all_dots_num
#> [1] TRUE
#> 
#> $all_dots_mats
#> [1] FALSE
#> 
#> $all_dots_list
#> [1] FALSE
#> 
#> $all_dots_vect
#> [1] TRUE
#> 
#> $all_dots_char
#> [1] FALSE
#> 
#> $all_dots_longer_than_1
#> [1] FALSE
#> 
#> $dots_names
#> [1] "a" "b"
#> 
#> $keep_args
#> $keep_args$.dat
#> NULL
#> 
#> $keep_args$FUN
#> NULL
#> 
#> $keep_args$dots
#>   a   b 
#> "a" "b" 
#> 
#> 
matsindf_apply_types(.dat = data.frame(), FUN = identity_fun,
                     a = matrix(c(1, 2)), b = matrix(c(2, 3)))
#> $.dat_null
#> [1] FALSE
#> 
#> $.dat_df
#> [1] TRUE
#> 
#> $.dat_list
#> [1] TRUE
#> 
#> $.dat_names
#> character(0)
#> 
#> $FUN_arg_all_names
#> [1] "a" "b"
#> 
#> $FUN_arg_default_names
#> NULL
#> 
#> $FUN_arg_default_values
#> NULL
#> 
#> $dots_present
#> [1] TRUE
#> 
#> $all_dots_num
#> [1] FALSE
#> 
#> $all_dots_mats
#> [1] TRUE
#> 
#> $all_dots_list
#> [1] FALSE
#> 
#> $all_dots_vect
#> [1] FALSE
#> 
#> $all_dots_char
#> [1] FALSE
#> 
#> $all_dots_longer_than_1
#> [1] FALSE
#> 
#> $dots_names
#> [1] "a" "b"
#> 
#> $keep_args
#> $keep_args$.dat
#> NULL
#> 
#> $keep_args$FUN
#> NULL
#> 
#> $keep_args$dots
#>   a   b 
#> "a" "b" 
#> 
#> 
matsindf_apply_types(.dat = list(), FUN = identity_fun,
                     a = c(1, 2), b = c(3, 4))
#> $.dat_null
#> [1] FALSE
#> 
#> $.dat_df
#> [1] FALSE
#> 
#> $.dat_list
#> [1] TRUE
#> 
#> $.dat_names
#> NULL
#> 
#> $FUN_arg_all_names
#> [1] "a" "b"
#> 
#> $FUN_arg_default_names
#> NULL
#> 
#> $FUN_arg_default_values
#> NULL
#> 
#> $dots_present
#> [1] TRUE
#> 
#> $all_dots_num
#> [1] TRUE
#> 
#> $all_dots_mats
#> [1] FALSE
#> 
#> $all_dots_list
#> [1] FALSE
#> 
#> $all_dots_vect
#> [1] TRUE
#> 
#> $all_dots_char
#> [1] FALSE
#> 
#> $all_dots_longer_than_1
#> [1] TRUE
#> 
#> $dots_names
#> [1] "a" "b"
#> 
#> $keep_args
#> $keep_args$.dat
#> NULL
#> 
#> $keep_args$FUN
#> NULL
#> 
#> $keep_args$dots
#>   a   b 
#> "a" "b" 
#> 
#> 
matsindf_apply_types(.dat = NULL, FUN = identity_fun,
                     a = list(1, 2), b = list(3, 4))
#> $.dat_null
#> [1] TRUE
#> 
#> $.dat_df
#> [1] FALSE
#> 
#> $.dat_list
#> [1] FALSE
#> 
#> $.dat_names
#> NULL
#> 
#> $FUN_arg_all_names
#> [1] "a" "b"
#> 
#> $FUN_arg_default_names
#> NULL
#> 
#> $FUN_arg_default_values
#> NULL
#> 
#> $dots_present
#> [1] TRUE
#> 
#> $all_dots_num
#> [1] FALSE
#> 
#> $all_dots_mats
#> [1] FALSE
#> 
#> $all_dots_list
#> [1] TRUE
#> 
#> $all_dots_vect
#> [1] TRUE
#> 
#> $all_dots_char
#> [1] FALSE
#> 
#> $all_dots_longer_than_1
#> [1] TRUE
#> 
#> $dots_names
#> [1] "a" "b"
#> 
#> $keep_args
#> $keep_args$.dat
#> NULL
#> 
#> $keep_args$FUN
#> NULL
#> 
#> $keep_args$dots
#>   a   b 
#> "a" "b" 
#> 
#> 
```
