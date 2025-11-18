# Decide where to get each argument to FUN

The precedence rules for where to obtain values for the `FUN` argument
to
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
are codified here. The rules are:

- Precedence order: `...`, `.dat`, defaults arguments to `FUN` (highest
  priority to lowest priority).

- If an element of `...` is a character string of length `1`, the
  element of `...` provides a mapping between an item in `.dat` (with
  same name as the value of the character string of length `1`) to an
  argument of `FUN` (with the same name as the name of the character
  string of length `1`).

- If the value of the character string of length `1` is not a name in
  `.dat`, the default arguments to `FUN` are checked in this order.

  - If the name of a default argument to `FUN` is the same as the value
    of the string of length `1` argument in `...`, a mapping occurs.

  - If a mapping is not possible, the default arg to `FUN` is used
    directly.

## Usage

``` r
where_to_get_args(.dat = NULL, FUN, ...)
```

## Arguments

- .dat:

  The `.dat` argument to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

- FUN:

  The `FUN` argument to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

- ...:

  The `...` argument to
  [`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

## Value

A named list wherein the names are the argument names to `FUN`. Values
are character vectors with 2 elements. The first element is named
`source` and provides the argument to
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
from which the named argument should be found, one of ".dat", "FUN", or
"...". The second element is named `arg_name` and provides the variable
name or argument name in the source that contains the input data for the
argument to `FUN`.

## Examples

``` r
example_fun <- function(a = 1, b) {
  list(c = a + b, d = a - b)
}
# b is not available anywhere, likely causing an error later
matsindf:::where_to_get_args(FUN = example_fun)
#> $a
#>   source arg_name 
#>    "FUN"      "a" 
#> 
#> $b
#> NULL
#> 
# b is now available in ...
matsindf:::where_to_get_args(FUN = example_fun, b = 2)
#> $a
#>   source arg_name 
#>    "FUN"      "a" 
#> 
#> $b
#>   source arg_name 
#>    "..."      "b" 
#> 
# b is now available in .dat
matsindf:::where_to_get_args(list(b = 2), FUN = example_fun)
#> $a
#>   source arg_name 
#>    "FUN"      "a" 
#> 
#> $b
#>   source arg_name 
#>   ".dat"      "b" 
#> 
# b now comes from ..., because ... takes precedence over .dat
matsindf:::where_to_get_args(list(b = 2), FUN = example_fun, b = 3)
#> $a
#>   source arg_name 
#>    "FUN"      "a" 
#> 
#> $b
#>   source arg_name 
#>    "..."      "b" 
#> 
# Mapping from c in .dat to b in FUN
matsindf:::where_to_get_args(list(c = 2),
                             FUN = example_fun, b = "c")
#> $a
#>   source arg_name 
#>    "FUN"      "a" 
#> 
#> $b
#>   source arg_name 
#>   ".dat"      "c" 
#> 
# Redirect from an arg in ... to a different default to FUN
matsindf:::where_to_get_args(FUN = example_fun, b = "a")
#> $a
#>   source arg_name 
#>    "FUN"      "a" 
#> 
#> $b
#>   source arg_name 
#>    "FUN"      "a" 
#> 
# b is found in FUN, not in .dat, because the mapping (b = "a")
# is not available in .dat
matsindf:::where_to_get_args(list(b = 2), FUN = example_fun, b = "a")
#> $a
#>   source arg_name 
#>    "FUN"      "a" 
#> 
#> $b
#>   source arg_name 
#>    "FUN"      "a" 
#> 
```
