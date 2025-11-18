# Create a usable list of default arguments to a function

`formals(FUN)` does not handle arguments without a default well,
returning a `name` vector of length `1`, which when converted to
character is "". This function detects that condition and replaces the
no-default argument with the value of `.no_default`, by default `NULL`.

## Usage

``` r
get_useable_default_args(FUN, which = c("values", "names"), no_default = NULL)
```

## Arguments

- FUN:

  A function from which values of default arguments are to be extracted.

- which:

  Tells whether to get "names" of arguments or "values" of arguments.
  Default is "values".

- no_default:

  The placeholder value for arguments with no default.

## Value

A named list of default arguments to `FUN`. Names are the argument
names. Values are the default argument values.

## Examples

``` r
f <- function(a = 42, b) {
  return(a + b)
}
matsindf:::get_useable_default_args(f)
#> $a
#> [1] 42
#> 
matsindf:::get_useable_default_args(f, no_default = logical())
#> $a
#> [1] 42
#> 
```
