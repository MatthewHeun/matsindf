# Apply a function to a `matsindf` data frame (and more)

Applies `FUN` to `.dat` or performs the calculation specified by `FUN`
on numbers or matrices. `FUN` must return a named list. The values of
the list returned `FUN` become entries in columns in a returned data
frame or entries in the sub-lists of a returned list. The names of the
items in the list returned by `FUN` become names of the columns in a
returned data frame or names of the list items in the returned list.

## Usage

``` r
matsindf_apply(.dat = NULL, FUN, ..., .warn_missing_FUN_args = TRUE)
```

## Arguments

- .dat:

  A list of named items or a data frame.

- FUN:

  The function to be applied to `.dat`.

- ...:

  Named arguments to be passed by name to `FUN`.

- .warn_missing_FUN_args:

  A boolean that tells whether to warn of missing arguments to `FUN`.
  Default is `TRUE`.

## Value

A named list or a data frame. (See details.)

## Details

If `is.null(.dat)` and `...` are all named numbers or matrices of the
form `argname = m`, `m`s are passed to `FUN` by `argname`s. The return
value is a named list provided by `FUN`. The arguments in `...` are not
included in the output.

If `is.null(.dat)` and `...` are all lists of numbers or matrices of the
form `argname = l`, `FUN` is `Map`ped across the various `l`s to obtain
a list of named lists returned from `FUN`. The return value is a list
whose top-level names are the names of the returned items from `FUN`
`.dat` is not included in the return value.

If `!is.null(.dat)` and `...` are all named, `length == 1` character
strings of the form `argname = string`, `argname`s are expected to be
names of arguments to `FUN`, and `string`s are expected to be column
names in `.dat`. The return value is `.dat` with additional columns (at
right) whose names are the names of list items returned from `FUN`. When
`.dat` contains columns whose names are same as columns added at the
right, a warning is emitted.

`.dat` can be a list of named items in which case a list will be
returned instead of a data frame.

If items in `.dat` have same names as arguments to `FUN`, it is not
necessary to specify any arguments in `...`. `matsindf_apply` assumes
that the appropriately-named items in `.dat` are intended to be
arguments to `FUN`. When an item name appears in both `...` and `.dat`,
`...` takes precedence.

if `.dat` is a data frame, the items in its columns (possibly matrices)
are [`unname()`](https://rdrr.io/r/base/unname.html)d before calling
`FUN`.

`NULL` arguments in `...` are ignored for the purposes of deciding
whether all arguments are numbers, matrices, lists of numbers of
matrices, or named character strings. However, all `NULL` arguments are
passed to `FUN`, so `FUN` should be able to deal with `NULL` arguments
appropriately.

If `.dat` is present, `...` contains `length == 1` strings, and one of
the `...` strings is not the name of a column in `.dat`, `FUN` is called
WITHOUT the argument whose column is missing. I.e., that argument is
treated as missing. If `FUN` works despite the missing argument,
execution proceeds. If `FUN` cannot handle the missing argument, an
error will occur in `FUN`.

It is suggested that `FUN` is able to handle empty data gracefully,
returning an empty result with the same names as when non-empty data are
fed to `FUN`. Attempts are made to handle zero-row data (in `.dat` or
`...`) gracefully. First, `FUN` is called with the empty (but named)
data. If `FUN` can handle empty data without error, the result is
returned. If `FUN` errors when fed empty data, `FUN` is called with an
empty argument list in the hopes that `FUN` has reasonable default
values. If that fails, `.dat` is returned unmodified (if not `NULL`) or
the data in `...` is returned.

If `.dat` is `NULL` and all named arguments in `...` are similarly
`NULL`, the result will be a list with each named argument being an
empty list. See examples.

## Examples

``` r
library(matsbyname)
example_fun <- function(a, b){
  return(list(c = sum_byname(a, b),
              d = difference_byname(a, b)))
}
# Single values for arguments
matsindf_apply(FUN = example_fun, a = 2, b = 2)
#> $c
#> [1] 4
#> 
#> $d
#> [1] 0
#> 
# Matrices for arguments
a <- 2 * matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))
b <- 0.5 * a
matsindf_apply(FUN = example_fun, a = a, b = b)
#> $c
#>    c1 c2
#> r1  3  6
#> r2  9 12
#> 
#> $d
#>    c1 c2
#> r1  1  2
#> r2  3  4
#> 
# Single values in lists are treated like columns of a data frame
matsindf_apply(FUN = example_fun, a = list(2, 2), b = list(1, 2))
#> $c
#> $c[[1]]
#> [1] 3
#> 
#> $c[[2]]
#> [1] 4
#> 
#> 
#> $d
#> $d[[1]]
#> [1] 1
#> 
#> $d[[2]]
#> [1] 0
#> 
#> 
# Matrices in lists are treated like columns of a data frame
matsindf_apply(FUN = example_fun, a = list(a, a), b = list(b, b))
#> $c
#> $c[[1]]
#>    c1 c2
#> r1  3  6
#> r2  9 12
#> 
#> $c[[2]]
#>    c1 c2
#> r1  3  6
#> r2  9 12
#> 
#> 
#> $d
#> $d[[1]]
#>    c1 c2
#> r1  1  2
#> r2  3  4
#> 
#> $d[[2]]
#>    c1 c2
#> r1  1  2
#> r2  3  4
#> 
#> 
# Single numbers in a data frame
DF <- data.frame(a = c(4, 4, 5), b = c(4, 4, 4))
matsindf_apply(DF, FUN = example_fun, a = "a", b = "b")
#> # A tibble: 3 × 4
#>       a     b     c     d
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     4     4     8     0
#> 2     4     4     8     0
#> 3     5     4     9     1
# By default, arguments to FUN come from DF
matsindf_apply(DF, FUN = example_fun)
#> # A tibble: 3 × 4
#>       a     b     c     d
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     4     4     8     0
#> 2     4     4     8     0
#> 3     5     4     9     1
# Now put some matrices in a data frame.
DF2 <- data.frame(a = I(list(a, a)), b = I(list(b,b)))
matsindf_apply(DF2, FUN = example_fun, a = "a", b = "b")
#> # A tibble: 2 × 4
#>   a             b             c             d            
#>   <I<list>>     <I<list>>     <list>        <list>       
#> 1 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
#> 2 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
# All arguments to FUN are supplied by named items in .dat
matsindf_apply(list(a = 1, b = 2), FUN = example_fun)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
#> $d
#> [1] -1
#> 
# All arguments are supplied by named arguments in ..., but mix them up.
# Note that the named arguments override the items in .dat
matsindf_apply(list(a = 1, b = 2, z = 10), FUN = example_fun, a = "z", b = "b")
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $z
#> [1] 10
#> 
#> $c
#> [1] 12
#> 
#> $d
#> [1] 8
#> 
# A warning is issued when an output item has same name as an input item.
matsindf_apply(list(a = 1, b = 2, c = 10), FUN = example_fun, a = "c", b = "b")
#> Warning: Name collision in matsindf::matsindf_apply(). The following arguments appear both in .dat and in the output of `FUN`: c
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 10
#> 
#> $c
#> [1] 12
#> 
#> $d
#> [1] 8
#> 
# When a zero-row data frame supplied to .dat,
# .dat is returned unmodified, unless FUN can handle empty data.
DF3 <- DF2[0, ]
DF3
#> [1] a b
#> <0 rows> (or 0-length row.names)
matsindf_apply(DF3, FUN = example_fun, a = "a", b = "b")
#> [1] a b
#> <0 rows> (or 0-length row.names)
# A list of named but empty lists is returned if
# NULL is passed to all named arguments.
matsindf_apply(FUN = example_fun, a = NULL, b = NULL)
#> $a
#> list()
#> 
#> $b
#> list()
#> 
```
