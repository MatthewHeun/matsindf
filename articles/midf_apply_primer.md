# A matsindf_apply primer

## Introduction

[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
is a powerful and versatile function that enables analysis with lists
and data frames by applying `FUN` in helpful ways. The function is
called
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
because it can be used to apply `FUN` to a `matsindf` data frame, a data
frame that contains matrices as individual entries in a data frame. (A
`matsindf` data frame can be created by calling
[`collapse_to_matrices()`](https://matthewheun.github.io/matsindf/reference/collapse_to_matrices.md),
as demonstrated below.)

But
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
can apply `FUN` across much more: data frames of single numbers, lists
of matrices, lists of single numbers, and individual numbers. This
vignette demonstrates
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
starting with simple examples and proceeding toward sophisticated
analyses.

## The basics

The basis of all analyses conducted with
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
is a function (`FUN`) to be applied across data supplied in `.dat` or
`...`. `FUN` must return a named list of variables as its result. Here
is an example function that both adds and subtracts its arguments, `a`
and `b`, and returns a list containing its result, `c` and `d`.

``` r
example_fun <- function(a, b){
  return(list(c = matsbyname::sum_byname(a, b), 
              d = matsbyname::difference_byname(a, b)))
}
```

Similar to [`lapply()`](https://rdrr.io/r/base/lapply.html) and its
siblings, additional argument(s) to
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
include the data over which `FUN` is to be applied. These arguments can,
in the first instance, be supplied as named arguments to the `...`
argument of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).
All arguments in `...` must be named. The `...` arguments to
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
are passed to `FUN` according to their names. In this case, the output
of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
is the the named list returned by `FUN`.

``` r
matsindf_apply(FUN = example_fun, a = 2, b = 1)
#> $c
#> [1] 3
#> 
#> $d
#> [1] 1
```

Passing an additional argument (`z = 2`) causes an unused argument
error, because `example_fun` does not have a `z` argument.

``` r
tryCatch(
  matsindf_apply(FUN = example_fun, a = 2, b = 1, z = 2),
  error = function(e){e}
)
#> <simpleError in matsindf_apply_types(.dat, FUN, ...): In matsindf::matsindf_apply(), the following unused arguments appeared in ...: z>
```

Failing to pass a needed argument (`b`) causes an error that indicates
the missing argument.

``` r
tryCatch(
  matsindf_apply(FUN = example_fun, a = 2),
  error = function(e){e}
)
#> Warning in matsindf_apply_types(.dat, FUN, ...): In matsindf::matsindf_apply(),
#> the following named arguments to FUN were not found in any of .dat, ..., or
#> defaults to FUN: b. Set .warn_missing_FUN_args = FALSE to suppress this warning
#> if you know what you are doing.
#> <getvarError in (function (a, b) {    return(list(c = matsbyname::sum_byname(a, b), d = matsbyname::difference_byname(a,         b)))})(a = 2): argument "b" is missing, with no default>
```

Alternatively, arguments to `FUN` can be given in a named list to
`.dat`, the first argument of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).
When a value is assigned to `.dat`, the return value from
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
contains all named variables in `.dat` (in this case both `a` and `b`)
in addition to the results provided by `FUN` (in this case both `c` and
`d`).

``` r
matsindf_apply(list(a = 2, b = 1), FUN = example_fun)
#> $a
#> [1] 2
#> 
#> $b
#> [1] 1
#> 
#> $c
#> [1] 3
#> 
#> $d
#> [1] 1
```

Extra variables are tolerated in `.dat`, because `.dat` is considered to
be a store of data from which variables can be drawn as needed.

``` r
matsindf_apply(list(a = 2, b = 1, z = 42), FUN = example_fun)
#> $a
#> [1] 2
#> 
#> $b
#> [1] 1
#> 
#> $c
#> [1] 3
#> 
#> $d
#> [1] 1
```

In contrast, arguments to `...` are named explicitly by the user, so
including an extra argument in `...` is considered an error, as shown
above.

## Some details

If a named argument is supplied by both `.dat` and `...`, the argument
in `...` takes precedence, overriding the argument in `.dat`.

``` r
matsindf_apply(list(a = 2, b = 1), FUN = example_fun, a = 10)
#> $a
#> [1] 10
#> 
#> $b
#> [1] 1
#> 
#> $c
#> [1] 11
#> 
#> $d
#> [1] 9
```

When supplying **both** `.dat` and `...`, `...` can contain named
strings of length `1` which are interpreted as mappings from named items
in `.dat` to arguments in the signature of `FUN`. In the example below,
`a = "z"` indicates that argument `a` to `FUN` should be supplied by
item `z` in `.dat`.

``` r
matsindf_apply(list(a = 2, b = 1, z = 42),
               FUN = example_fun, a = "z")
#> $a
#> [1] 2
#> 
#> $b
#> [1] 1
#> 
#> $z
#> [1] 42
#> 
#> $c
#> [1] 43
#> 
#> $d
#> [1] 41
```

If a named argument appears in both `.dat` and the output of `FUN`, a
name collision occurs in the output of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
and a warning is issued.

``` r
tryCatch(
  matsindf_apply(list(a = 2, b = 1, c = 42), FUN = example_fun),
  warning = function(w){w}
)
#> <simpleWarning in matsindf_apply(list(a = 2, b = 1, c = 42), FUN = example_fun): Name collision in matsindf::matsindf_apply(). The following arguments appear both in .dat and in the output of `FUN`: c>
```

`FUN` can accept more than just numerics. `example_fun_with_string()`
accepts a character string and a numeric. However, because `...`
argument that is a character string of length `1` has special meaning
(namely mapping variables in `.dat` to arguments of `FUN`), passing a
character string of length `1` can cause an error. To get around the
problem, wrap the single string in a list, as shown below.

``` r
example_fun_with_string <- function(str_a, b) {
  a <- as.numeric(str_a)
  list(added = matsbyname::sum_byname(a, b), subtracted = matsbyname::difference_byname(a, b))
}

# Causes an error
tryCatch(
  matsindf_apply(FUN = example_fun_with_string, str_a = "1", b = 2),
  error = function(e){e}
)
#> Warning in matsindf_apply_types(.dat, FUN, ...): In matsindf::matsindf_apply(),
#> the following named arguments to FUN were not found in any of .dat, ..., or
#> defaults to FUN: str_a. Set .warn_missing_FUN_args = FALSE to suppress this
#> warning if you know what you are doing.
#> <evalError in (function (str_a, b) {    a <- as.numeric(str_a)    list(added = matsbyname::sum_byname(a, b), subtracted = matsbyname::difference_byname(a,         b))})(b = 2): argument "str_a" is missing, with no default>
# To solve the problem, wrap "1" in list().
matsindf_apply(FUN = example_fun_with_string, str_a = list("1"), b = 2)
#> $added
#> [1] 3
#> 
#> $subtracted
#> [1] -1
matsindf_apply(FUN = example_fun_with_string, str_a = list("1"), b = list(2))
#> $added
#> [1] 3
#> 
#> $subtracted
#> [1] -1
matsindf_apply(FUN = example_fun_with_string, 
               str_a = list("1", "3"), 
               b = list(2, 4))
#> $added
#> $added[[1]]
#> [1] 3
#> 
#> $added[[2]]
#> [1] 7
#> 
#> 
#> $subtracted
#> $subtracted[[1]]
#> [1] -1
#> 
#> $subtracted[[2]]
#> [1] -1
matsindf_apply(.dat = list(str_a = list("1"), b = list(2)), FUN = example_fun_with_string)
#> $str_a
#> [1] "1"
#> 
#> $b
#> [1] 2
#> 
#> $added
#> [1] 3
#> 
#> $subtracted
#> [1] -1
matsindf_apply(.dat = list(m = list("1"), n = list(2)), FUN = example_fun_with_string, 
               str_a = "m", b = "n")
#> $m
#> [1] "1"
#> 
#> $n
#> [1] 2
#> 
#> $added
#> [1] 3
#> 
#> $subtracted
#> [1] -1
```

## `matsindf_apply()` and data frames

`.dat` can also contain a data frame (or tibble), both of which are
fancy lists. When `.dat` is a data frame or tibble, the output of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
is a tibble, and `FUN` acts like a specialized
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
adding new columns at the right of `.dat`.

``` r
matsindf_apply(.dat = data.frame(str_a = c("1", "3"), b = c(2, 4)), 
               FUN = example_fun_with_string)
#> # A tibble: 2 × 4
#>   str_a     b added subtracted
#>   <chr> <dbl> <dbl>      <dbl>
#> 1 1         2     3         -1
#> 2 3         4     7         -1
matsindf_apply(.dat = data.frame(str_a = c("1", "3"), b = c(2, 4)), 
               FUN = example_fun_with_string, 
               str_a = "str_a", b = "b")
#> # A tibble: 2 × 4
#>   str_a     b added subtracted
#>   <chr> <dbl> <dbl>      <dbl>
#> 1 1         2     3         -1
#> 2 3         4     7         -1
matsindf_apply(.dat = data.frame(m = c("1", "3"), n = c(2, 4)), 
               FUN = example_fun_with_string, 
               str_a = "m", b = "n")
#> # A tibble: 2 × 4
#>   m         n added subtracted
#>   <chr> <dbl> <dbl>      <dbl>
#> 1 1         2     3         -1
#> 2 3         4     7         -1
```

Additional niceties are available when `.dat` is a data frame or a
tibble.
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
works when the data frame is filled with single numeric values, as is
typical.

``` r
df <- data.frame(a = 2:4, b = 1:3)
matsindf_apply(df, FUN = example_fun)
#> # A tibble: 3 × 4
#>       a     b     c     d
#>   <int> <int> <int> <int>
#> 1     2     1     3     1
#> 2     3     2     5     1
#> 3     4     3     7     1
```

But
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
also works with `matsindf` data frames, data frames in which each cell
of the data frame is filled with a single matrix. To demonstrate use of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
with a `matsindf` data frame, we’ll construct a simple `matsindf` data
frame (`midf`) using functions in this package.

``` r
# Create a tidy data frame containing data for matrices
tidy <- tibble::tibble(Year = rep(c(rep(2017, 4), rep(2018, 4)), 2),
                       matnames = c(rep("U", 8), rep("V", 8)),
                       matvals = c(1:4, 11:14, 21:24, 31:34),
                       rownames = c(rep(c(rep("p1", 2), rep("p2", 2)), 2), 
                                    rep(c(rep("i1", 2), rep("i2", 2)), 2)),
                       colnames = c(rep(c("i1", "i2"), 4), 
                                    rep(c("p1", "p2"), 4))) |>
  dplyr::mutate(
    rowtypes = case_when(
      matnames == "U" ~ "Product",
      matnames == "V" ~ "Industry", 
      TRUE ~ NA_character_
    ),
    coltypes = case_when(
      matnames == "U" ~ "Industry",
      matnames == "V" ~ "Product",
      TRUE ~ NA_character_
    )
  )

tidy
#> # A tibble: 16 × 7
#>     Year matnames matvals rownames colnames rowtypes coltypes
#>    <dbl> <chr>      <int> <chr>    <chr>    <chr>    <chr>   
#>  1  2017 U              1 p1       i1       Product  Industry
#>  2  2017 U              2 p1       i2       Product  Industry
#>  3  2017 U              3 p2       i1       Product  Industry
#>  4  2017 U              4 p2       i2       Product  Industry
#>  5  2018 U             11 p1       i1       Product  Industry
#>  6  2018 U             12 p1       i2       Product  Industry
#>  7  2018 U             13 p2       i1       Product  Industry
#>  8  2018 U             14 p2       i2       Product  Industry
#>  9  2017 V             21 i1       p1       Industry Product 
#> 10  2017 V             22 i1       p2       Industry Product 
#> 11  2017 V             23 i2       p1       Industry Product 
#> 12  2017 V             24 i2       p2       Industry Product 
#> 13  2018 V             31 i1       p1       Industry Product 
#> 14  2018 V             32 i1       p2       Industry Product 
#> 15  2018 V             33 i2       p1       Industry Product 
#> 16  2018 V             34 i2       p2       Industry Product

# Convert to a matsindf data frame
midf <- tidy |>  
  dplyr::group_by(Year, matnames) |> 
  collapse_to_matrices(rowtypes = "rowtypes", coltypes = "coltypes") |> 
  tidyr::pivot_wider(names_from = "matnames", values_from = "matvals")

# Take a look at the midf data frame and some of the matrices it contains.
midf
#> # A tibble: 2 × 3
#>    Year U             V            
#>   <dbl> <list>        <list>       
#> 1  2017 <dbl [2 × 2]> <dbl [2 × 2]>
#> 2  2018 <dbl [2 × 2]> <dbl [2 × 2]>
midf$U[[1]]
#>    i1 i2
#> p1  1  2
#> p2  3  4
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
midf$V[[1]]
#>    p1 p2
#> i1 21 22
#> i2 23 24
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
```

With `midf` in hand, we can demonstrate use of
[`tidyverse`](https://tidyverse.org)-style functional programming to
perform matrix algebra within a data frame. The functions of the
`matsbyname` package (such as
[`difference_byname()`](https://matthewheun.github.io/matsbyname/reference/difference_byname.html)
below) can be used for this purpose.

``` r
result <- midf |> 
  dplyr::mutate(
    W = difference_byname(transpose_byname(V), U)
  )
result
#> # A tibble: 2 × 4
#>    Year U             V             W            
#>   <dbl> <list>        <list>        <list>       
#> 1  2017 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
#> 2  2018 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
result$W[[1]]
#>    i1 i2
#> p1 20 21
#> p2 19 20
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
result$W[[2]]
#>    i1 i2
#> p1 20 21
#> p2 19 20
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

This way of performing matrix calculations works equally well within a
2-row `matsindf` data frame (as shown above) or within a 1000-row
`matsindf` data frame.

## Programming with `matsindf_apply()`

Users can write their own functions using
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).
A flexible `calc_W()` function can be written as follows.

``` r
calc_W <- function(.DF = NULL, U = "U", V = "V", W = "W") {
  # The inner function does all the work.
  W_func <- function(U_mat, V_mat){
    # When we get here, U_mat and V_mat will be single matrices or single numbers, 
    # not a column in a data frame or an item in a list.
    if (length(U_mat) == 0 & length(V_mat == 0)) {
      # Tolerate zero-length arguments by returning a zero-length
      # a list with the correct name and return type.
      return(list(numeric()) |> magrittr::setnames(W))
    }
    # Calculate W_mat from the inputs U_mat and V_mat.
    W_mat <- matsbyname::difference_byname(
      matsbyname::transpose_byname(V_mat), 
      U_mat)
    # Return a named list.
    list(W_mat) |> magrittr::set_names(W)
  }
  # The body of the main function consists of a call to matsindf_apply
  # that specifies the inner function in the FUN argument.
  matsindf_apply(.DF, FUN = W_func, U_mat = U, V_mat = V)
}
```

This style of writing
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
functions is incredibly versatile, leveraging the capabilities of both
the `matsindf` and `matsbyname` packages. (Indeed, the `Recca` package
uses
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
heavily and is built upon the functions in the `matsindf` and
`matsbyname` packages.)

Functions written like `calc_W()` can operate in ways similar to
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
itself. To demonstrate, we’ll use `calc_W()` in all the ways that
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
can be used, going in the reverse order to our demonstration of the
capabilities of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
above.

`calc_W()` can be used as a specialized `mutate` function that operates
on `matsindf` data frames.

``` r
midf |> calc_W()
#> # A tibble: 2 × 4
#>    Year U             V             W            
#>   <dbl> <list>        <list>        <list>       
#> 1  2017 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
#> 2  2018 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
```

The added column could be given a different name from the default
(“`W`”) using the `W` argument.

``` r
midf |> calc_W(W = "W_prime")
#> # A tibble: 2 × 4
#>    Year U             V             W_prime      
#>   <dbl> <list>        <list>        <list>       
#> 1  2017 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
#> 2  2018 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
```

As with
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
column names in `midf` can be mapped to the arguments of `calc_W()` by
the arguments to `calc_W()`.

``` r
midf |> 
  dplyr::rename(X = U, Y = V) |> 
  calc_W(U = "X", V = "Y")
#> # A tibble: 2 × 4
#>    Year X             Y             W            
#>   <dbl> <list>        <list>        <list>       
#> 1  2017 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
#> 2  2018 <dbl [2 × 2]> <dbl [2 × 2]> <dbl [2 × 2]>
```

`calc_W()` can operate on lists of single matrices, too. This approach
works, because the default values for the `U` and `V` arguments to
`calc_W()` are “U” and “V”, respectively. The input list members (in
this case `midf$U[[1]]` and `midf$V[[1]]`) are returned with the output,
because `list(U = midf$U[[1]], V = midf$V[[1]])` is passed to the `.dat`
argument of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

``` r
calc_W(list(U = midf$U[[1]], V = midf$V[[1]]))
#> $U
#>    i1 i2
#> p1  1  2
#> p2  3  4
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> $V
#>    p1 p2
#> i1 21 22
#> i2 23 24
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
#> $W
#>    i1 i2
#> p1 20 21
#> p2 19 20
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

It may be clearer to name the arguments as required by the `calc_W()`
function without wrapping in a list first, as shown below. But in this
approach, the input matrices are not returned with the output, because
arguments `U` and `V` are passed to the `...` argument of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md),
not the `.dat` argument of
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md).

``` r
calc_W(U = midf$U[[1]], V = midf$V[[1]])
#> $W
#>    i1 i2
#> p1 20 21
#> p2 19 20
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

`calc_W()` can operate on data frames containing single numbers.

``` r
data.frame(U = c(1, 2), V = c(3, 4)) |> calc_W()
#> # A tibble: 2 × 3
#>       U     V     W
#>   <dbl> <dbl> <dbl>
#> 1     1     3     2
#> 2     2     4     2
```

Finally, `calc_W()` can be applied to single numbers, and the result is
1x1 matrix.

``` r
calc_W(U = 2, V = 3)
#> $W
#> [1] 1
```

It is good practice to write internal functions that tolerate
zero-length inputs, as `calc_W()` does. Doing so, enables results from
different calculations to be `rbind`ed together.

``` r
calc_W(U = numeric(), V = numeric())
#> $W
#> numeric(0)
calc_W(list(U = numeric(), V = numeric()))
#> $U
#> numeric(0)
#> 
#> $V
#> numeric(0)
#> 
#> $W
#> numeric(0)

res <- calc_W(list(U = c(2, 3, 4, 5), V = c(3, 4, 5, 6)))
res0 <- calc_W(list(U = numeric(), V = numeric()))
dplyr::bind_rows(res, res0)
#> # A tibble: 4 × 3
#>       U     V     W
#>   <dbl> <dbl> <dbl>
#> 1     2     3     1
#> 2     3     4     1
#> 3     4     5     1
#> 4     5     6     1
```

## Conclusion

This vignette demonstrated use of the versatile
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
function. Inputs to
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
can be

- single numbers,
- matrices, or
- data frames with appropriately-named columns.

[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md)
can be used for programming, and functions constructed as demonstrated
above share characteristics with
[`matsindf_apply()`](https://matthewheun.github.io/matsindf/reference/matsindf_apply.md):

- they can be used as specialized
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  operators, and
- they can be applied to single numbers, matrices, or data frames with
  appropriately-named columns.
