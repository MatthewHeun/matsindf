# Get symbols for all columns except ...

This convenience function performs a set difference between the columns
of `.DF` and the variable names (or symbols) given in `...`.

## Usage

``` r
everything_except(.DF, ..., .symbols = TRUE)
```

## Arguments

- .DF:

  A data frame whose variable names are to be differenced.

- ...:

  A string, strings, vector of strings, or list of strings representing
  column names to be subtracted from the names of `.DF`/

- .symbols:

  A boolean that defines the return type: `TRUE` for symbols, `FALSE`
  for strings.

## Value

A vector of symbols (when `.symbols = TRUE`) or strings (when
`symbol = FALSE`) containing all variables names except those given in
`...`.

## Examples

``` r
DF <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
everything_except(DF, "a", "b")
#> [[1]]
#> c
#> 
everything_except(DF, "a", "b", symbols = FALSE)
#> [[1]]
#> c
#> 
everything_except(DF, c("a", "b"))
#> [[1]]
#> c
#> 
everything_except(DF, list("a", "b"))
#> [[1]]
#> c
#> 
```
