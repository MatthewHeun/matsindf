# Group by all variables except some

This is a convenience function that allows grouping of a data frame by
all variables (columns) except those variables specified in `...`.

## Usage

``` r
group_by_everything_except(.DF, ..., .add = FALSE, .drop = FALSE)
```

## Arguments

- .DF:

  A data frame to be grouped.

- ...:

  A string, strings, vector of strings, or list of strings representing
  column names to be excluded from grouping.

- .add:

  When `.add = FALSE`, the default,
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  will override existing groups. To add to the existing groups, use
  `.add = TRUE`.

- .drop:

  When `.drop = TRUE`, empty groups are dropped. Default is `FALSE`.

## Value

A grouped version of `.DF`.

## Examples

``` r
library(dplyr)
DF <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
group_by_everything_except(DF) %>% group_vars()
#> [1] "a" "b" "c"
group_by_everything_except(DF, NULL) %>% group_vars()
#> [1] "a" "b" "c"
group_by_everything_except(DF, c()) %>% group_vars()
#> [1] "a" "b" "c"
group_by_everything_except(DF, list()) %>% group_vars()
#> [1] "a" "b" "c"
group_by_everything_except(DF, c) %>% group_vars()
#> [1] "a" "b" "c"
group_by_everything_except(DF, "a") %>% group_vars()
#> [1] "b" "c"
group_by_everything_except(DF, "c") %>% group_vars()
#> [1] "a" "b"
group_by_everything_except(DF, c("a", "c")) %>% group_vars()
#> [1] "b"
group_by_everything_except(DF, c("a")) %>% group_vars()
#> [1] "b" "c"
group_by_everything_except(DF, list("a")) %>% group_vars()
#> [1] "b" "c"
```
