# Create a message from a data frame

This function is especially helpful for cases when a data frame of
missing or unset values is at hand. Trim unneeded columns, then call
this function to create a string with rows separated by semicolons and
entries separated by commas.

## Usage

``` r
df_to_msg(df)
```

## Arguments

- df:

  The data frame to be converted to a message

## Value

A string with rows separated by semicolons and entries separated by
commas.

## Examples

``` r
data.frame(a = c(1, 2, 3), b = c("a", "b", "c")) |>
  df_to_msg()
#> [1] "a, b\n====\n1, a\n2, b\n3, c"
```
