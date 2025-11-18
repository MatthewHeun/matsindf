# Index a column in a data frame by groups relative to an initial year

This function indexes (by ratio) variables in `vars_to_index` to the
first time in `time_var` or to `index_time` (if specified). Groups in
`.DF` are both respected and required. Neither `var_to_index` nor
`time_var` can be in the grouping variables.

## Usage

``` r
index_column(
  .DF,
  var_to_index,
  time_var = "Year",
  index_time = NULL,
  indexed_var = paste0(var_to_index, suffix),
  suffix = "_indexed"
)
```

## Arguments

- .DF:

  the data frame in which the variables are contained

- var_to_index:

  the column name representing the variable to be indexed (a string)

- time_var:

  the name of the column containing time information. Default is
  "`Year`".

- index_time:

  the time to which data in `var_to_index` are indexed. If `NULL` (the
  default), `index_time` is set to the first time of each group.

- indexed_var:

  the name of the indexed variable. Default is
  "`<<var_to_index>>_<<suffix>>`".

- suffix:

  the suffix to be appended to the indexed variable. Default is
  "`_indexed`".

## Value

a data frame with same number of rows as `.DF` and the following
columns: grouping variables of `.DF`, `var_to_index`, `time_var`, and
one additional column containing indexed `var_to_index` named with the
value of `indexed_var`.

## Details

Note that this function works when the variable to index is a column of
numbers or a column of matrices.

## Examples

``` r
library(dplyr)
library(tidyr)
DF <- data.frame(Year = c(2000, 2005, 2010), a = c(10, 15, 20), b = c(5, 5.5, 6)) %>%
  gather(key = name, value = var, a, b) %>%
  group_by(name)
index_column(DF, var_to_index = "var", time_var = "Year", suffix = "_ratioed")
#> # A tibble: 6 × 4
#> # Groups:   name [2]
#>    Year name    var var_ratioed
#>   <dbl> <chr> <dbl>       <dbl>
#> 1  2000 a      10           1  
#> 2  2005 a      15           1.5
#> 3  2010 a      20           2  
#> 4  2000 b       5           1  
#> 5  2005 b       5.5         1.1
#> 6  2010 b       6           1.2
index_column(DF, var_to_index = "var", time_var = "Year", indexed_var = "now.indexed")
#> # A tibble: 6 × 4
#> # Groups:   name [2]
#>    Year name    var now.indexed
#>   <dbl> <chr> <dbl>       <dbl>
#> 1  2000 a      10           1  
#> 2  2005 a      15           1.5
#> 3  2010 a      20           2  
#> 4  2000 b       5           1  
#> 5  2005 b       5.5         1.1
#> 6  2010 b       6           1.2
index_column(DF, var_to_index = "var", time_var = "Year", index_time = 2005,
             indexed_var = "now.indexed")
#> # A tibble: 6 × 4
#> # Groups:   name [2]
#>    Year name    var now.indexed
#>   <dbl> <chr> <dbl>       <dbl>
#> 1  2000 a      10         0.667
#> 2  2005 a      15         1    
#> 3  2010 a      20         1.33 
#> 4  2000 b       5         0.909
#> 5  2005 b       5.5       1    
#> 6  2010 b       6         1.09 
if (FALSE) { # \dontrun{
  DF %>%
    ungroup() %>%
    group_by(name, var) %>%
    index_column(var_to_index = "var", time_var = "Year") # Fails! Do not group on var_to_index.
  DF %>%
    ungroup() %>%
    group_by(name, Year) %>%
    index_column(var_to_index = "var", time_var = "Year") # Fails! Do not group on time_var.
} # }
```
