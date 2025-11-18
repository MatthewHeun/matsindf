# Verify that column names in a data frame are not already present

In the `Recca` package, many functions add columns to an existing data
frame. If the incoming data frame already contains columns with the
names of new columns to be added, a name collision could occur, deleting
the existing column of data. This function provides a way to quickly
check whether `newcols` are already present in `.DF`.

## Usage

``` r
verify_cols_missing(.DF, newcols)
```

## Arguments

- .DF:

  the data frame to which `newcols` are to be added

- newcols:

  a single string, a single name, a vector of strings representing the
  names of new columns to be added to `.DF`, or a vector of names of new
  columns to be added to `.DF`

## Value

`NULL`. This function should be called for its side effect of checking
the validity of the names of `newcols` to be added to `.DF`.

## Details

This function terminates execution if a column of `.DF` will be
overwritten by one of the `newcols`.

## Examples

``` r
df <- data.frame(a = c(1,2), b = c(3,4))
verify_cols_missing(df, "d") # Silent. There will be no problem adding column "d".
newcols <- c("c", "d", "a", "b")
if (FALSE) verify_cols_missing(df, newcols) # \dontrun{} # Error: a and b are already in df.
```
