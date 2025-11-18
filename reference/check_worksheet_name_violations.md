# Develop a warning message for malformed Excel worksheet names

`write_mat_to_excel()` can include worksheet names, but it is important
that they are legal names. This function emits a warning when
`candidate_worksheet_names` is malformed.

## Usage

``` r
check_worksheet_name_violations(candidate_worksheet_names)
```

## Arguments

- candidate_worksheet_names:

  Worksheet names to be checked.

## Value

`NULL` invisibly and a warning if any problems are detected.

## Examples

``` r
# No warning
check_worksheet_name_violations(c("abc", "123"))
if (FALSE) { # \dontrun{
  # Warnings
  # Illegal characters
  check_worksheet_name_violations(c("abc", "["))
  # Empty name
  check_worksheet_name_violations(c("", "abc"))
  # Too long
  check_worksheet_name_violations(strrep("x", 32))
  # Duplicates
  check_worksheet_name_violations(c("abc123", "abc123"))
} # }
```
