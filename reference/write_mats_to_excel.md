# Write matrices to an Excel file

It is often useful to write matrices from a `matsindf`-style data frame
into an Excel file for viewing. This function writes one matrix to each
tab of the specified file. Tabs are named by the column specified in
`worksheet_names`.

## Usage

``` r
write_mats_to_excel(
  .psut_data = NULL,
  mat_colname,
  path,
  overwrite_file = FALSE,
  worksheet_names = NULL,
  overwrite_worksheets = FALSE,
  .wrote_mats_colname = "WroteMats",
  mat_bg_color = openxlsx2::wb_color(hex = "D9D9D9")
)
```

## Arguments

- .psut_data:

  A `matsindf`-style data frame.

- mat_colname:

  The name of a column in `.psut_data` containing matrices.

- path:

  The path to which the Excel file will be saved.

- overwrite_file:

  A boolean that tells whether to overwrite an existing file at `path`.

- worksheet_names:

  The name of a column in `.psut_data` containing names of worksheets.

- overwrite_worksheets:

  A boolean that tells whether to overwrite worksheets in an existing
  file at `path`.

- .wrote_mats_colname:

  The name of the outgoing column that tells whether a worksheet was
  written successfully. Default is "WroteMats".

- mat_bg_color:

  The background color for numerical cells in the matrices.

## Value

The wbWorkbook object that was saved (the result of
[`openxlsx2::wb_save()`](https://janmarvin.github.io/openxlsx2/reference/wb_save.html)),
invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Create a simple matrix
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"),
                                c("c1", "c2")))
  # Create a matsindf data frame
  df <- tibble::tibble(mat = list(mat, mat+1, mat+2),
                       worksheet_name = c("A", "B", "C"))
  # Create a temporary file
  mat_temp_path <- tempfile(pattern = "write_mat_to_excel_test_file",
                            fileext = ".xlsx")
  # Write the file.
  df |>
    write_mats_to_excel(mat_colname = "mat",
                        worksheet_names = "worksheet_name",
                        path = mat_temp_path,
                        overwrite_file = TRUE)
  # Check the appearance
  openxlsx2::wb_open(mat_wb)
  if (file.exists(mat_temp_path)) {
    res <- file.remove(mat_temp_path)
  }
} # }
```
