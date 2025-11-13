testthat::test_that("write_mat_to_excel() works as expected", {
  # Create a simple matrix
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"),
                                c("c1", "c2")))
  # Create a matsindf data frame
  df <- tibble::tibble(mat = list(mat, mat+1, mat+2),
                       worksheet_name = c("A", "B", "C"))
  # Create a temporary file
  mat_temp_path <- tempfile(pattern = "write_mat_to_excel_test_file", fileext = ".xlsx")
  # Write the file.
  df |>
    write_mats_to_excel(mat_colname = "mat",
                        worksheet_names = "worksheet_name",
                        path = mat_temp_path,
                        overwrite_file = TRUE)

  # Check that tabs are correct
  mat_wb <- openxlsx2::wb_load(mat_temp_path)
  worksheet_names <- openxlsx2::wb_get_sheet_names(mat_wb)
  expect_equal(worksheet_names, c(A = "A", B = "B", C = "C"))

  # Check the appearance
  # openxlsx2::wb_open(mat_wb)

  if (file.exists(mat_temp_path)) {
    res <- file.remove(mat_temp_path)
  }
})


test_that("check_worksheet_name_violations() works as expected", {
  # Is OK
  check_worksheet_name_violations(c("test1", "test2")) |>
    expect_null()
  # Illegal characters
  check_worksheet_name_violations(c("abc", "[")) |>
    expect_warning()
  # Empty name
  check_worksheet_name_violations(c("", "abc")) |>
    expect_warning()
  # Too long
  check_worksheet_name_violations(strrep("x", 32)) |>
    expect_warning()
  # Duplicates
  check_worksheet_name_violations(c("abc123", "abc123")) |>
    expect_warning()
})

























