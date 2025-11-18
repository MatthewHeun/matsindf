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
  openxlsx2::wb_load(mat_temp_path) |>
    openxlsx2::wb_get_sheet_names() |>
    expect_equal(c(A = "A", B = "B", C = "C"))

  # Check the appearance of the matrices
  # openxlsx2::wb_open(openxlsx2::wb_load(mat_temp_path))

  # Test for failure when lacking permission to overwrite the file.
  # The default is overwrite_file = FALSE,
  # so this should fail.
  df |>
    write_mats_to_excel(mat_colname = "mat",
                        worksheet_names = "worksheet_name",
                        path = mat_temp_path) |>
    expect_error(regexp = "^File .+ already exists\\.")

  # Test for failure when lacking permission to overwrite a worksheet.
  # The default is overwrite_worksheets = FALSE,
  # so this should fail.
  df |>
    write_mats_to_excel(mat_colname = "mat",
                        worksheet_names = "worksheet_name",
                        path = mat_temp_path,
                        overwrite_file = TRUE) |>
    expect_error("A worksheet by the name 'A' already exists")

  # Test that we can overwrite with correct permissions
  df |>
    write_mats_to_excel(mat_colname = "mat",
                        worksheet_names = "worksheet_name",
                        path = mat_temp_path,
                        overwrite_file = TRUE,
                        overwrite_worksheets = TRUE)
  openxlsx2::wb_load(mat_temp_path) |>
    openxlsx2::wb_get_sheet_names() |>
    expect_equal(c(A = "A", B = "B", C = "C"))
  # Test without specifying names of worksheets
  df |>
    write_mats_to_excel(mat_colname = "mat",
                        path = mat_temp_path,
                        overwrite_file = TRUE,
                        overwrite_worksheets = TRUE)
  openxlsx2::wb_load(mat_temp_path) |>
    openxlsx2::wb_get_sheet_names() |>
    expect_equal(c(A = "A", B = "B", C = "C", `1` = "1", `2` = "2", `3` = "3"))

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


test_that("saving a workbook works if you have no existing worksheets and no worksheet names supplied", {
  # Create a simple matrix
  mat <- matrix(1:6, nrow = 3, ncol = 2,
                dimnames = list(c("r1", "r2", "r3"),
                                c("c1", "c2")))
  # Create a matsindf data frame
  df <- tibble::tibble(mat = list(mat, mat+1, mat+2))
  # Create a temporary file
  mat_temp_path <- tempfile(pattern = "write_mat_to_excel_test_file", fileext = ".xlsx")
  # Write the file.
  df |>
    write_mats_to_excel(mat_colname = "mat",
                        path = mat_temp_path,
                        overwrite_file = TRUE)

  # Check that tabs are correct
  openxlsx2::wb_load(mat_temp_path) |>
    openxlsx2::wb_get_sheet_names() |>
    expect_equal(c(`1` = "1", `2` = "2", `3` = "3"))

  if (file.exists(mat_temp_path)) {
    res <- file.remove(mat_temp_path)
  }
})




















