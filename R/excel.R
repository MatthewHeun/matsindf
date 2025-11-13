#' Write matrices to an Excel file
#'
#' It is often useful to write matrices from a `matsindf`-style data frame
#' into an Excel file for viewing.
#' This function writes one matrix to each tab of the specified file.
#' Tabs are named by the column specified in `worksheet_names`.
#'
#' @param .psut_data A `matsindf`-style data frame.
#' @param mat_colname The name of a column in `.psut_data` containing matrices.
#' @param path The path to which the Excel file will be saved.
#' @param overwrite_file A boolean that tells whether to overwrite an existing file at `path`.
#' @param worksheet_names The name of a column in `.psut_data` containing names of worksheets.
#' @param overwrite_worksheets A boolean that tells whether to overwrite worksheets in an existing file at `path`.
#' @param .wrote_mats_colname The name of the outgoing column
#'                            that tells whether a worksheet was written successfully.
#'                            Default is "WroteMats".
#' @param mat_bg_color The background color for numerical cells in the matrices.
#'
#' @returns The wbWorkbook object that was saved (the result of `openxlsx2::wb_save()`),
#'          invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Create a simple matrix
#'   mat <- matrix(1:6, nrow = 3, ncol = 2,
#'                 dimnames = list(c("r1", "r2", "r3"),
#'                                 c("c1", "c2")))
#'   # Create a matsindf data frame
#'   df <- tibble::tibble(mat = list(mat, mat, mat),
#'                        worksheet_name = c("A", "B", "C"))
#'   # Create a temporary file
#'   mat_temp_path <- tempfile(pattern = "write_mat_to_excel_test_file",
#'                             fileext = ".xlsx")
#'   # Write the file.
#'   df |>
#'     write_mats_to_excel(mat_colname = "mat",
#'                         worksheet_names = "worksheet_name",
#'                         path = mat_temp_path,
#'                         overwrite_file = TRUE)
#'   # Check the appearance
#'   openxlsx2::wb_open(mat_wb)
#'   if (file.exists(mat_temp_path)) {
#'     res <- file.remove(mat_temp_path)
#'   }
#' }
write_mats_to_excel <- function(.psut_data = NULL,
                                mat_colname,
                                path,
                                overwrite_file = FALSE,
                                worksheet_names = NULL,
                                overwrite_worksheets = FALSE,
                                .wrote_mats_colname = "WroteMats",
                                # Gray
                                mat_bg_color = openxlsx2::wb_color(hex = "D9D9D9")) {

  # Check if path exists. Throw an error if overwrite_file is FALSE.
  if (file.exists(path) & !overwrite_file) {
    stop(paste("File", path,
               "already exists. Call `Recca::write_mats_to_excel()` with `overwrite = TRUE`?"))
  }
  if (file.exists(path)) {
    # The file already exists, and
    # the caller is OK with overwriting it
    mat_wb <- openxlsx2::wb_load(file = path)
  } else {
    # Create the workbook from scratch
    mat_wb <- openxlsx2::wb_workbook()
  }

  create_one_tab <- function(mat, worksheet_name) {
    # Figure out the worksheet name
    if (!is.null(worksheet_name)) {
      sheet_name <- worksheet_name
    } else {
      # Get existing sheet names
      existing_sheets <- openxlsx2::wb_get_sheet_names(mat_wb)

      # Create a new name by incrementing the integer
      if (length(existing_sheets) == 0) {
        sheet_name <- "1"
      } else {
        # Set the sheet name to 1 plus the larger of
        # number of sheets or
        # the largest number in the existing sheet names.
        sheet_name <- max(length(existing_sheets),
                          max(as.integer(existing_sheets)), na.rm = TRUE) + 1
      }
    }
    # Check for malformed sheet names. Emit a warning if problem found.
    check_worksheet_name_violations(sheet_name)

    # Get the worksheet names in the file we are building.
    # The existing_sheet_names can come
    # from a disk file to which we are adding or
    # from a the object in memory we are building (mat_wb).
    # mat_wb is both.
    existing_sheet_names <- openxlsx2::wb_get_sheet_names(mat_wb)

    if (!is.null(existing_sheet_names)) {
      if ((sheet_name %in% existing_sheet_names) & !overwrite_worksheets) {
        # If overwrite_worksheets is FALSE and the sheet exists,
        # give an error.
        stop(paste0("A worksheet by the name '", worksheet_name, "' already exists!"))
      }
      if ((sheet_name %in% existing_sheet_names) & overwrite_worksheets) {
        # If overwrite_worksheets is TRUE and the sheet exists,
        # remove it before writing a new sheet.

        # Note that I'm using chaining ($) throughout.
        # As discussed in the openxlsx2 documentation,
        # chaining modifies the workbook in place in memory,
        # which is crucial for writing the modified workbook
        # at the end of this function.
        # The alternative is piping (|>) which
        # creates a copy of the worksheet at a new memory location.
        # We don't want piping, because we want to access the modified
        # object after this function exits.
        mat_wb$remove_worksheet(sheet = sheet_name)
      }
    }

    # Add the new worksheet to the workbook
    mat_wb$add_worksheet(sheet_name)

    # Write the matrix to the worksheet.
    mat_wb$add_data(sheet = sheet_name,
                    # Account for the fact that this_mat could be a
                    # non-native matrix class (such as Matrix)
                    x = as.matrix(mat),
                    array = TRUE,
                    col_names = TRUE,
                    row_names = TRUE)

    # Calculate the region for the numbers of the matrix
    first_color_row <- 2
    last_color_row <- 1 + nrow(mat)
    first_color_col <- 2
    last_color_col <- 1 + ncol(mat)
    # Specify the wb_dims
    mat_region_nums <- openxlsx2::wb_dims(
      rows = first_color_row:last_color_row,
      cols = first_color_col:last_color_col)

    # Style cells

    # Set auto width for rownames
    mat_wb$set_col_widths(sheet = sheet_name,
                          cols = 1,
                          widths = "auto")
    # Right justify rownames
    mat_wb$add_cell_style(sheet = sheet_name,
                          dims = openxlsx2::wb_dims(cols = 1,
                                                    rows = first_color_row:last_color_row),
                          horizontal = "right")
    # Rotate and center colnames
    mat_wb$add_cell_style(sheet = sheet_name,
                          dims = openxlsx2::wb_dims(rows = 1,
                                                    cols = first_color_col:last_color_col),
                          text_rotation = 90,
                          horizontal = "center",
                          vertical = "bottom")
    # Center all numbers in the cells
    mat_wb$add_cell_style(sheet = sheet_name,
                          dims = mat_region_nums,
                          horizontal = "center")

    # Add fill color
    mat_wb$add_fill(sheet = sheet_name,
                    dims = mat_region_nums,
                    color = mat_bg_color)


    # Return a value
    list(TRUE) %>%
      magrittr::set_names(.wrote_mats_colname)
  }




  out <- matsindf::matsindf_apply(.psut_data,
                                  FUN = create_one_tab,
                                  mat = mat_colname,
                                  worksheet_name = worksheet_names)
  # Make sure the directory exists
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  # Write the workbook
  mat_wb |>
    openxlsx2::wb_save(file = path, overwrite = overwrite_file)
}


#' Develop a warning message for malformed Excel worksheet names
#'
#' `write_mat_to_excel()` can include worksheet names, but
#' it is important that they are legal names.
#' This function emits a warning when `candidate_worksheet_names`
#' is malformed.
#'
#' @param candidate_worksheet_names Worksheet names to be checked.
#'
#' @returns `NULL` invisibly and a warning if any problems are detected.
#'
#' @export
#'
#' @examples
#' # No warning
#' check_worksheet_name_violations(c("abc", "123"))
#' \dontrun{
#'   # Warnings
#'   # Illegal characters
#'   check_worksheet_name_violations(c("abc", "["))
#'   # Empty name
#'   check_worksheet_name_violations(c("", "abc"))
#'   # Too long
#'   check_worksheet_name_violations(strrep("x", 32))
#'   # Duplicates
#'   check_worksheet_name_violations(c("abc123", "abc123"))
#' }
check_worksheet_name_violations <- function(candidate_worksheet_names) {
  seen <- character(0)

  for (name in candidate_worksheet_names) {
    problems <- character(0)

    # 1. Check for illegal characters: \ / * ? [ ]
    matches <- gregexpr("(\\\\|/|\\*|\\?|\\[|\\])", name)[[1]]
    if (any(matches > 0)) {
      illegal_chars <- substring(name, matches, matches)
      problems <- c(problems,
                    paste0("contains illegal character(s): ",
                           paste(unique(illegal_chars), collapse = " "))
      )
    }

    # 2. Check for empty names
    if (nchar(name) == 0) {
      problems <- c(problems, "is blank (worksheet names cannot be empty)")
    }

    # 3. Check for length
    if (nchar(name) > 31) {
      problems <- c(problems, "exceeds Excel's 31-character limit")
    }

    # 4. Check for duplicates
    if (name %in% seen) {
      problems <- c(problems, "is a duplicate (worksheet names must be unique)")
    } else {
      seen <- c(seen, name)
    }

    # Report problems
    if (length(problems) > 0) {
      warning(
        sprintf("Invalid Excel worksheet name: '%s'\n  Problem(s): %s",
                name, paste(problems, collapse = "; ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}
