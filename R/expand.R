#' Expand a `matsindf` data frame
#'
#' Any tidy data frame of matrices (in which each row represents one matrix observation)
#' can also be represented as a tidy data frame
#' with each non-zero matrix entry as an observation on its own row.
#' This function and `collapse_to_matrices()` convert between the two representations.
#'
#' Names for output columns are specified in the `rownames`, `colnames`,
#' `rowtypes`, and `coltypes`, arguments.
#' The entries of the \pkg{matsindf}-style matrices are stored in an output column named `values.`
#'
#' @param .DF The data frame containing \pkg{matsindf}-style matrices.
#'            (`.DF` may also be a named list of matrices, in which case
#'            names of the matrices are taken from the names of items in the list and
#'            list items are expected to be matrices.)
#' @param matnames The name of the column in `.DF` containing matrix names (a string).
#'                 Default is "matnames".
#' @param matvals The name of the column in `.DF` containing IO-style matrices
#'                or constants (a string).
#'                This will also be the name of the column containing matrix entries
#'                in the output data frame.
#'                Default is "matvals".
#' @param rownames The name for the output column of row names (a string).
#'                 Default is "rownames".
#' @param colnames The name for the output column of column names (a string).
#'                 Default is "colnames".
#' @param rowtypes An optional name for the output column of row types (a string).
#'                 Default is "rowtypes".
#' @param coltypes The optional name for the output column of column types (a string).
#'                 Default is "coltypes".
#' @param drop If specified, the value to be dropped from output,
#'             For example, `drop = 0` will cause `0` entries in the matrices
#'             to be deleted from output.
#'             If `NA`, no values are dropped from output.
#'             Default is `NA`.
#'
#' @return A tidy data frame containing expanded \pkg{matsindf}-style matrices.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(matsbyname)
#' ptype <- "Products"
#' itype <- "Industries"
#' tidy <- data.frame(Country  = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",
#'                                  "US",  "US",  "US",  "US", "GH", "US"),
#'                   Year      = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,
#'                                  1980,  1980,  1980,  1980, 1971, 1980),
#'                   matrix    = c(  "U",   "U",   "Y",   "Y",   "Y",   "V",   "V",
#'                                   "U",   "U",   "Y",   "Y", "eta", "eta"),
#'                   row       = c(  "c1",  "c2",  "c1",  "c2",  "c2",  "i1",  "i2",
#'                                   "c1",  "c1",  "c1",  "c2", NA, NA),
#'                   col       = c(  "i1",  "i2",  "i1",  "i2",  "i3",  "c1",  "c2",
#'                                   "i1",  "i2",  "i1",  "i2", NA, NA),
#'                   rowtypes  = c( ptype, ptype, ptype, ptype, ptype, itype, itype,
#'                                  ptype, ptype, ptype, ptype, NA, NA),
#'                   coltypes  = c(itype, itype, itype, itype, itype, ptype, ptype,
#'                                 itype, itype, itype, itype, NA, NA),
#'                   vals      = c(11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,
#'                                 11 ,   12 ,   11 ,   22,   0.2, 0.3)) %>%
#'   group_by(Country, Year, matrix)
#' mats <- collapse_to_matrices(tidy, matnames = "matrix", rownames = "row", colnames = "col",
#'                              rowtypes = "rowtypes", coltypes = "coltypes",
#'                              matvals = "vals") %>%
#'   ungroup()
#' expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
#'                      rownames = "rows", colnames = "cols",
#'                      rowtypes = "rt",   coltypes = "ct")
#' expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
#'                      rownames = "rows", colnames = "cols",
#'                      rowtypes = "rt",   coltypes = "ct", drop = 0)
expand_to_tidy <- function(.DF,
                           matnames = "matnames",
                           matvals = "matvals",
                           rownames = "rownames",
                           colnames = "colnames",
                           rowtypes = "rowtypes",
                           coltypes = "coltypes",
                           drop = NA){
  if (!is.data.frame(.DF) & is.list(.DF)) {
    # Create an empty 1-row data frame with row names taken from .DF and promote to a column
    tempDF <- matrix(NA, nrow = length(.DF), ncol = 1, dimnames = list(names(.DF), matvals)) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      tibble::rownames_to_column(matnames)
    # Set the matvals column to be the list of items in .DF
    tempDF[[matvals]] <- I(.DF)
    .DF <- tempDF
  }

  temp <- .DF |>
    # Remove any rows with NULL entries in the matvals column.
    dplyr::filter(! sapply(.data[[matvals]], is.null))
  if (nrow(temp) == 0) {
    return(NULL)
  }
  temp |>
    # group by everything except matvals column so that "do" will act as desired
    dplyr::group_by_at(setdiff(colnames(.DF), matvals)) |>
    dplyr::do(
      # Convert .data to row, col, val format
      mat_to_rowcolval(.data[[matvals]][[1L]],
                       rownames = rownames,
                       colnames = colnames,
                       rowtypes = rowtypes,
                       coltypes = coltypes,
                       matvals = matvals,
                       drop = drop)
    ) |>
    # Remove the grouping
    dplyr::ungroup()
}
