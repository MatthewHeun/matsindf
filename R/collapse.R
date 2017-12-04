#' Collapse a "tidy" data frame to IO-style matrices
#'
#' A "tidy" data frame contains information that can be collapsed into IO-style matrices,
#' including columns for matrix names, row names, column names, row types, column types, and values.
#' These columns are specified in the \code{matnames}, \code{rownames}, \code{colnames},
#' \code{rowtypes}, \code{coltypes}, and \code{values} arguments, respectively.

#' An IO-style matrix has named rows and columns.
#' In addition, IO-style matrices have "types" for row and column information, such as "Commodities", "Industries",
#' "Products", or "Machines".
#' The row and column types for the IO matrices are stored as an attribute on the matrix (\code{"rowcoltypes"}),
#' which can be accessed with the \code{rowcoltypes} function.
#' The row and column types are both respected and propagated by the various \code{_byname} functions.
#' Use the \code{_byname} functions when you do operations on the IO-style matrices.
#' The IO-style matrices will be stored in a column with same name as the incoming \code{values} column.
#'
#' This function is similar to \code{summarise} in that groups are respected.
#' (In fact, calls to this function may not work properly unless grouping is provided.
#' Errors of the form "Error: Duplicate identifiers for rows ..." are usually fixed by
#' grouping \code{.data} prior to calling this function.)
#' The usual approach is to \code{group_by} the \code{matnames} column
#' and any other columns to be preserved in the output.
#' Note that execution is halted if any of
#' \code{rownames}, \code{rowtypes}, \code{colnames}, \code{coltypes}, or \code{values} is a grouping variable.
#' \code{rowtypes} and \code{coltypes} should be the same for all rows of a given matrix;
#' execution is halted if that is not the case.
#'
#' \code{dplyr::spread}ing the output by \code{matnames} may be necessary before calculations are done on the matrics.
#' See the example.
#'
#' @param .data the "tidy" data frame
#' @param matnames a string identifying the column in \code{.data} containing matrix names for matrices to be created
#' @param rownames a string identifying the column in \code{.data} containing row names for matrices to be created
#' @param colnames a string identifying the column in \code{.data} containing column names for matrices to be created
#' @param rowtypes a string identifying the column in \code{.data} containing the type of values in rows of the matrices to be created
#' @param coltypes a string identifying the column in \code{.data} containing the type of values in columns of the matrices to be created
#' @param   values a string identifying the column in \code{.data} containing values to be inserted into the matrices to be created.
#'                 This will also be the name of the column in the output containing matrices formed from the
#'                 data in the \code{values} column.
#'
#' @return a data frame with matrices in columns
#' @export
#'
#' @examples
#' ctype <- "Commodities"
#' itype <- "Industries"
#' tidy <- data.frame(Country = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
#'                   Year    = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
#'                   matrix  = c(   "U",   "U",   "E",   "E",   "E",   "V",   "V",   "U",   "U",   "E",   "E", "eta", "eta"),
#'                   row     = c( "c 1", "c 2", "c 1", "c 2", "c 2", "i 1", "i 2", "c 1", "c 1", "c 1", "c 2", NA, NA),
#'                   col     = c( "i 1", "i 2", "i 1", "i 2", "i 3", "c 1", "c 2", "i 1", "i 2", "i 1", "i 2", NA, NA),
#'                   rowtype = c(ctype, ctype, ctype, ctype, ctype, itype, itype, ctype, ctype, ctype, ctype, NA, NA),
#'                   coltype = c(itype, itype, itype, itype, itype, ctype, ctype, itype, itype, itype, itype, NA, NA),
#'                   vals  = c(   11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,   11 ,   12 ,   11 ,   22,   0.2, 0.3)
#' ) %>% group_by(Country, Year, matrix)
#' IOmats <- collapse_to_IOmatrices(tidy, matnames = "matrix", rownames = "row", colnames = "col",
#'                                                              rowtypes = "rowtype", coltypes = "coltype",
#'                                                              values = "vals")
#' IOmats %>% spread(key = matrix, value = vals)
collapse_to_IOmatrices <- function(.data, matnames, rownames, colnames, rowtypes, coltypes, values){
  # Ensure that none of rownames, rowtypes, colnames, coltypes, or values is a group variable.
  # These can't be in the group variables.  If they were, we wouldn't be able to summarise them into the matrices.
  if (any(c(rownames, rowtypes, colnames, coltypes, values) %in% groups(.data))){
    cant_group <- c(rownames, rowtypes, colnames, coltypes, values)
    violator <- which(cant_group %in% groups(.data))
    stop(paste(cant_group[[violator]], "is an illegal grouping variable in argument .data in spread_to_matrices."))
  }
  .data %>%
    # We want to add grouping on the rowtypes and coltypes columns.
    # Doing so both
    # (a) ensures that rowtype and coltype are all same for each matrix and
    # (b) preserves rowtype and coltype in columns.
    group_by_(.dots = c(rowtypes, coltypes), add = TRUE) %>%
    dplyr::do(
      # Convert .data to matrices
      .temp_matrices_col = rowcolval_to_mat(., rownames = rownames, colnames = colnames, values = values,
                                            rowtype = rowtypes, coltype = coltypes)
    ) %>%
    rename_(.dots = setNames(".temp_matrices_col", values)) %>%
    select_(.dots = c(groups(.data), values)) %>% # Eliminate rowtypes and coltypes from output
    data.frame(check.names = FALSE)
}
