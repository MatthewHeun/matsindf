#' Collapse a "tidy" data frame to matrices in a data frame `matsindf`)
#'
#' A "tidy" data frame contains information that can be collapsed into matrices,
#' including columns for
#' matrix names, row names, column names, row types, column types, and values (entries in matrices).
#' These column names are specified as strings by the `matnames`, `rownames`, `colnames`,
#' `rowtypes`, `coltypes`, and `values` arguments to `collapse_to_matrices()`, respectively.
#' A `matsindf`-style matrix has named rows and columns.
#' In addition, `matsindf`-style matrices have "types" for row and column information,
#' such as "Commodities", "Industries", "Products", or "Machines".
#' The row and column types for the `matsindf`-style matrices are stored as attributes on the matrix
#' (`rowtype` and `coltype`),
#' which can be accessed with the functions `matsbyname::rowtype()` and `matsbyname::coltype()`.
#' Row and column types are both respected and propagated by the various `*_byname` functions
#' of the `matsbyname` package.
#' Use the `*_byname` functions when you do operations on the `matsindf`-style matrices.
#' The `matsindf`-style matrices will be stored
#' in a column with same name as the incoming `values` column.

#' This function is similar to `tidyr::nest()`, which stores data frames into a cell of a data frame.
#' With `collapse_to_matrices`, matrices are created.
#' This function respects groups, like `dplyr::summarise()`.
#' (In fact, calls to this function may not work properly unless grouping is provided.
#' Errors of the form "Error: Duplicate identifiers for rows ..." are usually fixed by
#' grouping `.DF` prior to calling this function.)
#' The usual approach is to `dplyr::group_by()` the `matnames` column
#' and any other columns to be preserved in the output.
#' Note that execution is halted if any of
#' `rownames`, `colnames`, `rowtypes`, `coltypes`, or `values` is a grouping variable in `.DF`.
#' `rowtypes` and `coltypes` should be the same for all rows of the same matrix in `.DF`;
#' execution is halted if that is not the case.
#' `tidyr::pivot_wider()`ing the output by `matnames` may be necessary before
#' calculations are done on the collapsed matrices.
#' See the example.
#'
#' Groups are not preserved on output.
#'
#' Note that two types of matrices can be created, a `matrix` or a `Matrix`.
#' `Matrix` has the advantage of representing sparse matrices with less memory
#' (and disk space).
#' `Matrix` objects are created by `matsbyname::Matrix()`.
#'
#' @param .DF the "tidy" data frame
#' @param matnames A string identifying the column in `.DF` containing matrix names for matrices to be created.
#'                 Default is "matnames".
#' @param matvals  A string identifying the column in `.DF` containing values to be inserted into the matrices to be created.
#'                 This will also be the name of the column in the output containing matrices formed from the
#'                 data in the `matvals` column.
#'                 Default is "matvals".
#' @param rownames A string identifying the column in `.DF` containing row names for matrices to be created.
#'                 Default is "rownames".
#' @param colnames A string identifying the column in `.DF` containing column names for matrices to be created.
#'                 Default is "colnames".
#' @param rowtypes An optional string identifying the column in `.DF` containing the type of values in rows of the matrices to be created.
#'                 Default is `if ("rowtypes" %in% names(.DF)) "rowtypes" else NULL`,
#'                 so that failure to set the rowtypes argument will give `NULL`, as appropriate.
#' @param coltypes An optional string identifying the column in `.DF` containing the type of values in columns of the matrices to be created
#'                 Default is `if ("coltypes" %in% names(.DF)) "rowtypes" else NULL`,
#'                 so that failure to set the coltypes argument will give `NULL`, as appropriate.
#' @param matrix.class    The type of matrix to be created, one of "matrix" or "Matrix".
#'                        Default is "matrix".
#'
#' @return A data frame with matrices in the `matvals` column.
#'
#' @seealso `tidyr::nest()` and `dplyr::summarise()`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(tibble)
#' ptype <- "Products"
#' itype <- "Industries"
#' tidy <- data.frame(Country = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",
#'                                 "US",  "US",  "US",  "US", "GH", "US"),
#'                   Year    = c(  1971,  1971,  1971,  1971,  1971,  1971,  1971,
#'                                 1980,  1980,  1980,  1980, 1971, 1980),
#'                   matrix  = c(   "U",   "U",   "E",   "E",   "E",   "V",   "V",
#'                                  "U",   "U",   "E",   "E", "eta", "eta"),
#'                   row     = c( "c 1", "c 2", "c 1", "c 2", "c 2", "i 1", "i 2",
#'                                "c 1", "c 1", "c 1", "c 2", NA, NA),
#'                   col     = c( "i 1", "i 2", "i 1", "i 2", "i 3", "c 1", "c 2",
#'                                "i 1", "i 2", "i 1", "i 2", NA, NA),
#'                   rowtypes = c( ptype, ptype, ptype, ptype, ptype, itype, itype,
#'                                 ptype, ptype, ptype, ptype, NA, NA),
#'                   coltypes = c( itype, itype, itype, itype, itype, ptype, ptype,
#'                                 itype, itype, itype, itype, NA, NA),
#'                   vals  = c(    11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,
#'                                 11 ,   12 ,   11 ,   22,   0.2, 0.3)
#' ) %>% group_by(Country, Year, matrix)
#' mats <- collapse_to_matrices(tidy, matnames = "matrix", matvals = "vals",
#'                              rownames = "row", colnames = "col",
#'                              rowtypes = "rowtypes", coltypes = "coltypes")
#' mats %>% pivot_wider(names_from = matrix, values_from = vals)
collapse_to_matrices <- function(.DF, matnames = "matnames", matvals = "matvals", rownames = "rownames", colnames = "colnames",
                                 rowtypes = if ("rowtypes" %in% names(.DF)) "rowtypes" else NULL,
                                 coltypes = if ("coltypes" %in% names(.DF)) "coltypes" else NULL,
                                 matrix.class = c("matrix", "Matrix")) {
  matrix.class <- match.arg(matrix.class)
  # Ensure that none of rownames, colnames, or values is a group variable.
  # These can't be in the group variables.
  # If they were, we wouldn't be able to summarise them into the matrices.
  if (any(c(matvals, rownames, colnames, rowtypes, coltypes) %in% dplyr::groups(.DF))) {
    cant_group <- c(rownames, colnames, rowtypes, coltypes, matvals)
    violator <- which(cant_group %in% dplyr::groups(.DF))
    stop(paste(cant_group[[violator]], "is/are grouping variable/s.",
               "Cannot group on rownames, colnames,",
               "rowtypes, coltypes, or matvals in argument .DF of collapse_to_matrices."))
  }
  # Ensure that not only one of rowtypes or coltypes is non-NULL.
  if (xor(is.null(rowtypes), is.null(coltypes))) {
    # xor is TRUE when is.null(rowtypes) is different from is.null(coltypes).
    # When is.null(rowtypes) is different from is.null(coltypes),
    # this is almost surely an error.
    # Why would anyone set rowtype but not coltype, or vice versa?
    # Identify this error.
    stop(paste("One of rowtypes or coltypes was non-NULL while the other was NULL.",
               "Both need to be NULL or both need to be non-NULL in collapse_to_matrices."))
  }
  # If we get here, both rowtypes and coltypes are not NULL.
  # Thus, we need to test only for the one of them being non-NULL
  .DF %>%
    {if (!is.null(rowtypes)) {
      dplyr::group_by(.DF, !!as.name(rowtypes), !!as.name(coltypes), .add = TRUE)
    } else {
      .DF
    } } %>%
    dplyr::do(
      # Convert .DF to matrices
      "{matvals}" := rowcolval_to_mat(.data, rownames = rownames, colnames = colnames, matvals = matvals,
                                      rowtypes = rowtypes, coltypes = coltypes,
                                      matrix.class = matrix.class)
    ) %>%
    dplyr::select(!!!dplyr::group_vars(.DF), !!matvals) %>%
    data.frame(check.names = FALSE)
}

