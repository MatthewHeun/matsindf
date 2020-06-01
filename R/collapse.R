#' Collapse a "tidy" data frame to matrices in a data frame (\code{matsindf})
#'
#' A "tidy" data frame contains information that can be collapsed into matrices,
#' including columns for
#' matrix names, row names, column names, row types, column types, and values (entries in matrices).
#' These column names are specified as strings by the \code{matnames}, \code{rownames}, \code{colnames},
#' \code{rowtypes}, \code{coltypes}, and \code{values} arguments to \link{collapse_to_matrices}, respectively.
#' A \pkg{matsindf}-style matrix has named rows and columns.
#' In addition, \pkg{matsindf}-style matrices have "types" for row and column information,
#' such as "Commodities", "Industries", "Products", or "Machines".
#' The row and column types for the \pkg{matsindf}-style matrices are stored as attributes on the matrix
#' (\code{rowtype} and \code{coltype}),
#' which can be accessed with the \code{\link[matsbyname]{rowtype}} and \code{\link[matsbyname]{coltype}} functions
#' of the \pkg{matsbyname} package.
#' Row and column types are both respected and propagated by the various \code{_byname} functions
#' of the \pkg{matsbyname} package.
#' Use the \code{*_byname} functions when you do operations on the \pkg{matsindf}-style matrices.
#' The \pkg{matsindf}-style matrices will be stored
#' in a column with same name as the incoming \code{values} column.

#' This function is similar to \code{\link[tidyr]{nest}}, which stores data frames into a cell of a data frame.
#' With \code{collapse_to_matrices}, matrices are created.
#' This function is similar to \code{\link{summarise}} in that groups are respected.
#' (In fact, calls to this function may not work properly unless grouping is provided.
#' Errors of the form "Error: Duplicate identifiers for rows ..." are usually fixed by
#' grouping \code{.DF} prior to calling this function.)
#' The usual approach is to \code{\link{group_by}} the \code{matnames} column
#' and any other columns to be preserved in the output.
#' Note that execution is halted if any of
#' \code{rownames}, \code{rowtypes}, \code{colnames}, \code{coltypes}, or \code{values} is a grouping variable.
#' \code{rowtypes} and \code{coltypes} should be the same for all rows of the same matrix;
#' execution is halted if that is not the case.
#' \code{\link{spread}}ing the output by \code{matnames} may be necessary before calculations are done on the matrices.
#' See the example.
#'
#' Groups are not preserved on output.
#'
#' @param .DF the "tidy" data frame
#' @param matnames a string identifying the column in \code{.DF} containing matrix names for matrices to be created.
#'                 Default is "\code{matnames}".
#' @param matvals  a string identifying the column in \code{.DF} containing values to be inserted into the matrices to be created.
#'                 This will also be the name of the column in the output containing matrices formed from the
#'                 data in the \code{matvals} column.
#'                 Default is "\code{matvals}".
#' @param rownames a string identifying the column in \code{.DF} containing row names for matrices to be created.
#'                 Default is "\code{rownames}".
#' @param colnames a string identifying the column in \code{.DF} containing column names for matrices to be created.
#'                 Default is "\code{colnames}".
#' @param rowtypes optional string identifying the column in \code{.DF} containing the type of values in rows of the matrices to be created.
#'                 Default is "\code{rowtypes}".
#' @param coltypes optional string identifying the column in \code{.DF} containing the type of values in columns of the matrices to be created
#'                 Default is "\code{coltypes}".
#'
#' @return a data frame with matrices in columns
#'
#' @seealso \code{\link[tidyr]{nest}} and \code{\link[dplyr]{summarise}}.
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
#' mats %>% spread(key = matrix, value = vals)
collapse_to_matrices <- function(.DF, matnames = "matnames", matvals = "matvals", rownames = "rownames", colnames = "colnames",
                                 rowtypes = "rowtypes", coltypes = "coltypes"){
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
    # Why would anyone set rowtype but not coltype, or vice versa.
    # Identify this error.
    stop(paste("One of rowtypes or coltypes was non-NULL while the other was NULL.",
               "Both need to be NULL or both need to be non-NULL in collapse_to_matrices."))
  }
  # If we get here, both rowtypes and coltypes are not (NULL).
  # Thus, we need to test only for the one of them being non-NULL.
  .DF %>%
    {if (!is.null(rowtypes)) {
      dplyr::group_by(.DF, !!as.name(rowtypes), !!as.name(coltypes), .add = TRUE)
    } else {
      .DF
    } } %>%
    dplyr::do(
      # Convert .DF to matrices
      !!matvals := rowcolval_to_mat(.data, rownames = rownames, colnames = colnames, matvals = matvals,
                                    rowtypes = rowtypes, coltypes = coltypes)
    ) %>%
    dplyr::select(!!!dplyr::group_vars(.DF), !!matvals) %>%
    # data.frame(check.names = FALSE)
    # As of R 4.0.0, stringsAsFactors = FALSE will be the default.
    data.frame(check.names = FALSE, stringsAsFactors = FALSE)
}
