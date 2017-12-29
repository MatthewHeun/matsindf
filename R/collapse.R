#' Collapse a "tidy" data frame to matrices in a data frame (\code{matsindf})
#'
#' A "tidy" data frame contains information that can be collapsed into matrices,
#' including columns for
#' matrix names, row names, column names, row types, column types, and values (entries in matrices).
#' These column names are specified as strings by the \code{matnames}, \code{rownames}, \code{colnames},
#' \code{rowtypes}, \code{coltypes}, and \code{values} arguments to \link{collapse_to_matrices}, respectively.
#' An \code{\link{matsindf}}-style matrix has named rows and columns.
#' In addition, \code{\link{matsindf}}-style-style matrices have "types" for row and column information,
#' such as "Commodities", "Industries", "Products", or "Machines".
#' The row and column types for the \code{\link{matsindf}}-style matrices are stored as attributes on the matrix
#' (\code{rowtype} and \code{coltype}),
#' which can be accessed with the \code{\link{rowtype}} and \code{\link{coltype}} functions
#' of the \code{\link{byname}} package.
#' Row and column types are both respected and propagated by the various \code{_byname} functions
#' of the \code{\link{byname}} package.
#' Use the \code{_byname} functions when you do operations on the \code{\link{matsindf}}-style matrices.
#' The \code{\link{matsindf}}-style-style matrices will be stored
#' in a column with same name as the incoming \code{values} column.

#' This function is similar to \code{\link{summarise}} in that groups are respected.
#' (In fact, calls to this function may not work properly unless grouping is provided.
#' Errors of the form "Error: Duplicate identifiers for rows ..." are usually fixed by
#' grouping \code{.data} prior to calling this function.)
#' The usual approach is to \code{\link{group_by}} the \code{matnames} column
#' and any other columns to be preserved in the output.
#' Note that execution is halted if any of
#' \code{rownames}, \code{rowtypes}, \code{colnames}, \code{coltypes}, or \code{values} is a grouping variable.
#' \code{rowtypes} and \code{coltypes} should be the same for all rows of the same matrix;
#' execution is halted if that is not the case.
#' \code{\link{spread}}ing the output by \code{matnames} may be necessary before calculations are done on the matrics.
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
#'
#' @export
#'
#' @importFrom byname setrowtype
#' @importFrom byname setcoltype
#' @importFrom dplyr filter
#' @importFrom dplyr groups
#' @importFrom dplyr group_by
#' @importFrom dplyr group_by_at
#' @importFrom dplyr group_vars
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom tibble column_to_rownames
#' @importFrom tibble remove_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#'
#' @examples
#' library(magrittr)
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
#'                   rowtype = c( ptype, ptype, ptype, ptype, ptype, itype, itype,
#'                                ptype, ptype, ptype, ptype, NA, NA),
#'                   coltype = c( itype, itype, itype, itype, itype, ptype, ptype,
#'                                itype, itype, itype, itype, NA, NA),
#'                   vals  = c(    11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,
#'                                 11 ,   12 ,   11 ,   22,   0.2, 0.3)
#' ) %>% group_by(Country, Year, matrix)
#' mats <- collapse_to_matrices(tidy, matnames = "matrix", values = "vals",
#'                              rownames = "row", colnames = "col",
#'                              rowtypes = "rowtype", coltypes = "coltype")
#' mats %>% spread(key = matrix, value = vals)
collapse_to_matrices <- function(.data, matnames, values, rownames, colnames, rowtypes, coltypes){
  # Ensure that none of rownames, rowtypes, colnames, coltypes, or values is a group variable.
  # These can't be in the group variables.  If they were, we wouldn't be able to summarise them into the matrices.
  if (any(c(values, rownames, rowtypes, colnames, coltypes) %in% groups(.data))) {
    cant_group <- c(rownames, colnames, rowtypes, coltypes, values)
    violator <- which(cant_group %in% groups(.data))
    stop(paste(cant_group[[violator]], " are grouping variables.",
               "Cannot group on rownames, colnames,",
               "rowtypes, coltypes, or values in argument .data of collapse_to_matrices."))
  }
  .data %>%
    # We want to add grouping on the rowtypes and coltypes columns.
    # Doing so both
    # (a) ensures that rowtype and coltype are all same for each matrix and
    # (b) preserves rowtype and coltype in columns.
    group_by(!!rowtypes, !!coltypes, add = TRUE) %>%
    dplyr::do(
      # Convert .data to matrices
      !!values := rowcolval_to_mat(.data, rownames = rownames, colnames = colnames, values = values,
                                   rowtype = rowtypes, coltype = coltypes)
    ) %>%
    select(!!!group_vars(.data), !!values) %>%
    data.frame(check.names = FALSE)
}
