#' Expand a "tidy" data frame with matsindf-style matrices to a "tidy" data frame with each matrix entry as an observation
#'
#' A data frame with \code{\link{matsindf}}-style matrices contains matrices with names \code{matnames}
#' in the column specified by \code{matvals}).
#' An IO-style matrix has named rows and columns.
#' In addition, \code{\link{matsindf}}-style matrices have "types" for row and column information, such as "Commodities", "Industries",
#' "Products", or "Machines".
#'
#' Names for output columns are specified in the \code{rownames}, \code{colnames},
#' \code{rowtypes}, and \code{coltypes}, arguments.
#' The entries of the \code{\link{matsindf}}-style matrices are stored in an output column named \code{values}.
#'
#' @param    .data the data frame containing \code{\link{matsindf}}-style matrices
#' @param matnames name of the column in \code{.data} containing matrix names (a string)
#' @param  matvals name of the column in \code{.data} containing IO-style matrices or constants (a string),
#' This will also be the name of the column containing matrix entries in the output data frame.
#' @param rownames name for the output column of row names (a string)
#' @param colnames name for the output column of column names (a string)
#' @param rowtypes name for the output column of row types (a string)
#' @param coltypes name for the output column of column types (a string)
#' @param     drop if specified, the value to be dropped from output,
#' For example, \code{drop = 0} will cause \code{0} entries in the matrices to be deleted from output.
#' If \code{NA}, no values are dropped from output.
#'
#' @return a tidy data frame containing expanded \code{\link{matsindf}}-style matrices
#' @export
#'
#' @examples
#' ptype <- "Products"
#' itype <- "Industries"
#' tidy <- data.frame(Country = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
#'                   Year    = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
#'                   matrix  = c(   "U",   "U",   "Y",   "Y",   "Y",   "V",   "V",   "U",   "U",   "Y",   "Y", "eta", "eta"),
#'                   row     = c(  "c1",  "c2",  "c1",  "c2",  "c2",  "i1",  "i2",  "c1",  "c1",  "c1",  "c2", NA, NA),
#'                   col     = c(  "i1",  "i2",  "i1",  "i2",  "i3",  "c1",  "c2",  "i1",  "i2",  "i1",  "i2", NA, NA),
#'                   rowtype = c(ptype, ptype, ptype, ptype, ptype, itype, itype, ptype, ptype, ptype, ptype, NA, NA),
#'                   coltype = c(itype, itype, itype, itype, itype, ptype, ptype, itype, itype, itype, itype, NA, NA),
#'                   vals  = c(   11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,   11 ,   12 ,   11 ,   22,   0.2, 0.3)
#' ) %>% group_by(Country, Year, matrix)
#' mats <- collapse_to_matrices(tidy, matnames = "matrix", rownames = "row", colnames = "col",
#'                                                              rowtypes = "rowtype", coltypes = "coltype",
#'                                                              values = "vals") %>%
#'           ungroup
#' expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
#'                      rownames = "rows", colnames = "cols",
#'                      rowtypes = "rt",   coltypes = "ct")
#' expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
#'                      rownames = "rows", colnames = "cols",
#'                      rowtypes = "rt",   coltypes = "ct", drop = 0)
expand_to_tidy <- function(.data, matnames, matvals, rownames, colnames, rowtypes, coltypes, drop = NA){
  .data %>%
    # group by everything except matvals column so that "do" will act as desired
    group_by_(.dots = setdiff(colnames(.data), matvals)) %>%
    dplyr::do(
      # Convert .data to row, col, val format
      mat_to_rowcolval(.[[matvals]][[1]], rownames = rownames, colnames = colnames,
                       rowtype = rowtypes, coltype = coltypes,
                       values = matvals, drop = drop)
    ) %>%
    # Remove the grouping
    ungroup
}
