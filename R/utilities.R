#' Convert a matrix to a data frame with rows, columns, and values.
#'
#' This function "unwraps" a matrix into a tidy data frame with
#' a values column and
#' factors for row names, column names, row types, and column types.
#' Optionally, values can be dropped.
#'
#' @param  .matrix the IO-style matrix to be converted to a data frame with rows, columns, and values
#' @param   values a string for the name of the output column containing values
#' @param rownames a string for the name of the output column containing row names
#' @param colnames a string for the name of the output column containing column names
#' @param  rowtype a string for the name of the output column containing row types
#' @param  coltype a string for the name of the output column containing column types
#' @param     drop if specified, the value to be dropped from output.
#' For example, \code{drop = 0} will cause \code{0} entries in the matrices to be deleted from output.
#' If \code{NA}, no values are dropped from output.
#'
#' @return a data frame with rows, columns, and values
#' @export
#'
#' @examples
#' library(magrittr)
#' library(byname)
#' data <- data.frame(Country  = c("GH", "GH", "GH"),
#'                    rows = c( "c1",  "c1", "c2"),
#'                    cols = c( "i1",  "i2", "i2"),
#'                    rt = c("Commodities", "Commodities", "Commodities"),
#'                    ct = c("Industries", "Industries", "Industries"),
#'                    vals = c(  11  ,  12,   22 ))
#' data
#' A <- data %>%
#'   rowcolval_to_mat(rownames = "rows", colnames = "cols",
#'                     rowtype = "rt",    coltype = "ct", values = "vals")
#' A
#' mat_to_rowcolval(A, rownames = "rows", colnames = "cols",
#'                  rowtype = "rt", coltype = "ct", values = "vals")
#' mat_to_rowcolval(A, rownames = "rows", colnames = "cols",
#'                  rowtype = "rt", coltype = "ct", values = "vals", drop = 0)
#' # This also works for single values
#' mat_to_rowcolval(2, values = "vals",
#'                  rownames = "rows", colnames = "cols",
#'                  rowtype = "rt", coltype = "ct")
#' mat_to_rowcolval(0, values = "vals",
#'                  rownames = "rows", colnames = "cols",
#'                  rowtype = "rt", coltype = "ct", drop = 0)
mat_to_rowcolval <- function(.matrix, values, rownames, colnames, rowtype, coltype, drop = NA){
  if (is.matrix(.matrix)) {
    out <- .matrix %>%
      setrowtype(rowtype(.matrix)) %>%
      setcoltype(coltype(.matrix)) %>%
      data.frame(check.names = FALSE) %>%
      rownames_to_column(var = rownames) %>%
      gather(key = !!colnames, value = !!values, !!!colnames(.matrix))
    if (!is.null(rowtype(.matrix))) {
      out[[rowtype]] <- rowtype(.matrix)
    }
    if (!is.null(coltype(.matrix))) {
      out[[coltype]] <- coltype(.matrix)
    }
  } else if ((is.numeric(.matrix) | is.logical(.matrix)) & length(.matrix) == 1) {
    # We have a single value. Construct a mostly-empty data frame.
    out <- data.frame(r = NA, c = NA, v = .matrix, rt = NA, ct = NA)
    names(out) <- c(rownames, colnames, values, rowtype, coltype)
  } else {
    stop(paste("Unknown type of .matrix in mat_to_rowcolval", .matrix,
               "of class", class(.matrix), "and length", length(.matrix)))
  }
  if (!is.na(drop)) {
    out <- out[out[[values]] != drop, ]
  }
  return(out)
}

#' Convert a tidy data frame into a matrix with named rows and columns
#'
#' Columns not specified in one of \code{rownames}, \code{colnames}, \code{rowtype}, \code{coltype}, or \code{values}
#' are silently dropped.
#' \code{rowtypes} and \code{coltypes} are added as attributes to the resulting matrix
#' (via \code{\link{setrowtype}} and \code{\link{setcoltype}}).
#' The resulting matrix is a (under the hood) a data frame.
#' If both \code{rownames} and \code{colnames} columns of \code{.data} contain \code{NA},
#' it is assumed that this is a single value, not a matrix,
#' in which case the value in the \code{values} column is returned.
#'
#' @param .data a tidy data frame containing columns for row names, column names, and values
#' @param rownames the name of the column in \code{.data} containing row names (a string)
#' @param colnames the name of the column in \code{.data} containing column names (a string)
#' @param values the name of the column in \code{.data} containing values with which to fill the matrix (a string)
#' @param fill the value for missing entries in the resulting matrix (default is \code{0})
#' @param rowtype an optional string identifying the types of information found in rows of the matrix to be constructed
#' @param coltype an optional string identifying the types of information found in columns of the matrix to be constructed
#'
#' @return a matrix with named rows and columns and, optionally, row and column types
#' @export
#'
#' @examples
#' library(magrittr)
#' library(byname)
#' library(dplyr)
#' data <- data.frame(Country  = c("GH", "GH", "GH"),
#'                    rows = c( "c 1",  "c 1", "c 2"),
#'                    cols = c( "i 1",  "i 2", "i 2"),
#'                    vals = c(   11  ,   12,    22 ))
#' A <- rowcolval_to_mat(data, rownames = "rows", colnames = "cols", values = "vals")
#' A
#' rowtype(A) # NULL, because types not set
#' coltype(A) # NULL, because types not set
#' B <- rowcolval_to_mat(data, rownames = "rows", colnames = "cols", values = "vals",
#'                             rowtype  = "Commodities", coltype  = "Industries")
#' B
#' C <- data %>% bind_cols(data.frame(rt = c("Commodities", "Commodities", "Commodities"),
#'                                    ct = c("Industries", "Industries", "Industries"))) %>%
#'   rowcolval_to_mat(rownames = "rows", colnames = "cols", values = "vals",
#'                    rowtype = "rt", coltype = "ct")
#' C
#' # Also works for single values if both the rownames and colnames columns contain NA
#' data2 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA),
#'   rowtype = c(NA), coltype = c(NA), vals = c(2))
#' data2 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", values = "vals",
#'   rowtype = "rowtype", coltype = "coltype")
#' data3 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA), vals = c(2))
#' data3 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", values = "vals")
#' # Fails when rowtype or coltype not all same. In data3, column rt is not all same.
#' data4 <- data %>% bind_cols(data.frame(rt = c("Commodities", "Industries", "Commodities"),
#'                                        ct = c("Industries", "Industries", "Industries")))
#' \dontrun{rowcolval_to_mat(data4, rownames = "rows", colnames = "cols",
#'                           values = "vals", rowtype = "rt", coltype = "ct")}
rowcolval_to_mat <- function(.data, values, rownames, colnames, rowtype = NULL, coltype = NULL, fill = 0){
  if (!is.null(rowtype)) {
    # If rowtype is supplied and is not NA, check if it is one of the columns of .data
    if (rowtype %in% colnames(.data)) {
      # Only do this if none of the entries in this column are NA. If any of the entries are NA skip this
      if (!any(is.na(.data[[rowtype]]))) {
        # Check if all entries in the rowtype column are the same
        rt <- .data[[rowtype]]
        if (any(rt != rt[[1]])) {
          # All values in the rowtype column should be the same. If not, how are we to know which to use?
          stop(paste("Not all values in", rowtype, "(rowtype) were same as first entry:", rt[[1]]))
        }
        # But if they are all same, use it as the rowtype
        rowtype <- as.character(rt[[1]])
      }
    }
  }

  if (! is.null(coltype)) {
    # If rowtype is supplied and is not NA, check if it is one of the columns of .data
    if (coltype %in% colnames(.data)) {
      # Only do this if none of the entries in this column are NA. If any of the entries are NA skip this
      if (! any(is.na(.data[[coltype]]))) {
        # Check if all entries in the rowtype column are the same
        ct <- .data[[coltype]]
        if (any(ct != ct[[1]])) {
          # All values in the coltype column should be the same. If not, how are we to know which to use?
          stop(paste("Not all values in", coltype, "(coltype) were same as first entry:", ct[[1]]))
        }
        # But if they are all same, use it as the rowtype
        coltype <- as.character(ct[[1]])
      }
    }
  }

  # If the data have NA for row, and col, we have a single value.  Extract and return.
  singles <- .data %>%
    filter(is.na(!!as.name(rownames)) & is.na(!!as.name(colnames)))

  if (nrow(singles) == 1) {
    return(.data[[values]][[1]])
  }

  # The remainder of the rows have matrix information stored in the columns
  # rownames, colnames, rowtype, coltype
  # Put that data in a matrix and return it.
  .data %>%
    select(!!rownames, !!colnames, !!values) %>%
    # It is possible to have rows with the same Industry in .data,
    # because multiple fuel sources can make the same type of output
    # from identical industries.
    # For example, in Ghana, 2011, Industrial heat/furnace consumes
    # both Fuel oil and Refinery gas to make MTH.200.C.
    # To avoid problems below, we can to summarise all of the rows
    # with same rownames and colnames into one.
    group_by_at(c(rownames, colnames)) %>%
    summarise(!!values := sum(!!as.name(values))) %>%
    spread(key = !!colnames, value = !!values, fill = fill) %>%
    remove_rownames %>%
    data.frame(check.names = FALSE) %>% # Avoids munging names of columns
    column_to_rownames(var = rownames) %>%
    as.matrix %>%
    setrowtype(rowtype) %>% setcoltype(coltype)
}
