#' Convert a matrix to a data frame with rows, columns, and values.
#'
#' This function "expands" a matrix into a tidy data frame with
#' a values column and
#' factors for row names, column names, row types, and column types.
#' Optionally, values can be dropped.
#'
#' @param  .matrix The IO-style matrix to be converted to a data frame with rows, columns, and values.
#' @param  matvals A string for the name of the output column containing values. Default is "matvals".
#' @param rownames A string for the name of the output column containing row names. Default is "rownames".
#' @param colnames A string for the name of the output column containing column names. Default is "colnames".
#' @param rowtypes A string for the name of the output column containing row types. Default is "rowtypes".
#' @param coltypes A string for the name of the output column containing column types. Default is "coltypes".
#' @param     drop If specified, the value to be dropped from output. Default is `NA`.
#'                 For example, `drop = 0` will cause `0` entries in the matrices to be deleted from output.
#'                 If `NA`, no values are dropped from output.
#'
#' @return A data frame with rows, columns, and values.
#'
#' @export
#'
#' @examples
#' library(matsbyname)
#' data <- data.frame(Country  = c("GH", "GH", "GH"),
#'                    rows = c( "c1",  "c1", "c2"),
#'                    cols = c( "i1",  "i2", "i2"),
#'                    rt = c("Commodities", "Commodities", "Commodities"),
#'                    ct = c("Industries", "Industries", "Industries"),
#'                    vals = c(  11  ,  12,   22 ))
#' data
#' A <- data %>%
#'   rowcolval_to_mat(rownames = "rows", colnames = "cols",
#'                    rowtypes = "rt",   coltypes = "ct", matvals = "vals")
#' A
#' mat_to_rowcolval(A, rownames = "rows", colnames = "cols",
#'                  rowtypes = "rt", coltypes = "ct", matvals = "vals")
#' mat_to_rowcolval(A, rownames = "rows", colnames = "cols",
#'                  rowtypes = "rt", coltypes = "ct", matvals = "vals", drop = 0)
#' # This also works for single values
#' mat_to_rowcolval(2, matvals = "vals",
#'                  rownames = "rows", colnames = "cols",
#'                  rowtypes = "rt", coltypes = "ct")
#' mat_to_rowcolval(0, matvals = "vals",
#'                  rownames = "rows", colnames = "cols",
#'                  rowtypes = "rt", coltypes = "ct", drop = 0)
mat_to_rowcolval <- function(.matrix, matvals = "matvals",
                             rownames = "rownames", colnames = "colnames",
                             rowtypes = "rowtypes", coltypes = "coltypes",
                             drop = NA){
  if (matsbyname::is_matrix_or_Matrix(.matrix)) {
    if (matsbyname::is.Matrix(.matrix)) {
      temp <- as.matrix(.matrix) %>%
        matsbyname::setrowtype(matsbyname::rowtype(.matrix)) %>%
        matsbyname::setcoltype(matsbyname::coltype(.matrix))
      .matrix <- temp
    }
    out <- .matrix %>%
      data.frame(check.names = FALSE, stringsAsFactors = FALSE) %>%
      tibble::rownames_to_column(var = rownames) %>%
      tidyr::gather(key = !!colnames, value = !!matvals, !!!colnames(.matrix))
    if (!is.null(matsbyname::rowtype(.matrix))) {
      out[[rowtypes]] <- matsbyname::rowtype(.matrix)
    }
    if (!is.null(matsbyname::coltype(.matrix))) {
      out[[coltypes]] <- matsbyname::coltype(.matrix)
    }
  } else if ((is.numeric(.matrix) | is.logical(.matrix)) & length(.matrix) == 1) {
    # We have a single value. Construct a mostly-empty data frame.
    out <- data.frame(r = NA, c = NA, v = .matrix, rt = NA, ct = NA, stringsAsFactors = FALSE)
    names(out) <- c(rownames, colnames, matvals, rowtypes, coltypes)
  } else {
    stop(paste("Unknown type of .matrix in mat_to_rowcolval", .matrix,
               "of class", class(.matrix), "and length", length(.matrix)))
  }
  if (!is.na(drop)) {
    out <- out[out[[matvals]] != drop, ]
  }
  return(out)
}

#' Collapse a tidy data frame into a matrix with named rows and columns
#'
#' Columns not specified in one of `rownames`, `colnames`, `rowtype`, `coltype`, or `values`
#' are silently dropped.
#' `rowtypes` and `coltypes` are added as attributes to the resulting matrix
#' (via `matsbyname::setrowtype()` and `matsbyname::setcoltype()`.
#' The resulting matrix is a (under the hood) a data frame.
#' If both `rownames` and `colnames` columns of `.DF` contain `NA`,
#' it is assumed that this is a single value, not a matrix,
#' in which case the value in the `values` column is returned.
#'
#' Note that two types of matrices can be created, a `matrix` or a `Matrix`.
#' `Matrix` has the advantage of representing sparse matrices with less memory
#' (and disk space).
#' `Matrix` objects are created by `matsbyname::Matrix()`.
#'
#' @param .DF       A tidy data frame containing columns for row names, column names, and values.
#' @param matvals   The name of the column in `.DF` containing values with which to fill the matrix (a string). Default is "matvals".
#' @param rownames  The name of the column in `.DF` containing row names (a string). Default is "rownames".
#' @param colnames  The name of the column in `.DF` containing column names (a string). Default is "colnames".
#' @param rowtypes  An optional string identifying the types of information found in rows of the matrix to be constructed. Default is "rowtypes".
#' @param coltypes  An optional string identifying the types of information found in columns of the matrix to be constructed. Default is "coltypes".
#' @param fill      The value for missing entries in the resulting matrix. default is `0`.
#' @param matrix.class `r lifecycle::badge("deprecated")` Use `matrix_class` instead.
#' @param matrix_class One of "matrix" or "Matrix".
#'                     "matrix" creates a `base::matrix` object with the `matrix()` function.
#'                     "Matrix" creates a `Matrix::Matrix` object using the `matsbyname::Matrix()` function.
#'                     This could be a sparse matrix.
#'                     Default is "matrix".
#'
#' @return A matrix with named rows and columns and, optionally, row and column types.
#'
#' @export
#'
#' @examples
#' library(matsbyname)
#' library(dplyr)
#' data <- data.frame(Country  = c("GH", "GH", "GH"),
#'                    rows = c( "c 1",  "c 1", "c 2"),
#'                    cols = c( "i 1",  "i 2", "i 2"),
#'                    vals = c(   11  ,   12,    22 ))
#' A <- rowcolval_to_mat(data, rownames = "rows", colnames = "cols", matvals = "vals")
#' A
#' rowtype(A) # NULL, because types not set
#' coltype(A) # NULL, because types not set
#' B <- rowcolval_to_mat(data, rownames = "rows", colnames = "cols", matvals = "vals",
#'                             rowtypes  = "Commodities", coltypes  = "Industries")
#' B
#' C <- data %>% bind_cols(data.frame(rt = c("Commodities", "Commodities", "Commodities"),
#'                                    ct = c("Industries", "Industries", "Industries"))) %>%
#'   rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
#'                    rowtypes = "rt", coltypes = "ct")
#' C
#' # Also works for single values if both the rownames and colnames columns contain NA
#' data2 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA),
#'   rowtypes = c(NA), coltypes = c(NA), vals = c(2))
#' data2 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
#'   rowtypes = "rowtypes", coltypes = "coltypes")
#' data3 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA), vals = c(2))
#' data3 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals")
#' # Fails when rowtypes or coltypes not all same. In data3, column rt is not all same.
#' data4 <- data %>% bind_cols(data.frame(rt = c("Commodities", "Industries", "Commodities"),
#'                                        ct = c("Industries", "Industries", "Industries")))
#' \dontrun{rowcolval_to_mat(data4, rownames = "rows", colnames = "cols",
#'                           matvals = "vals", rowtypes = "rt", coltypes = "ct")}
rowcolval_to_mat <- function(.DF, matvals = "matvals",
                             rownames = "rownames", colnames = "colnames",
                             rowtypes = "rowtypes", coltypes = "coltypes",
                             fill = 0,
                             matrix.class = lifecycle::deprecated(),
                             matrix_class = c("matrix", "Matrix")) {
  if (lifecycle::is_present(matrix.class)) {
    lifecycle::deprecate_warn(when = "0.4.3",
                              what = "create_matrix_byname(matrix.class)",
                              with = "create_matrix_byname(matrix_class)")
    matrix_class <- matrix.class
  }

  matrix_class <- match.arg(matrix_class)

  if (!is.null(rowtypes)) {
    # If rowtype is supplied and is not NA, check if it is one of the columns of .DF
    if (rowtypes %in% colnames(.DF)) {
      # Only do this if none of the entries in this column are NA. If any of the entries are NA skip this
      if (!any(is.na(.DF[[rowtypes]]))) {
        # Check if all entries in the rowtype column are the same
        rt <- .DF[[rowtypes]]
        if (any(rt != rt[[1]])) {
          # All values in the rowtype column should be the same. If not, how are we to know which to use?
          stop(paste("Not all values in", rowtypes, "(rowtype) were same as first entry:", rt[[1]]))
        }
        # But if they are all same, use it as the rowtype
        rowtypes <- as.character(rt[[1]])
      }
    }
  }

  if (!is.null(coltypes)) {
    # If rowtype is supplied and is not NA, check if it is one of the columns of .DF
    if (coltypes %in% colnames(.DF)) {
      # Only do this if none of the entries in this column are NA. If any of the entries are NA skip this
      if (!any(is.na(.DF[[coltypes]]))) {
        # Check if all entries in the rowtype column are the same
        ct <- .DF[[coltypes]]
        if (any(ct != ct[[1]])) {
          # All values in the coltype column should be the same. If not, how are we to know which to use?
          stop(paste("Not all values in", coltypes, "(coltype) were same as first entry:", ct[[1]]))
        }
        # But if they are all same, use it as the rowtype
        coltypes <- as.character(ct[[1]])
      }
    }
  }

  # If the data have NA for row, and col, we have a single value.  Extract and return.
  singles <- .DF %>%
    dplyr::filter(is.na(!!as.name(rownames)) & is.na(!!as.name(colnames)))

  if (nrow(singles) == 1) {
    return(.DF[[matvals]][[1]])
  }

  # The remainder of the rows have matrix information stored in the columns
  # rownames, colnames, rowtype, coltype
  # Put that data in a matrix and return it.
  out <- .DF %>%
    dplyr::select(!!rownames, !!colnames, !!matvals) %>%
    # It is possible to have rows with the same Industry in .DF,
    # because multiple fuel sources can make the same type of output
    # from identical industries.
    # For example, in Ghana, 2011, Industrial heat/furnace consumes
    # both Fuel oil and Refinery gas to make MTH.200.C.
    # To avoid problems below, we can to summarise all of the rows
    # with same rownames and colnames into one.
    dplyr::group_by_at(c(rownames, colnames)) %>%
    dplyr::summarise(!!matvals := sum(!!as.name(matvals))) %>%
    tidyr::spread(key = !!colnames, value = !!matvals, fill = fill) %>%
    tibble::remove_rownames() %>%
    data.frame(check.names = FALSE, stringsAsFactors = FALSE) %>% # Avoids munging names of columns
    tibble::column_to_rownames(var = rownames) %>%
    as.matrix() %>%
    matsbyname::setrowtype(rowtype = rowtypes) %>% matsbyname::setcoltype(coltype = coltypes)
  if (matrix_class == "Matrix") {
    out <- matsbyname::Matrix(out)
  }
  return(out)
}


#' Index a column in a data frame by groups relative to an initial year
#'
#' This function indexes (by ratio) variables in \code{vars_to_index}
#' to the first time in \code{time_var}
#' or to \code{index_time} (if specified).
#' Groups in \code{.DF} are both respected and required.
#' Neither \code{var_to_index} nor \code{time_var} can be in the grouping variables.
#'
#' Note that this function works when the variable to index is
#' a column of numbers or a column of matrices.
#'
#' @param .DF the data frame in which the variables are contained
#' @param var_to_index the column name representing the variable to be indexed (a string)
#' @param time_var the name of the column containing time information.
#'        Default is "\code{Year}".
#' @param index_time the time to which data in \code{var_to_index} are indexed.
#'        If \code{NULL} (the default), \code{index_time} is set to the first time of each group.
#' @param indexed_var the name of the indexed variable. Default is "\code{<<var_to_index>>_<<suffix>>}".
#' @param suffix the suffix to be appended to the indexed variable. Default is "\code{_indexed}".
#'
#' @return a data frame with same number of rows as \code{.DF} and the following columns:
#' grouping variables of \code{.DF}, \code{var_to_index}, \code{time_var},
#' and one additional column containing indexed \code{var_to_index}
#' named with the value of \code{indexed_var}.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' DF <- data.frame(Year = c(2000, 2005, 2010), a = c(10, 15, 20), b = c(5, 5.5, 6)) %>%
#'   gather(key = name, value = var, a, b) %>%
#'   group_by(name)
#' index_column(DF, var_to_index = "var", time_var = "Year", suffix = "_ratioed")
#' index_column(DF, var_to_index = "var", time_var = "Year", indexed_var = "now.indexed")
#' index_column(DF, var_to_index = "var", time_var = "Year", index_time = 2005,
#'              indexed_var = "now.indexed")
#' \dontrun{
#'   DF %>%
#'     ungroup() %>%
#'     group_by(name, var) %>%
#'     index_column(var_to_index = "var", time_var = "Year") # Fails! Do not group on var_to_index.
#'   DF %>%
#'     ungroup() %>%
#'     group_by(name, Year) %>%
#'     index_column(var_to_index = "var", time_var = "Year") # Fails! Do not group on time_var.
#' }
index_column <- function(.DF, var_to_index, time_var = "Year", index_time = NULL,
                         indexed_var = paste0(var_to_index, suffix), suffix = "_indexed"){
  if (var_to_index %in% dplyr::group_vars(.DF)) {
    stop(paste0("Indexing variable '", var_to_index, "' in groups of .DF in index_column."))
  }
  if (time_var %in% dplyr::group_vars(.DF)) {
    stop(paste0("Time variable '", time_var, "' in groups of .DF in index_column."))
  }
  var_to_index_init <- as.name(paste0(var_to_index, "_init"))
  var_to_index <- as.name(var_to_index)
  time_var <- as.name(time_var)
  indexed_var <- as.name(indexed_var)

  # We need to make two new columns in the incoming data frame.
  # Ensure that they are not already present.
  verify_cols_missing(.DF, newcols = c(var_to_index_init, indexed_var))

  # IndexYearData is a data frame containing the value of var_to_index in the indexing year.
  if (is.null(index_time)) {
    # Set IndexYearData to first year data for each group.
    IndexYearData <- .DF %>%
      dplyr::summarise(
        !!time_var := min(!!time_var)
      ) %>%
      dplyr::inner_join(.DF, by = c(dplyr::group_vars(.DF), as.character(time_var)))
  } else {
    # We have an index_time and should use it.
    # Set IndexYearData to data from index year for each group.
    IndexYearData <- .DF %>%
      dplyr::filter(!!time_var == index_time)
  }

  IndexYearData <- IndexYearData %>%
    dplyr::rename(
      !!var_to_index_init := !!var_to_index
    ) %>%
    # Eliminate year column
    dplyr::select(-(!!time_var))

  # Bring together and return
  .DF %>%
    dplyr::right_join(IndexYearData, by = dplyr::group_vars(.DF)) %>%
    dplyr::mutate(
      # !!indexed_var := !!var_to_index / !!var_to_index_init
      !!indexed_var := matsbyname::quotient_byname(!!var_to_index, !!var_to_index_init)
    ) %>%
    # Remove var_to_index_init
    dplyr::select(-(!!var_to_index_init))
}


#' Verify that column names in a data frame are not already present
#'
#' In the \code{Recca} package, many functions add columns to an existing data frame.
#' If the incoming data frame already contains columns with the names of new columns to be added,
#' a name collision could occur, deleting the existing column of data.
#' This function provides a way to quickly check whether \code{newcols} are already present in
#' \code{.DF}.
#'
#' This function terminates execution if a column of \code{.DF} will be overwritten
#' by one of the \code{newcols}.
#'
#' @param .DF the data frame to which \code{newcols} are to be added
#' @param newcols a single string, a single name,
#'                a vector of strings representing the names of new columns to be added to \code{.DF}, or
#'                a vector of names of new columns to be added to \code{.DF}
#'
#' @return \code{NULL}. This function should be called for its side effect of checking the validity
#'         of the names of \code{newcols} to be added to \code{.DF}.
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1,2), b = c(3,4))
#' verify_cols_missing(df, "d") # Silent. There will be no problem adding column "d".
#' newcols <- c("c", "d", "a", "b")
#' \dontrun{verify_cols_missing(df, newcols)} # Error: a and b are already in df.
verify_cols_missing <- function(.DF, newcols){
  if (!is.vector(newcols)) {
    newcols <- c(newcols)
  }
  df_names <- names(.DF)
  if (any(newcols %in% df_names)) {
    violators <- paste0("'", newcols[which(newcols %in% df_names)], "'", collapse = ", ")
    stop(paste0("column(s) ", violators, " is (are) already column names in data frame '",
                deparse(substitute(.DF)), "'"))
  }
  invisible(NULL)
}


#' Get symbols for all columns except ...
#'
#' This convenience function performs a set difference between
#' the columns of `.DF` and the variable names (or symbols) given in `...`.
#'
#' @param .DF A data frame whose variable names are to be differenced.
#' @param ... A string, strings, vector of strings, or list of strings representing column names to be subtracted from the names of `.DF`/
#' @param .symbols A boolean that defines the return type: `TRUE` for symbols, `FALSE` for strings.
#'
#' @return A vector of symbols (when `.symbols = TRUE`) or
#'         strings (when `symbol = FALSE`) containing all variables names except those given in `...`.
#'
#' @export
#'
#' @examples
#' DF <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
#' everything_except(DF, "a", "b")
#' everything_except(DF, "a", "b", symbols = FALSE)
#' everything_except(DF, c("a", "b"))
#' everything_except(DF, list("a", "b"))
everything_except <- function(.DF, ..., .symbols = TRUE){
  dots <- list(...) %>% unlist()
  if (all(is.character(dots))) {
    to_exclude <- dots
  } else {
    # Assume all items in ... are symbols.
    # Convert symbols to strings using the deparse(substitute()) trick.
    to_exclude <- deparse(substitute(...))
  }
  grouping_vars <- base::setdiff(names(.DF), to_exclude)
  if (!.symbols) {
    return(grouping_vars)
  }
  sapply(grouping_vars, as.name, USE.NAMES = FALSE)
}


#' Group by all variables except some
#'
#' This is a convenience function
#' that allows grouping of a data frame by all variables (columns)
#' except those variables specified in `...`.
#'
#' @param .DF A data frame to be grouped.
#' @param ... A string, strings, vector of strings, or list of strings representing column names to be excluded from grouping.
#' @param .add When `.add = FALSE`, the default, `dplyr::group_by()` will override existing groups.
#'             To add to the existing groups, use `.add = TRUE`.
#' @param .drop When `.drop = TRUE`, empty groups are dropped. Default is `FALSE`.
#'
#' @return A grouped version of `.DF`.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' DF <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
#' group_by_everything_except(DF) %>% group_vars()
#' group_by_everything_except(DF, NULL) %>% group_vars()
#' group_by_everything_except(DF, c()) %>% group_vars()
#' group_by_everything_except(DF, list()) %>% group_vars()
#' group_by_everything_except(DF, c) %>% group_vars()
#' group_by_everything_except(DF, "a") %>% group_vars()
#' group_by_everything_except(DF, "c") %>% group_vars()
#' group_by_everything_except(DF, c("a", "c")) %>% group_vars()
#' group_by_everything_except(DF, c("a")) %>% group_vars()
#' group_by_everything_except(DF, list("a")) %>% group_vars()
group_by_everything_except <- function(.DF, ..., .add = FALSE, .drop = FALSE){
  grouping_cols <- do.call(everything_except, list(.DF = .DF, ...))
  .DF %>%
    dplyr::group_by(!!!grouping_cols, .add = .add, .drop = .drop)
}


#' Add a column of matrix names to tidy data frame
#'
#' @param .DF a data frame with \code{ledger_side_colname} and \code{energy_colname}.
#' @param ledger_side_colname the name of the column in \code{.DF} that contains ledger side
#'        (a string). Default is "\code{Ledger.side}".
#' @param energy_colname the name of the column in \code{.DF} that contains energy values
#'        (a string). Default is "\code{E.ktoe}".
#' @param supply_side the identifier for items on the supply side of the ledger (a string).
#'        Default is "\code{Supply}".
#' @param consumption_side the identifier for items on the consumption side
#'        of the ledger (a string). Default is "\code{Consumption}".
#' @param matname_colname the name of the output column containing the name of the matrix
#'        in which this row belongs (a string). Default is "\code{UVY}".
#' @param U_name the name for the use matrix (a string). Default is "\code{U}".
#' @param V_name the name for the make matrix (a string). Default is "\code{V}".
#' @param Y_name the name for the final demand matrix (a string). Default is "\code{Y}".
#'
#' @return \code{.DF} with an added column, \code{UVY_colname}.
#'
#' @examples
#' matsindf:::add_UKEnergy2000_matnames(UKEnergy2000)
add_UKEnergy2000_matnames <- function(.DF,
                         # Input columns
                         ledger_side_colname = "Ledger.side",
                         energy_colname = "E.ktoe",
                         # Input identifiers for supply and consumption
                         supply_side = "Supply",
                         consumption_side = "Consumption",
                         # Output column
                         matname_colname = "matname",
                         # Ouput identifiers for use, make, and final demand matrices
                         U_name = "U",
                         V_name = "V",
                         Y_name = "Y"){
  .DF %>% dplyr::mutate(
    # Add a column that indicates the matrix in which this entry belongs.
    !!as.name(matname_colname) := dplyr::case_when(
      # All negative values on the Supply side of the ledger belong in the use (U) matrix.
      (!!as.name(ledger_side_colname)) == supply_side & (!!as.name(energy_colname)) <= 0 ~ U_name,
      # All positive values on the Supply side of the ledger belong in the make (V) matrix.
      (!!as.name(ledger_side_colname)) == supply_side & (!!as.name(energy_colname)) > 0 ~ V_name,
      # All Consumption items belong in the final demand (Y) matrix.
      (!!as.name(ledger_side_colname)) == consumption_side ~ Y_name,
      # Identify any places where our logic is faulty.
      TRUE ~ NA_character_
    )
  )
}

#' Add row, column, row type, and column type metadata
#'
#' @param .DF a data frame containing \code{matname_colname}.
#' @param matname_colname the name of the column in \code{.DF} that contains names of matrices
#'        (a string).  Default is "\code{matname}".
#' @param U_name the name for use matrices (a string). Default is "\code{U}".
#' @param V_name the name for make matrices (a string). Default is "\code{V}".
#' @param Y_name the name for final demand matrices (a string). Default is "\code{Y}".
#' @param product_colname the name of the column in \code{.DF} where Product names
#'        is found (a string). Default is "\code{Product}".
#' @param flow_colname the name of the column in \code{.DF} where Flow information is found
#'        (a string).
#'        The Flow column usually contains the industries involved in this flow.
#'        Default is "\code{Flow}".
#' @param industry_type the name that identifies production industries and
#'        and transformation processes (a string). Default is "\code{Industry}".
#' @param product_type the name that identifies energy carriers (a string).
#'        Default is "\code{Product}".
#' @param sector_type the name that identifies final demand sectors (a string).
#'        Default is "\code{Sector}".
#' @param rowname_colname the name of the output column that contains row names for matrices
#'        (a string). Default is "\code{rowname}".
#' @param colname_colname the name of the output column that contains column names for matrices
#'        (a string). Default is "\code{colname}".
#' @param rowtype_colname the name of the output column that contains row types for matrices
#'        (a string). Default is "\code{rowtype}".
#' @param coltype_colname the name of the output column that contains column types for matrices
#'        (a string). Default is "\code{coltype}".
#'
#' @return \code{.DF} with additional columns named
#'         \code{rowname_colname}, \code{colname_colname},
#'         \code{rowtype_colname}, and \code{coltype_colname}.
#'
#' @examples
#' UKEnergy2000 %>%
#'   matsindf:::add_UKEnergy2000_matnames(.) %>%
#'   matsindf:::add_UKEnergy2000_row_col_meta(.)
add_UKEnergy2000_row_col_meta <- function(.DF,
                                          # Input column containing matrix names
                                          matname_colname = "matname",
                                          U_name = "U", V_name = "V", Y_name = "Y",
                                          product_colname = "Product", flow_colname = "Flow",
                                          industry_type = "Industry", product_type = "Product",
                                          sector_type = "Sector",
                                          # Output columns
                                          rowname_colname = "rowname", colname_colname = "colname",
                                          rowtype_colname = "rowtype", coltype_colname = "coltype"){
  .DF %>%
    dplyr::mutate(
      !!as.name(rowname_colname) := dplyr::case_when(
        (!!as.name(matname_colname)) == U_name ~ !!as.name(product_colname),
        (!!as.name(matname_colname)) == V_name ~ !!as.name(flow_colname),
        (!!as.name(matname_colname)) == Y_name ~ !!as.name(product_colname),
        TRUE ~ NA_character_
      ),
      !!as.name(colname_colname) := dplyr::case_when(
        (!!as.name(matname_colname)) == U_name ~ !!as.name(flow_colname),
        (!!as.name(matname_colname)) == V_name ~ !!as.name(product_colname),
        (!!as.name(matname_colname)) == Y_name ~ !!as.name(flow_colname),
        TRUE ~ NA_character_
      ),
      !!as.name(rowtype_colname) := dplyr::case_when(
        (!!as.name(matname_colname)) == U_name ~ product_type,
        (!!as.name(matname_colname)) == V_name ~ industry_type,
        (!!as.name(matname_colname)) == Y_name ~ product_type,
        TRUE ~ NA_character_
      ),
      !!as.name(coltype_colname) := dplyr::case_when(
        (!!as.name(matname_colname)) == U_name ~ industry_type,
        (!!as.name(matname_colname)) == V_name ~ product_type,
        (!!as.name(matname_colname)) == Y_name ~ sector_type,
        TRUE ~ NA_character_
      )
    )
}


#' Create a message from a data frame
#'
#' This function is especially helpful for cases when a data frame
#' of missing or unset values is at hand.
#' Trim unneeded columns, then call this function
#' to create a string with rows separated by semicolons and entries separated by commas.
#'
#' @param df The data frame to be converted to a message
#'
#' @return A string with rows separated by semicolons and entries separated by commas.
#'
#' @export
#'
#' @examples
#' data.frame(a = c(1, 2, 3), b = c("a", "b", "c")) |>
#'   df_to_msg()
df_to_msg <- function(df) {
  titles <- paste0(names(df), collapse = ", ")
  rows <- lapply(1:nrow(df), function(r) {
    df[r, ] %>%
      as.list() %>%
      paste(collapse = ", ")
  }) %>%
    paste(collapse = "\n")
  gridline <- rep("=", times = nchar(titles)) |>
    paste0(collapse = "")
  paste0(titles, "\n", gridline, "\n", rows)
}


#' Find columns that contain matrices
#'
#' It is often helpful to find the columns of a `matsindf`
#' data frame that contain exclusively or some matrices.
#' This function helps with that task.
#'
#' By default, a column is considered a matrix column if `all()` of the
#' rows contain matrices.
#' Use the `.test_any` argument to modify this behavior.
#'
#' By default, the vector of integers returned from this function
#' is named by the columns.
#' Use the `.drop_names` function to modify this behavior.
#'
#' @param .df The data frame to be queried for matrix columns.
#' @param .drop_names A boolean that tells whether to remove the names from
#'                    the returned integer vector.
#'                    Default is `FALSE`.
#' @param .any A boolean that tells whether a column is reported when
#'             `any()` of the rows contain matrices
#'             (instead of `all()` rows contain matrices).
#'             Default is `FALSE`, in which case
#'             all entries in a column must be a matrix for
#'             the column to be reported.
#'
#' @return A vector of integers saying which columns contain matrices.
#'
#' @export
#'
#' @examples
#' tidy <- tibble::tibble(matrix = c("V1", "V1", "V1", "V2", "V2"),
#'                          row = c("i1", "i1", "i2", "i1", "i2"),
#'                          col = c("p1", "p2", "p2", "p1", "p2"),
#'                          vals = c(1, 2, 3, 4, 5)) %>%
#'   dplyr::mutate(
#'     rowtypes = "Industries",
#'     coltypes  = "Products"
#'   ) %>%
#'   dplyr::group_by(matrix)
#' matsdf <- tidy %>%
#'   collapse_to_matrices(matnames = "matrix", matvals = "vals",
#'                        rownames = "row", colnames = "col",
#'                        rowtypes = "rowtypes", coltypes = "coltypes")
#' matsdf
#' matrix_cols(matsdf)
#' matrix_cols(matsdf, .drop_names = TRUE)
matrix_cols <- function(.df, .drop_names = FALSE, .any = FALSE) {
  cnames <- colnames(.df)
  out <- sapply(cnames, FUN = function(this_cname) {
    this_col <- .df[[this_cname]]
    true_false <- sapply(this_col, FUN = function(this_item) {
      matsbyname::is_matrix_or_Matrix(this_item)
    })
    if (.any) {
      return(any(true_false))
    } else {
      return(all(true_false))
    }
  }) %>%
    which()
  if (.drop_names) {
    out <- unname(out)
  }
  return(out)
}


