#' Apply a function to a \code{matsindf} data frame (and more)
#'
#' Applies \code{FUN} to \code{.DF} or
#' performs the calculation specified by \code{FUN}
#' on numbers or matrices.
#' \code{FUN} must return a named list.
#'
#' If \code{...} are all named numbers or matrices
#' of the form \code{argname = m},
#' \code{.DF} is ignored, and
#' \code{m}s are passed to \code{FUN} by \code{argname}s.
#' The return value is a named list provided by \code{FUN}.
#'
#' If \code{...} are all lists of numbers or matrices
#' of the form \code{argname = l},
#' \code{.DF} is ignored, and
#' \code{FUN} is \code{Map}ped across the various \code{l}s
#' to obtain a list of named lists returned from \code{FUN}.
#' The return value is a data frame
#' whose rows are the named lists returned from \code{FUN} and
#' whose column names are the names of the list items returned from \code{FUN}.
#' The series of named lists are \code{rbind}-ed to create the output data frame.
#' Columns of \code{.DF} are not present in the return value.
#'
#' If \code{...} are all named character strings
#' of the form \code{argname = string},
#' \code{.DF} is required,
#' \code{argname}s are expected to be names of arguments to \code{FUN}, and
#' \code{string}s are expected to be column names in \code{.DF}.
#' The return value is \code{.DF} with additional columns (at right)
#' whose names are the names of list items returned from \code{FUN}.
#'
#' \code{NULL} arguments in ... are ignored for the purposes of deciding whether
#' all arguments are numbers, matrices, lists of numbers of matrieces, or named character strings.
#' However, all \code{NULL} arguments are passed to \code{FUN},
#' so \code{FUN} should be able to deal with \code{NULL} arguments appropriately.
#'
#' If \code{.DF} is present, \code{...} contains strings, and one of the \code{...} strings is not the name
#' of a column in \code{.DF},
#' \code{FUN} is called WITHOUT the argument whose column is missing.
#' I.e., that argument is treated as missing.
#' If \code{FUN} works despite the missing argument, execution proceeds.
#' If \code{FUN} cannot handle the missing argument, an error will occur in \code{FUN}.
#'
#' @param .DF the \code{matsindf} data frame
#' @param FUN the function to be applied to \code{.DF}
#' @param ... named arguments to be passed by name to \code{FUN}.
#'
#' @return a named list or a data frame. (See details.)
#'
#' @export
#'
#' @examples
#' example_fun <- function(a, b){
#'   return(list(c = sum_byname(a, b), d = difference_byname(a, b)))
#' }
#' # Single values for arguments
#' matsindf_apply(FUN = example_fun, a = 2, b = 2)
#' # Matrices for arguments
#' a <- 2 * matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
#'               dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' b <- 0.5 * a
#' matsindf_apply(FUN = example_fun, a = a, b = b)
#' # Single values in lists
#' matsindf_apply(FUN = example_fun, a = list(2, 2), b = list(1, 2))
#' # Matrices in lists
#' matsindf_apply(FUN = example_fun, a = list(a, a), b = list(b, b))
#' # Single numbers in a data frame
#' DF <- data.frame(a = c(4, 4, 5), b = c(4, 4, 4))
#' matsindf_apply(DF, FUN = example_fun, a = "a", b = "b")
#' # Matrices in data frames (matsindf)
#' DF2 <- data.frame(a = I(list(a, a)), b = I(list(b,b)))
#' matsindf_apply(DF2, FUN = example_fun, a = "a", b = "b")
matsindf_apply <- function(.DF = NULL, FUN, ...){
  # dots <- list(...)
  # dots_except_NULL <- dots[which(!as.logical(lapply(dots, is.null)))]
  # all_dots_num  <- all(lapply(dots_except_NULL, FUN = is.numeric) %>% as.logical())
  # all_dots_mats <- all(lapply(dots_except_NULL, FUN = is.matrix) %>% as.logical())
  # all_dots_list <- all(lapply(dots_except_NULL, FUN = is.list) %>% as.logical())
  # all_dots_char <- all(lapply(dots_except_NULL, FUN = is.character) %>% as.logical())
  types <- matsindf_apply_types(...)
  if (is.null(.DF) & (types$all_dots_num | types$all_dots_mats)) {
    # We're not in a data frame.
    # Simply call FUN on ...
    return(FUN(...))
  }
  if (is.null(.DF) & types$all_dots_list) {
    # All arguments are coming in as lists.
    # Map FUN across the lists.
    # The result of Map is a list containing all the rows of output.
    # But we want columns of output, so transpose.
    out_list <- transpose(Map(f = FUN, ...))
    numrows <- length(out_list[[1]])
    numcols <- length(out_list)
    # Create a data frame filled with NA values.
    out_df <- data.frame(matrix(NA, nrow = numrows, ncol = numcols)) %>% set_names(names(out_list))
    # Fill the data frame with new columns.
    for (j in 1:numcols) {
      out_df[[j]] <- out_list[[j]]
    }
    return(out_df)
  }
  if (types$all_dots_char) {
    arg_cols <- lapply(list(...), FUN = function(colname){
      return(.DF[[colname]])
    })
    # If one of the ... strings is not a name of a column in .DF,
    # it is, practically speaking, a missing argument, and we should treat it as such.
    # If an arg is not present in .DF, it will be NULL in arg_cols.
    # To treat it as "missing," we remove it from the arg_cols.
    arg_cols <- arg_cols[which(!as.logical(lapply(arg_cols, is.null)))]
    # Then, we call FUN, possibly with the missing argument.
    # If FUN can handle the missing argument, everything will be fine.
    # If not, an error will occur in FUN.
    return(do.call(matsindf_apply, args = c(list(.DF = NULL, FUN = FUN), arg_cols)) %>%
             bind_rows() %>%
             bind_cols(.DF, .))
  }

  # If we get here, we don't know how to deal with our inputs.
  # Try our best to give a meaningful error message.
  clss <- lapply(list(...), class) %>% paste(collapse = ",")
  msg <- paste(
    "unknown state in matsindf_apply",
    "... must be all same type, all single numbers, all matrices, all lists, or all character.",
    "types are:",
    clss
  )
  stop(msg)
}


matsindf_apply_types <- function(...){
  dots <- list(...)
  dots_except_NULL <- dots[which(!as.logical(lapply(dots, is.null)))]
  all_dots_num  <- all(lapply(dots_except_NULL, FUN = is.numeric) %>% as.logical())
  all_dots_mats <- all(lapply(dots_except_NULL, FUN = is.matrix) %>% as.logical())
  all_dots_list <- all(lapply(dots_except_NULL, FUN = is.list) %>% as.logical())
  all_dots_char <- all(lapply(dots_except_NULL, FUN = is.character) %>% as.logical())
  if (all_dots_mats) {
    # Matrices are numerics.
    # However, when all items in ... are matrices, we want to operate as matrices, not as numbers.
    # So, set all_dots_num to FALSE.
    all_dots_num <- FALSE
  }
  list(all_dots_num = all_dots_num, all_dots_mats = all_dots_mats, all_dots_list = all_dots_list, all_dots_char = all_dots_char)
}



