#' Apply a function to a \code{matsindf} data frame (and more)
#'
#' Applies \code{FUN} to \code{.DF} or
#' performs the calculation specified by \code{FUN}
#' on numbers or matrices.
#' \code{FUN} must return a named list.
#'
#' If \code{is.null(.DF)} and \code{...} are all named numbers or matrices
#' of the form \code{argname = m},
#' \code{m}s are passed to \code{FUN} by \code{argname}s.
#' The return value is a named list provided by \code{FUN}.
#' The arguments in \code{...} are not included in the output.
#'
#' If \code{is.null(.DF)} and \code{...} are all lists of numbers or matrices
#' of the form \code{argname = l},
#' \code{FUN} is \code{Map}ped across the various \code{l}s
#' to obtain a list of named lists returned from \code{FUN}.
#' The return value is a data frame
#' whose rows are the top-level lists returned from \code{FUN} and
#' whose column names are the names of the list items returned from \code{FUN}.
#' Columns of \code{.DF} are not included in the return value.
#'
#' If \code{!is.null(.DF)} and \code{...} are all named character strings
#' of the form \code{argname = string},
#' \code{argname}s are expected to be names of arguments to \code{FUN}, and
#' \code{string}s are expected to be column names in \code{.DF}.
#' The return value is \code{.DF} with additional columns (at right)
#' whose names are the names of list items returned from \code{FUN}.
#' When \code{.DF} contains columns whose names are same as columns added at the right,
#' a warning is emitted.
#'
#' \code{.DF} can be a list of named items in which case a list will be returned.
#'
#' If items in \code{.DF} have same names are arguments to \code{FUN},
#' it is not necessary to specify any arguments in \code{...}.
#' \code{matsindf_apply} assumes that the appropriately-named items in \code{.DF} are
#' intended to be arguments to \code{FUN}.
#' When an item name appears in both \code{...} and \code{.DF},
#' \code{...} takes precedence.
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
#' @param .DF a list of named items or a data frame
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
#' # All arguments to FUN are supplied by named items in .DF
#' matsindf_apply(list(a = 1, b = 2), FUN = example_fun)
#' # All arguments are supplied by named arguments in ..., but mix them up.
#' # Note that the named arguments override the items in .DF
#' matsindf_apply(list(a = 1, b = 2, z = 10), FUN = example_fun, a = "z", b = "b")
#' # Warning is issued when an output item has same name as an input item.
#' \dontrun{matsindf_apply(list(a = 1, b = 2, c = 10), FUN = example_fun, a = "c", b = "b")}
matsindf_apply <- function(.DF = NULL, FUN, ...){
  # dots <- list(...)
  # dots_except_NULL <- dots[which(!as.logical(lapply(dots, is.null)))]
  # all_dots_num  <- all(lapply(dots_except_NULL, FUN = is.numeric) %>% as.logical())
  # all_dots_mats <- all(lapply(dots_except_NULL, FUN = is.matrix) %>% as.logical())
  # all_dots_list <- all(lapply(dots_except_NULL, FUN = is.list) %>% as.logical())
  # all_dots_char <- all(lapply(dots_except_NULL, FUN = is.character) %>% as.logical())
  types <- matsindf_apply_types(...)
  if (is.null(.DF) & (types$all_dots_num | types$all_dots_mats)) {
    # .DF is not present, and we have numbers or matricies in the ... arguments.
    # Simply call FUN on ... .
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
  # Note that is.list(.DF) covers the cases where .DF is either a list or a data frame.
  if (is.list(.DF) & (!types$dots_present | types$all_dots_char)) {
    dots <- list(...)
    # Get the names of arguments to FUN.
    FUN_arg_names <- names(formals(get(deparse(substitute(FUN, env = .GlobalEnv)))))
    # Get arguments in dots whose names are also names of arguments to FUN
    dot_names_in_FUN <- dots[FUN_arg_names]
    # Get the names of items or columns in .DF that are also arguments to FUN,
    # but do this in a way that assumes the names of the items or columns are the names
    # to be used for the arguments.
    .DF_names_in_FUN <- (names(.DF) %>% set_names(.) %>% as.list())[FUN_arg_names]
    # Create a list of arguments to use when extracting information from .DF
    # Because dot_names is ahead of .DF_names, dot_names takes precedence over .DF_names.
    use_dots <- c(dot_names_in_FUN, .DF_names_in_FUN)[FUN_arg_names]

    # If one of the ... strings is NULL, we won't be able to
    # extract a column from .DF.
    # So, eliminate all NULLs from the ... strings.
    use_dots_not_null <- use_dots[which(!as.logical(lapply(use_dots, is.null)))]
    arg_cols <- lapply(use_dots_not_null, FUN = function(colname){
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
    result <- do.call(matsindf_apply, args = c(list(.DF = NULL, FUN = FUN), arg_cols))
    # Check to see if the names of result are the same as any names of .DF.
    # If so, emit a warning.
    result_names <- names(result)
    common_names <- intersect(result_names, names(.DF))
    if (length(common_names) > 0) {
      warning("name collision in matsindf_apply: ", common_names)
    }
    if (is.data.frame(.DF)) {
      return(result %>% bind_rows() %>% bind_cols(.DF, .))
    }
    if (is.list(.DF)) {
      return(c(.DF, result))
    }
    # If we get here, we have a value for .DF that doesn't make sense.
    # Throw an error.
    stop(".DF must be a data frame or a list in matsindf_apply, was ", class(.DF))
  }

  # If we get here, we don't know how to deal with our inputs.
  # Try our best to give a meaningful error message.
  clss <- lapply(list(...), class) %>% paste(collapse = ",")
  msg <- paste(
    "unknown state in matsindf_apply",
    "... must be missing or all same type: all single numbers, all matrices, all lists, or all character.",
    "types are:",
    clss
  )
  stop(msg)
}


#' Determine types of ... argument for matsindf_apply
#'
#' This is a convenience function that returns a logical list for the types of \code{...}
#' with components named \code{all_dots_num}, \code{all_dots_mats},
#' \code{all_dots_list}, and \code{all_dots_char}.
#'
#' When arguments are present in \code{...}, \code{dots_present} is \code{TRUE} but \code{FALSE} otherwise.
#' When all items in \code{...} are single numbers, \code{all_dots_num} is \code{TRUE} and all other list members are \code{FALSE}.
#' When all items in \code{...} are matrices, \code{all_dots_mats} is \code{TRUE} and all other list members are \code{FALSE}.
#' When all items in \code{...} are lists, \code{all_dots_list} is \code{TRUE} and all other list members are \code{FALSE}.
#' When all items in \code{...} are character strings, \code{all_dots_char} is \code{TRUE} and all other list members are \code{FALSE}.
#'
#' @param ... the list of arguments to be checked
#'
#' @return A logical list with components named \code{dots_present},
#' \code{all_dot_num}, \code{all_dots_mats},
#' \code{all_dots_list}, and \code{all_dots_char}.
#'
#' @export
#'
#' @examples
#' matsindf_apply_types(a = 1, b = 2)
#' matsindf_apply_types(a = matrix(c(1, 2)), b = matrix(c(2, 3)))
#' matsindf_apply_types(a = list(1, 2), b = list(3, 4), c = list(5, 6))
#' matsindf_apply_types(a = "a", b = "b", c = "c")
matsindf_apply_types <- function(...){
  dots <- list(...)
  dots_present <- length(dots) > 0
  if (!dots_present) {
    all_dots_num  <- FALSE
    all_dots_mats <- FALSE
    all_dots_list <- FALSE
    all_dots_char <- FALSE
  } else {
    # arguments are present in the ... argument.
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
  }
  list(dots_present = dots_present,
       all_dots_num = all_dots_num,
       all_dots_mats = all_dots_mats,
       all_dots_list = all_dots_list,
       all_dots_char = all_dots_char)
}



