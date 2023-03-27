#' Apply a function to a `matsindf` data frame (and more)
#'
#' Applies `FUN` to `.dat` or
#' performs the calculation specified by `FUN`
#' on numbers or matrices.
#' `FUN` must return a named list.
#' The names become columns in the return data frame.
#' The values become entries in columns in the return data frame.
#'
#' If `is.null(.dat)` and `...` are all named numbers or matrices
#' of the form `argname = m`,
#' `m`s are passed to `FUN` by `argname`s.
#' The return value is a named list provided by `FUN`.
#' The arguments in `...` are not included in the output.
#'
#' If `is.null(.dat)` and `...` are all lists of numbers or matrices
#' of the form `argname = l`,
#' `FUN` is `Map`ped across the various `l`s
#' to obtain a list of named lists returned from `FUN`.
#' The return value is a data frame
#' whose rows are the top-level lists returned from `FUN` and
#' whose column names are the names of the list items returned from `FUN`.
#' Columns of `.dat` are not included in the return value.
#'
#' If `!is.null(.dat)` and `...` are all named character strings
#' of the form `argname = string`,
#' `argname`s are expected to be names of arguments to `FUN`, and
#' `string`s are expected to be column names in `.dat`.
#' The return value is `.dat` with additional columns (at right)
#' whose names are the names of list items returned from `FUN`.
#' When `.dat` contains columns whose names are same as columns added at the right,
#' a warning is emitted.
#'
#' `.dat` can be a list of named items in which case a list will be returned.
#'
#' If items in `.dat` have same names as arguments to `FUN`,
#' it is not necessary to specify any arguments in `...`.
#' `matsindf_apply` assumes that the appropriately-named items in `.dat` are
#' intended to be arguments to `FUN`.
#' When an item name appears in both `...` and `.dat`,
#' `...` takes precedence.
#'
#' `NULL` arguments in `...` are ignored for the purposes of deciding whether
#' all arguments are numbers, matrices, lists of numbers of matrices, or named character strings.
#' However, all `NULL` arguments are passed to `FUN`,
#' so `FUN` should be able to deal with `NULL` arguments appropriately.
#'
#' If `.dat` is present, `...` contains strings, and one of the `...` strings is not the name
#' of a column in `.dat`,
#' `FUN` is called WITHOUT the argument whose column is missing.
#' I.e., that argument is treated as missing.
#' If `FUN` works despite the missing argument, execution proceeds.
#' If `FUN` cannot handle the missing argument, an error will occur in `FUN`.
#'
#' If `.dat` is a zero-row data frame, `.dat` is returned unmodified.
#' If `.dat` is a list of items with zero length, `.dat` is returned unmodified.
#' If `.dat` is `NULL` (the default) and items in `...` have zero length,
#' `...` is wrapped in a list and returned.
#'
#' @param .dat A list of named items or a data frame.
#' @param FUN The function to be applied to `.dat`.
#' @param ... Named arguments to be passed by name to `FUN`.
#'
#' @return A named list or a data frame. (See details.)
#'
#' @export
#'
#' @examples
#' library(matsbyname)
#' example_fun <- function(a, b){
#'   return(list(c = sum_byname(a, b),
#'               d = difference_byname(a, b)))
#' }
#' # Single values for arguments
#' matsindf_apply(FUN = example_fun, a = 2, b = 2)
#' # Matrices for arguments
#' a <- 2 * matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
#'               dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' b <- 0.5 * a
#' matsindf_apply(FUN = example_fun, a = a, b = b)
#' # Single values in lists are treated like columns of a data frame
#' matsindf_apply(FUN = example_fun, a = list(2, 2), b = list(1, 2))
#' # Matrices in lists are treated like columns of a data frame
#' matsindf_apply(FUN = example_fun, a = list(a, a), b = list(b, b))
#' # Single numbers in a data frame
#' DF <- data.frame(a = c(4, 4, 5), b = c(4, 4, 4))
#' matsindf_apply(DF, FUN = example_fun, a = "a", b = "b")
#' # By default, arguments to FUN come from DF
#' matsindf_apply(DF, FUN = example_fun)
#' # Now put some matrices in a data frame.
#' DF2 <- data.frame(a = I(list(a, a)), b = I(list(b,b)))
#' matsindf_apply(DF2, FUN = example_fun, a = "a", b = "b")
#' # All arguments to FUN are supplied by named items in .dat
#' matsindf_apply(list(a = 1, b = 2), FUN = example_fun)
#' # All arguments are supplied by named arguments in ..., but mix them up.
#' # Note that the named arguments override the items in .dat
#' matsindf_apply(list(a = 1, b = 2, z = 10), FUN = example_fun, a = "z", b = "b")
#' # A warning is issued when an output item has same name as an input item.
#' matsindf_apply(list(a = 1, b = 2, c = 10), FUN = example_fun, a = "c", b = "b")
#' # When a zero-row data frame supplied to .dat,
#' # .dat is returned unmodified.
#' DF3 <- DF2[0, ]
#' DF3
#' matsindf_apply(DF3, FUN = example_fun, a = "a", b = "b")
matsindf_apply <- function(.dat = NULL, FUN, ...){
  types <- matsindf_apply_types(.dat, FUN, ...)

  if (!types$.dat_null) {
    if (!types$.dat_list) {
      # Cases 6, 7, 8, 9, and 10
      # If we get here, we have a value for .dat that doesn't make sense.
      # Throw an error.
      stop(".dat must be NULL, a data frame, or a list in matsindf_apply(), was ", class(.dat))
    }
  }

  if (types$.dat_list & types$dots_present & !types$all_dots_char) {
    # Case 14
    # Get the names of the arguments to FUN
    FUN_arg_names <- types$FUN_arg_names
    # Combine the arguments in ... and .dat, keeping arguments in ... when the same name is present in both.
    new_dots <- c(list(...), .dat)[FUN_arg_names]
    # Re-call with the new arguments (neglecting the arguments to .dat)
    return(matsindf_apply(.dat = new_dots, FUN = FUN))
  }

  if (types$all_dots_num | types$all_dots_mats) {
    # Cases 2 and 12
    # ************** This probably isn't right.
    # ************** Need to account for .dat somehow!
    # .dat is not present, and we have numbers or matrices in the ... arguments.
    # Simply call FUN on ... .
    return(FUN(...))
  }

  if (types$all_dots_list | types$all_dots_vect) {
    # Cases 5 and 15
    # All arguments are coming in as lists or vectors across which FUN should be mapped.
    # Map FUN across the lists.
    # The result of Map is a list containing all the rows of output.
    # But we want columns of output, so transpose.
    # out_list <- purrr::transpose(Map(f = FUN, ...))
    # Fixing a potential bug here.
    # The result of Map is a named list.
    # But when the names are present for one list but not another,
    # binary matsbyname functions will fail.
    out_list <- Map(f = FUN, ...) |>
      unname() %>%
      purrr::transpose()
    # Work around a possible error condition here.
    numcols <- length(out_list)
    if (numcols == 0) {
      # We got nothing back from FUN.
      # Return the original input.
      if (!missing(.dat)) {
        return(.dat)
      }
      return(list(...))
    } else {
      numrows <- length(out_list[[1]])
    }
    # Create a data frame filled with NA values.
    out_df <- data.frame(matrix(NA, nrow = numrows, ncol = numcols)) |>
      magrittr::set_names(names(out_list))
    # Fill the data frame with new columns.
    for (j in 1:numcols) {
      out_df[[j]] <- out_list[[j]]
    }
    return(out_df)
  }

  # Note that is.list(.dat) covers the cases where .dat is either a list or a data frame.
  if (types$.dat_list & (!types$dots_present | types$all_dots_char)) {
    # Cases 11 and 13
    dots <- list(...)
    # Get the names of arguments to FUN.
    FUN_arg_names <- types$FUN_arg_names
    # Get arguments in dots whose names are also names of arguments to FUN
    dot_names_in_FUN <- dots[FUN_arg_names]
    # Get the names in .dat
    .dat_names <- types$.dat_names
    # Get the names of items or columns in .dat that are also arguments to FUN,
    # but do this in a way that assumes the names of the items or columns are the names
    # to be used for the arguments.
    .dat_names_in_FUN <- (.dat_names %>%
                            magrittr::set_names(.dat_names) |>
                            as.list())[FUN_arg_names]
    # Create a list of arguments to use when extracting information from .dat
    # Because dot_names is ahead of .dat_names, dot_names takes precedence over .dat_names.
    use_dots <- c(dot_names_in_FUN, .dat_names_in_FUN)[FUN_arg_names]

    # If one of the ... strings is NULL, we won't be able to
    # extract a column from .dat.
    # So, eliminate all NULLs from the ... strings.
    use_dots_not_null <- use_dots[which(!as.logical(lapply(use_dots, is.null)))]
    arg_cols <- lapply(use_dots_not_null, FUN = function(colname){
      .dat[[colname]]
    })
    # If one of the ... strings is not a name of a column in .dat,
    # it is, practically speaking, a missing argument, and we should treat it as such.
    # If an arg is not present in .dat, it will be NULL in arg_cols.
    # To treat it as "missing," we remove it from the arg_cols.
    arg_cols <- arg_cols[which(!as.logical(lapply(arg_cols, is.null)))]
    # Then, we call FUN, possibly with the missing argument(s).
    # If FUN can handle the missing argument, everything will be fine.
    # If not, an error will occur in FUN.
    result <- do.call(matsindf_apply, args = c(list(.dat = NULL, FUN = FUN), arg_cols))


    # This code is probably what I want.
    # But not sure yet. Need more testing.
    # Will comment for now.
    # dots_cols_could_be_strings <- dots[types$arg_source$dots]
    # if (types$all_dots_char) {
    #   dots_cols <- .dat[unlist(dots_cols_could_be_strings)]
    # } else {
    #   dots_cols <- dots_cols_could_be_strings
    # }
    # .dat_cols <- .dat[types$arg_source$.dat]
    # default_cols <- as.list(formals(FUN))[types$arg_source$defaults]
    #
    # arg_cols <- c(dots_cols, .dat_cols, default_cols)
    #
    #
    # result <- do.call(matsindf_apply, args = c(list(.dat = NULL, FUN = FUN), arg_cols))






    # Check to see if the names of result are the same as any names of .dat.
    # If so, emit a warning.
    common_names <- intersect(names(.dat), names(result))
    if (length(common_names) > 0) {
      warning("name collision in matsindf_apply: ", common_names)
    }
    if (is.data.frame(.dat)) {
      return(dplyr::bind_cols(.dat, dplyr::bind_rows(result)))
    }
    if (is.list(.dat)) {
      return(c(.dat, result))
    }
  }

  # Some arguments could be coming in as strings while all other arguments are of same type.
  # This outcome is possible when
  # (1) .dat is missing and
  # (2) an outer function has string defaults for names and
  # (3) some of the arguments are missing.
  # To put it another way, when there is no .dat and some arguments are specified as strings,
  # it means that the string arguments are unavailable (missing).
  # In a last-ditch effort, let's try to
  # * eliminate all strings from ...
  # * re-call this function with the remaining arguments
  # This approach will, in effect, call FUN with missing arguments.
  # If FUN can handle the missing arguments,
  # we'll get a result.
  # If FUN can't handle the missing arguments,
  # an error will occur.
  if (types$.dat_null) {
    dots <- list(...)
    chars <- lapply(dots, function(x) is.character(x)) %>% as.logical()
    dots <- dots[which(!chars)]
    if (length(dots) == 0) {
      # We have eliminated all of the arguments.
      # This is possibly an error.
      # And calling ourselves again would result in a stack overflow.
      # But we will die trying.
      # So just try to call FUN without any arguments.
      return(do.call(FUN, args = list()))
      # stop(".dat was missing and all arguments were strings")
    }
    # Now that we have eliminated the missing string arguments,
    # call ourselves again.
    return(do.call(matsindf_apply, args = c(list(.dat = NULL, FUN = FUN), dots)))
  }

  # We'll never get here, because at the top of this function, we check for
  #   if (!is.null(.dat)).
  # Then, just above, we check for
  #   (is.null(.dat))
  # So, that covers everything, and
  # there is no need for further checks.
  # Also, the code below is the only code not hit by tests,
  # resulting in code coverage less than 100%.
  # But, it is actually impossible to get here.
  # By commenting this code,
  # the package obtains 100% testing coverage.
  #
  # If we get here, we don't know how to deal with our inputs.
  # Try our best to give a meaningful error message.
  # clss <- lapply(list(...), class) %>% paste(collapse = ",")
  # msg <- paste(
  #   "unknown state in matsindf_apply",
  #   "... must be missing or all same type: all single numbers, all matrices, all lists, or all character.",
  #   "types are:",
  #   clss
  # )
  # stop(msg)
}


#' Determine types of `.dat` and `...` arguments for matsindf_apply()
#'
#' This is a convenience function that returns a list
#' for the types of `.dat` and `...` as well as names in `.dat` and `...`,
#' with components named `.dat_null`, `.dat_df`, `.dat_list`, `.dat_names`,
#' `FUN_arg_names`,
#'  `dots_present`, `all_dots_num`, `all_dots_mats`,
#' `all_dots_list`, `all_dots_vect`, `all_dots_char`, `dots_names`, and
#' `arg_source`.
#'
#' When `.dat` is a `data.frame`, both `.dat_list` and `.dat_df` are `TRUE`.
#'
#' When arguments are present in `...`, `dots_present` is `TRUE` but `FALSE` otherwise.
#' When all items in `...` are single numbers, `all_dots_num` is `TRUE` and all other list members are `FALSE`.
#' When all items in `...` are matrices, `all_dots_mats` is `TRUE` and all other list members are `FALSE`.
#' When all items in `...` are lists, `all_dots_list` is `TRUE` and all other list members are `FALSE`.
#' When all items in `...` are vectors (including lists), `all_dots_vect` is `TRUE`.
#' When all items in `...` are character strings, `all_dots_char` is `TRUE` and all other list members are `FALSE`.
#'
#' `arg_source` is a `list()` with two named member vectors, `.dat` and `dots`.
#' When `TRUE`, parameters should be taken from the named location.
#' Corresponding names will never both be `TRUE`.
#' An error occurs if a parameter needed by `FUN` is available from neither `.dat` nor `...`.
#'
#' @param .dat The `.dat` argument to be checked.
#' @param FUN The function sent to `matsindf_apply()`.
#' @param ... The list of arguments to be checked.
#'
#' @return A logical list with components named
#' `.dat_null`, `.dat_df`, `.dat_list`, `.dat_names`,
#' `FUN_arg_names`,
#' `dots_present`,
#' `all_dot_num`, `all_dots_mats`,
#' `all_dots_list`, `all_dots_char`, `dots_names`, and
#' `arg_source`.
#'
#' @export
#'
#' @examples
#' identity_fun <- function(a, b) {list(a = a, b = b)}
#' matsindf_apply_types(.dat = NULL, FUN = identity_fun, a = 1, b = 2)
#' matsindf_apply_types(.dat = data.frame(), FUN = identity_fun,
#'                      a = matrix(c(1, 2)), b = matrix(c(2, 3)))
#' matsindf_apply_types(.dat = list(), FUN = identity_fun,
#'                      a = c(1, 2), b = c(3, 4), c = c(5, 6))
#' matsindf_apply_types(.dat = NULL, FUN = identity_fun,
#'                      a = list(1, 2), b = list(3, 4), c = list(5, 6))
#' matsindf_apply_types(.dat = NULL, FUN = identity_fun,
#'                      a = "a", b = "b", c = "c")
matsindf_apply_types <- function(.dat, FUN, ...) {
  # Check .dat
  if (is.null(.dat)) {
    .dat_null <- TRUE
    .dat_df <- FALSE
    .dat_list <- FALSE
  } else {
    .dat_null <- FALSE
    if (is.data.frame(.dat)) {
      .dat_df <- TRUE
      .dat_list <- TRUE
    } else if (is.list(.dat)) {
      .dat_df <- FALSE
      .dat_list <- TRUE
    } else {
      .dat_df <- FALSE
      .dat_list <- FALSE
    }
  }
  .dat_names <- names(.dat)

  # Check FUN
  FUN_arg_default_values <- formals(FUN) |> as.list()
  FUN_arg_names <- names(FUN_arg_default_values)
  # Tell which arguments have default values for FUN.
  # It appears that when a default is missing, the class of the argument is name.
  # So this test figures out which ones are present or not.
  defaults_present <- !sapply(FUN_arg_default_values, FUN = is.name)

  # Check ...
  dots <- list(...)
  dots_present <- length(dots) > 0
  if (!dots_present) {
    all_dots_num  <- FALSE
    all_dots_mats <- FALSE
    all_dots_list <- FALSE
    all_dots_vect <- FALSE
    all_dots_char <- FALSE
  } else {
    # arguments are present in the ... argument.
    dots_except_NULL <- dots[which(!as.logical(lapply(dots, is.null)))]
    all_dots_num  <- all(lapply(dots_except_NULL, FUN = is.numeric) %>% as.logical())
    all_dots_mats <- all(lapply(dots_except_NULL, FUN = matsbyname::is_matrix_or_Matrix) %>% as.logical())
    all_dots_list <- all(lapply(dots_except_NULL, FUN = is.list) %>% as.logical())
    all_dots_vect <- all(lapply(dots_except_NULL, FUN = function(x){
      (!"matrix" %in% class(x)) & (length(x) > 1)
    }) %>% as.logical())
    all_dots_char <- all(lapply(dots_except_NULL, FUN = is.character) %>% as.logical())
    if (all_dots_mats) {
      # Matrices are numerics.
      # However, when all items in ... are matrices, we want to operate as matrices, not as numbers.
      # So, set all_dots_num to FALSE.
      all_dots_num <- FALSE
    }
  }
  dots_names <- names(dots)

  # Figure out where to pull needed parameters from, dots, .dat, or defaults.
  # Precedence is given to ... over .dat over defaults when there are conflicts.
  args_present <- list(dots = FUN_arg_names %in% dots_names |>
                         magrittr::set_names(FUN_arg_names),
                       .dat = FUN_arg_names %in% .dat_names |>
                         magrittr::set_names(FUN_arg_names),
                       defaults = defaults_present)
  # If an argument is present in ..., we use it from ... .
  use_from_dots <- args_present$dots
  # If an argument is NOT present in ... but is present in .dat, use it from .dat.
  use_from_dat <- mapply(args_present$dots, args_present$.dat,
                         FUN = function(apdots, apdat) {
                           # We use parameters from .dat only
                           # when the corresponding parameter is NOT in dots.
                           !apdots & apdat
                         })
  # If an argument is NOT present in ... and NOT present in .dat, we use it from defaults, if possible.
  use_from_defaults <- mapply(args_present$dots, use_from_dat, args_present$defaults,
                              FUN = function(apdots, ufdat, apdef) {
                                !apdots & !ufdat & apdef
                              })
  arg_source <- list(dots = use_from_dots, .dat = use_from_dat, defaults = use_from_defaults)
  # Double check that each parameter has only one and only one source.
  args_ok <- mapply(arg_source$dots, arg_source$.dat, arg_source$defaults,
                    FUN = function(asdots, asdat, asdef) {
                      if (!asdat & !asdots) {
                        # We don't have the argument in .dat or ... .
                        # The argument needs to come from defaults.
                        # So return TRUE iff the arg is available from defaults
                        return(asdef)
                      }
                      if (asdat | asdots) {
                        # If the argument is available from .dat or ... .
                        # We should obtain the arg from defaults.
                        # So this arg is OK when asdef is FALSE.
                        # Therefore, return the not of asdef.
                        return(!asdef)
                      }
                    })
  if (!all(args_ok)) {
    # Give a helpful error message
    msg <- paste("In matsindf::matsindf_apply(), the following named arguments to FUN were found neither in .dat, nor in ..., nor in defaults:",
                 paste(names(args_ok[!args_ok]), collapse = ", "))
    stop(msg)
  }


  list(.dat_null = .dat_null,
       .dat_df = .dat_df,
       .dat_list = .dat_list,
       .dat_names = .dat_names,
       FUN_arg_names = FUN_arg_names,
       dots_present = dots_present,
       all_dots_num = all_dots_num,
       all_dots_mats = all_dots_mats,
       all_dots_list = all_dots_list,
       all_dots_vect = all_dots_vect,
       all_dots_char = all_dots_char,
       dots_names = dots_names,
       arg_source = arg_source)
}



