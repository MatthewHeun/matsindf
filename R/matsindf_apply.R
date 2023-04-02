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

  # New approach!

  DF <- build_matsindf_apply_data_frame(.dat = .dat, FUN = FUN, ... = ...)
  DF_only_needed_args <- DF |>
    dplyr::select(types$FUN_arg_all_names)

  # At this point, we have a data frame in .dat only.
  # Send one row at a time to FUN
  # new_data <- DF_only_needed_args |>
  new_data <- DF_only_needed_args |>
    as.list() |>
    purrr::list_transpose(simplify = FALSE) |>
    # Each row is now a column
    lapply(FUN = function(this_row) {
      # Call FUN on one row at a time.
      do.call(what = FUN, args = this_row)
    }) |>
    # Re-transpose to get back to original orientation.
    purrr::transpose()

  if (!types$.dat_df & !types$all_dots_longer_than_1) {
    new_data <- unlist(new_data, recursive = FALSE)
  }

  if (types$.dat_null) {
    # Return only the new variables as a list
    return(new_data)
  }

  common_names <- intersect(names(.dat), names(new_data))
  if (length(common_names) > 0) {
    msg <- paste("Name collision in matsindf::matsindf_apply().",
                 "The following arguments appear both in .dat and in the output of `FUN`:",
                 paste(common_names, collapse = ", "))
    warning(msg)
  }

  # Make a list of the return information
  if (types$all_dots_char) {
    out <- c(.dat, new_data)
  } else {
    out <- c(DF, new_data)
  }

  out <- out |>
    purrr::modify_if(.p = matsindf:::should_unlist, .f = unlist, recursive = FALSE)

  if (types$.dat_list & !types$.dat_df) {
    # Return as a list.
    return(out)
  }

  # We want a data frame with all of the incoming data included.
  # Convert to a tibble, which is much better at handling list columns.
  tibble::as_tibble(out)
}


#' Determine types of `.dat` and `...` arguments for matsindf_apply()
#'
#' This is a convenience function that returns a list
#' for the types of `.dat` and `...` as well as names in `.dat` and `...`,
#' with components named `.dat_null`, `.dat_df`, `.dat_list`, `.dat_names`,
#' `FUN_arg_all_names`, `FUN_arg_default_names`, `FUN_arg_default_values`,
#'  `dots_present`, `all_dots_num`, `all_dots_mats`,
#' `all_dots_list`, `all_dots_vect`, `all_dots_char`,
#' `all_dots_longer_than_1`, `dots_names`, and
#' `keep_args`.
#'
#' When `.dat` is a `data.frame`, both `.dat_list` and `.dat_df` are `TRUE`.
#'
#' When arguments are present in `...`, `dots_present` is `TRUE` but `FALSE` otherwise.
#' When all items in `...` are single numbers, `all_dots_num` is `TRUE` and all other list members are `FALSE`.
#' When all items in `...` are matrices, `all_dots_mats` is `TRUE` and all other list members are `FALSE`.
#' When all items in `...` are lists, `all_dots_list` is `TRUE` and all other list members are `FALSE`.
#' When all items in `...` are vectors (including lists), `all_dots_vect` is `TRUE`.
#' When all items in `...` have length > 1, `all_dots_longer_than_1` is `TRUE`.
#' When all items in `...` are character strings, `all_dots_char` is `TRUE` and all other list members are `FALSE`.
#'
#' The various `FUN_arg_*` components give information about the arguments to `FUN`.
#' `FUN_arg_all_names` gives the names of all arguments to `FUN`,
#' regardless of whether they have default values.
#' `FUN_arg_default_names` gives the names of only those arguments with default values.
#' `FUN_arg_default_values` gives the values of the default arguments,
#' already `eval()`ed in the global environment.
#' When there are no values in a category, `NULL` is returned.
#' thus, if `FUN` has no arguments with default values assigned in the signature of the function,
#' both `FUN_arg_default_names` and `FUN_arg_default_values` will be `NULL`.
#' If `FUN` has no arguments, all of
#' `FUN_arg_all_names`, `FUN_arg_default_names` and `FUN_arg_default_values` will be `NULL`.
#'
#' `keep_args` is a named `list()` of arguments,
#' which indicates which arguments to keep from which source
#' (`...`, `.dat`, or default args to `FUN`)
#' by order of preference,
#' `...` over `.dat` over default arguments to `FUN`.
#' Arguments not used by `FUN` are kept,
#' again according to the rules of preference.
#'
#' @param .dat The `.dat` argument to be checked.
#' @param FUN The function sent to `matsindf_apply()`.
#' @param ... The list of arguments to `matsindf_apply()` to be checked.
#'
#' @return A logical list with components named
#' `.dat_null`, `.dat_df`, `.dat_list`, `.dat_names`,
#' `FUN_arg_all_names`, `FUN_arg_default_names`, `FUN_arg_default_values`,
#'  `dots_present`, `all_dots_num`, `all_dots_mats`,
#' `all_dots_list`, `all_dots_vect`, `all_dots_char`,
#' `all_dots_longer_than_1`, `dots_names`, and
#' `keep_args`.
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
matsindf_apply_types <- function(.dat = NULL, FUN, ...) {

  ############################
  # Check .dat, FUN, and ... #
  ############################

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
  # Names of all arguments
  FUN_arg_all_names <- names(formals(FUN))
  # Names of only those argument with default values
  FUN_arg_default_names <- get_useable_default_args(FUN, which = "names")
  # The default values for the default arguments
  FUN_arg_default_values <- get_useable_default_args(FUN, which = "values")

  # Check ...
  dots <- list(...)
  dots_present <- length(dots) > 0

  #################################################
  # Set some logical values for the outgoing list #
  #################################################

  if (!dots_present) {
    all_dots_num  <- FALSE
    all_dots_mats <- FALSE
    all_dots_list <- FALSE
    all_dots_vect <- FALSE
    all_dots_char <- FALSE
    all_dots_longer_than_1 <- FALSE
  } else {
    # arguments are present in the ... argument.
    dots_except_NULL <- dots[which(!as.logical(lapply(dots, is.null)))]
    all_dots_num  <- all(lapply(dots_except_NULL, FUN = is.numeric) %>% as.logical())
    all_dots_mats <- all(lapply(dots_except_NULL, FUN = matsbyname::is_matrix_or_Matrix) %>% as.logical())
    all_dots_list <- all(lapply(dots_except_NULL, FUN = is.list) %>% as.logical())
    all_dots_vect <- all(lapply(dots_except_NULL, FUN = is.vector) |> as.logical())
    all_dots_char <- all(lapply(dots_except_NULL, FUN = is.character) %>% as.logical())
    if (all_dots_mats) {
      # Matrices are numerics.
      # However, when all items in ... are matrices, we want to operate as matrices, not as numbers.
      # So, set all_dots_num to FALSE.
      all_dots_num <- FALSE
    }
    all_dots_longer_than_1 <- all(lapply(dots_except_NULL, FUN = function(x) {
      (!matsbyname::is_matrix_or_Matrix(x)) & (length(x) > 1)
    }) |> as.logical())
  }
  dots_names <- names(dots)

  # Document which arguments are present BEFORE renaming occurs.
  args_present <- list(dots = dots_names,
                       .dat = .dat_names,
                       fun_defaults = FUN_arg_default_names)

  # Figure out where to pull needed parameters from: ..., .dat, or defaults.
  # Precedence is given to ... over .dat over defaults when there are conflicts.

  ################################
  # First, work on args in ... . #
  ################################

  # If an argument is present in ..., we use it from ... .
  keep_args_dots <- args_present$dots
  # Check if any of the arguments in ... have values that are single strings.
  # If so, we actually want to keep arguments in .dat whose name is the value of a ... argument.
  dots_args_to_pull_from_.dat <- sapply(keep_args_dots,
                                        FUN = function(this_arg) {
                                          if (!(is.character(dots[[this_arg]]) & length(dots[[this_arg]]) == 1)) {
                                            return(NULL)
                                          }
                                          if (dots[[this_arg]] %in% args_present$.dat) {
                                            return(dots[[this_arg]])
                                          }
                                          return(NULL)
                                        })
  if (all(sapply(dots_args_to_pull_from_.dat, FUN = is.null))) {
    # If the whole array is empty, set to NULL.
    dots_args_to_pull_from_.dat <- NULL
  } else {
    # Keep only non-NULL elements
    dots_args_to_pull_from_.dat <- dots_args_to_pull_from_.dat[!sapply(dots_args_to_pull_from_.dat, is.null)]
  }
  # But don't keep arguments in dots that we'll pull from .dat.
  keep_args_dots <- setdiff(keep_args_dots, names(dots_args_to_pull_from_.dat))

  ################################
  # Second, work on args in .dat #
  ################################

  # Keep all args in .dat, unless they also exist in ... and are not single strings,
  # i.e., keep all args in .dat, unless they are in keep_args_dots,
  # The logic for these decisions is found in .dat_names_to_keep()
  keep_args_.dat <- .dat_names_to_keep(.dat = .dat, FUN = FUN, ...)
  # The call to .dat_names_to_keep() does not preserve names of the returned vector.
  # Now put names on dots_args_to_pull_from_.dat.
  # The names indicate which argument to FUN each column of .dat will supply.
  new_names_keep_args_.dat <- character(length(keep_args_.dat))
  if (length(new_names_keep_args_.dat) > 0) {
    # We have some new names to figure out.
    for (i in 1:length(new_names_keep_args_.dat)) {
      if (keep_args_.dat[i] %in% dots_args_to_pull_from_.dat) {
        # Needs a name.
        # Find the corresponding item in dots_args_to_pull_from_.dat, if it exists.
        this_arg_has_a_name <- which(dots_args_to_pull_from_.dat == keep_args_.dat[i])
        new_names_keep_args_.dat[i] <- names(dots_args_to_pull_from_.dat[this_arg_has_a_name])
      } else {
        # Nothing special here. Just name the argument by its value.
        new_names_keep_args_.dat[i] <- keep_args_.dat[i]
      }
    }
  } else {
    # We don't have any renaming to do.
    new_names_keep_args_.dat <- NULL
  }
  # Set the names on keep_args_.dat.
  # The names on keep_args_.dat are the names to which we will rename these columns
  # when we do the calculations.
  keep_args_.dat <- keep_args_.dat |>
    magrittr::set_names(new_names_keep_args_.dat)

  ######################################
  # Third, work on default args to FUN #
  ######################################

  # Keep all args in defaults, unless they also exist in ... or .dat.
  keep_args_fun_defaults <- setdiff(args_present$fun_defaults, union(keep_args_dots, keep_args_.dat)) |>
    # or in the names of keep_args_.dat.
    setdiff(names(keep_args_.dat))
  # We may get to this point and keep_args_.dat is character(),
  # a character vector of length 0.
  # Under those conditions, set to NULL.
  if (is.character(keep_args_.dat) & length(keep_args_.dat) == 0) {
    keep_args_.dat <- NULL
  }

  ############################################
  # Bundle all keep_args together in a list. #
  ############################################

  keep_args <- list(dots = keep_args_dots, .dat = keep_args_.dat, fun_defaults = keep_args_fun_defaults)
  # Double check that each named argument has only one and only one source.
  args_ok <- !any(duplicated(unlist(keep_args)))
  if (!args_ok) {
    # Give a helpful error message
    repeat_values <- keep_args[duplicated(keep_args)] |>
      unique()
    msg <- paste("In matsindf::matsindf_apply(), the following named arguments to FUN were not removed from ..., or defaults:",
                 paste(repeat_values, collapse = ", "))
    stop(msg)
  }

  ############################################
  # Check that required args are present and #
  # that no extra args are specified in ...  #
  ############################################

  # The arguments that we have available are from keep_args$dots,
  # the names of keep_args$.dat (because the columns of .dat will be later renamed to the names of keep_args$.dat), and
  # keep_args$fun_defaults.
  args_available <- c(keep_args$dots, names(keep_args$.dat), keep_args$fun_defaults)
  # Look in two directions.
  # (1) Are all needed args to FUN available?
  # Double-check that all arguments needed for FUN are available.
  all_required_args_present <- all(FUN_arg_all_names %in% args_available)
  if (!all_required_args_present) {
    missing_args <- FUN_arg_all_names[!(FUN_arg_all_names %in% args_available)]
    msg <- paste("In matsindf::matsindf_apply(), the following named arguments to FUN were found neither in .dat, nor in ..., nor in defaults to FUN:",
                 paste(missing_args, collapse = ", "))
    stop(msg)
  }
  # (2) Do we have any extra args?
  # Extra args would come from unneeded args in ... .
  extra_dots_args_present <- any(!(keep_args$dots %in% FUN_arg_all_names))
  if (extra_dots_args_present) {
    extra_dots_args <- keep_args$dots[!(keep_args$dots %in% FUN_arg_all_names)]
    msg <- paste("In matsindf::matsindf_apply(), the following unused arguments appeared in ...:",
                 paste(extra_dots_args, collapse = ", "))
    stop(msg)
  }

  # Return everything.
  list(.dat_null = .dat_null,
       .dat_df = .dat_df,
       .dat_list = .dat_list,
       .dat_names = .dat_names,
       FUN_arg_all_names = FUN_arg_all_names,
       FUN_arg_default_names = FUN_arg_default_names,
       FUN_arg_default_values = FUN_arg_default_values,
       dots_present = dots_present,
       all_dots_num = all_dots_num,
       all_dots_mats = all_dots_mats,
       all_dots_list = all_dots_list,
       all_dots_vect = all_dots_vect,
       all_dots_char = all_dots_char,
       all_dots_longer_than_1 = all_dots_longer_than_1,
       dots_names = dots_names,
       keep_args = keep_args)
}


#' Create a data frame consisting of the input data for matsindf_apply()
#'
#' This is an internal helper function that takes the types list
#' and creates a data frame from which calculations
#' can proceed.
#'
#' This function enforces the precedence rules for `matsindf_apply()`, namely that
#' variables found in `...` take priority over
#' variables found in `.dat`, which take priority over
#' variables found in the default values of `FUN`.
#'
#' @param types The output of `matsindf_apply_types()`.
#' @param dots The contents of the `...` argument to `matsindf_apply()`, as a list or a data frame.
#' @param .dat The value of the `.dat` argument to `matsindf_apply()`, as a list or a data frame.
#' @param defaults The values of the default arguments for the `FUN` argument to `matsindf_apply()`.
#'
#' @return A data frame (actually, a `tibble`)
#'         with columns from `dots`, `.dat`, and the default values to `FUN`,
#'         according to precedence rules for `matsindf_apply()`.
#'
#' @export
build_matsindf_apply_data_frame <- function(.dat, FUN, ...) {

  types <- matsindf_apply_types(.dat = .dat, FUN = FUN, ... = ...)

  dots_df <- list(...) |>
    # If we have single matrices in the list,
    # they should we wrapped in list() to prevent
    # expanding to more columns that we want.
    purrr::modify_if(.p = matsbyname::is_matrix_or_Matrix, .f = function(m) {list(m)}) |>
    # Make a tibble out of the ... arguments
    tibble::as_tibble() |>
    # And select only those columns that we want to keep
    dplyr::select(dplyr::all_of(types$keep_args$dots))

  # Make a tibble out of the .dat argument (list or data frame)
  .dat_df <- .dat |>
    # Wrap in a list if the item itself is a matrix.
    purrr::modify_if(.p = is.matrix, .f = function(this_matrix) {list(this_matrix)}) |>
    tibble::as_tibble() |>
    # Keep only the arguments we want.
    dplyr::select(dplyr::all_of(types$keep_args$.dat)) |>
    # And set to their new names
    magrittr::set_names(names(types$keep_args$.dat))

  # Make a tibble out of the default arguments
  defaults_df <- types$FUN_arg_default_values |>
    purrr::compact() |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(types$keep_args$fun_defaults))

  df_list <- list(dots_df, .dat_df, defaults_df)
  df_list_length <- length(df_list)

  # Cycle through all data frames, looking for the first one.
  for (i in 1:df_list_length) {
    if (nrow(df_list[[i]]) > 0) {
      out <- df_list[[i]]
      next_i <- i+1
      break
    }
  }
  # Start after the one we just found and
  # cbind the other tibbles.
  for (i in next_i:df_list_length) {
    if (nrow(df_list[[i]]) > 0) {
      out <- dplyr::bind_cols(out, df_list[[i]])
    }
  }
  # Now return the full data frame.
  out
}


#' Create a usable list of default arguments to a function
#'
#' `formals(FUN)` does not handle arguments without a default well,
#' returning a `name` vector of length `1`,
#' which when converted to character is "".
#' This function detects that condition and replaces the no-default argument with
#' the value of `.no_default`, by default `NULL`.
#'
#' @param FUN A function from which values of default arguments are to be extracted.
#' @param which Tells whether to get "names" of arguments or "values" of arguments.
#'              Default is "values".
#' @param no_default The placeholder value for arguments with no default.
#'
#' @return A named list of default arguments to `FUN`.
#'         Names are the argument names.
#'         Values are the default argument values.
#'
#' @examples
#' f <- function(a = 42, b) {
#'   return(a + b)
#' }
#' matsindf:::get_useable_default_args(f)
#' matsindf:::get_useable_default_args(f, no_default = logical())
get_useable_default_args <- function(FUN, which = c("values", "names"), no_default = NULL) {
  which <- match.arg(which)
  out <- formals(FUN)
  delete_these <- sapply(out, FUN = function(x) {
    all(is.name(x)) & all(as.character(x) == "")
  })
  out <- out[!delete_these]
  if (which == "values") {
     out <- lapply(out, FUN = eval)
  } else if (which == "names") {
    out <- names(out)
  }
  if (length(out) == 0) {
    out <- NULL
  }
  out
}


#' Tell whether a column can be unlisted
#'
#' When evaluating each row of a data frame in `matsindf_apply()`,
#' the result will be a `tibble` with list columns.
#' This function tells whether a column can be unlisted.
#'
#' @param this_col The column to be checked.
#'                 Or a `data.frame`, in which case every column is checked.
#'
#' @return A boolean. `TRUE` if the column can be unlisted, `FALSE` otherwise.
#'         When `this_col` is a `data.frame`, a named boolean vector,
#'         one entry for each column.
#'
#' @examples
#' DF <- tibble::tibble(a = list(1, 2, 3), b = c("a", "b", "c"),
#'                      c = list(matrix(c(42, 43)),
#'                               matrix(c(44, 45)),
#'                               matrix(c(46, 47))))
#' matsindf:::should_unlist(DF$a)
#' matsindf:::should_unlist(DF$b)
#' matsindf:::should_unlist(DF$c)
#' sapply(DF, FUN = function(this_col) {matsindf:::should_unlist(this_col)})
#' matsindf:::should_unlist(DF)
should_unlist <- function(this_col) {
  if (is.data.frame(this_col)) {
    return(sapply(this_col, FUN = matsindf:::should_unlist))
  }
  if (!is.list(this_col)) {
    return(FALSE)
  }
  is_mat <- sapply(this_col, matsbyname::is_matrix_or_Matrix)
  if (!all(is_mat)) {
    return(TRUE)
  }
  return(FALSE)
}


#' Tell which `.dat` arguments to keep
#'
#' The logic for deciding which arguments of `.dat` are kept
#' is somewhat complex.
#' This function abstracts pulls all of that code in one place.
#'
#' For the name of each argument in `.dat`, the following table
#' decides whether that argument should be kept:
#'
#' \tabular{cccl}{
#' in `names(...)`   \tab  in string values of `...`   \tab decision  \tab reason \cr
#' no                \tab  yes or no                   \tab keep      \tab will not be replaced  \cr
#' yes or no         \tab  yes                         \tab keep      \tab will be renamed       \cr
#' yes               \tab  no                          \tab delete    \tab will be replaced      \cr
#' }
#'
#' @param .dat The `.dat` argument to `matsindf_apply()`.
#' @param dots The `...` argument to `matsindf_apply()`, as a list.
#'
#' @return A vector of names of `.dat` arguments to keep.
#'
#' @examples
#' example_fun <- function(a, b) {c(c = a + b, d = a - b)}
#' # Keeps all items in .dat, because there no items in ...
#' # that take precedence.
#' matsindf:::.dat_names_to_keep(.dat = list(a = 2, b = 1, z = 42),
#'                               FUN = example_fun)
#' # Keeps "b" and "z", because
#' # "a" is referenced to "z"
#' matsindf:::.dat_names_to_keep(.dat = list(a = 2, b = 1, z = 42),
#'                               FUN = example_fun,
#'                               a = "z")
.dat_names_to_keep <- function(.dat = NULL, FUN, ...) {
  dots <- list(...)
  dots_names <- names(dots)
  .dat_names <- names(.dat)

  if (length(dots) == 0) {
    # We have no ... arguments, so everything in .dat should be kept.
    return(.dat_names)
  }

  which_dots_are_single_character <- sapply(dots, FUN = function(this_dot) {
    if (is.character(this_dot)) {
      if (length(this_dot) == 1) {
        return(TRUE)
      }
    }
    return(FALSE)
  })

  single_character_dots <- dots[which_dots_are_single_character]

  # Apply the logic
  which_.dat_names_to_keep <- sapply(.dat_names, FUN = function(this_.dat_name) {
    if (!(this_.dat_name %in% dots_names)) {
      # If this .dat name is not in the names of ...,
      # this .dat argument will not be replaced by the argument in ...,
      # so keep it.
      return(TRUE)
    } else if (this_.dat_name %in% single_character_dots) {
      # If this .dat name is a single character ...,
      # it will be used in the call to FUN,
      # so keep it.
      return(TRUE)
    } else {
      # If we get here, this .dat name IS in the names of ...,
      # so it will be replaced,
      # so delete it.
      return(FALSE)
    }
  })

  # Return only those names that we want to keep.
  .dat_names[which_.dat_names_to_keep]
}
