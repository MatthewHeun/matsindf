#' Apply a function to a `matsindf` data frame (and more)
#'
#' Applies `FUN` to `.dat` or
#' performs the calculation specified by `FUN`
#' on numbers or matrices.
#' `FUN` must return a named list.
#' The values of the list returned `FUN` become
#' entries in columns in a returned data frame
#' or entries in the sub-lists of a returned list.
#' The names of the items in the list returned by `FUN` become
#' names of the columns in a returned data frame or
#' names of the list items in the returned list.
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
#' The return value is a list
#' whose top-level names are the names of the returned items from `FUN`
#' `.dat` is not included in the return value.
#'
#' If `!is.null(.dat)` and `...` are all named, `length == 1` character strings
#' of the form `argname = string`,
#' `argname`s are expected to be names of arguments to `FUN`, and
#' `string`s are expected to be column names in `.dat`.
#' The return value is `.dat` with additional columns (at right)
#' whose names are the names of list items returned from `FUN`.
#' When `.dat` contains columns whose names are same as columns added at the right,
#' a warning is emitted.
#'
#' `.dat` can be a list of named items in which case a list will be returned
#' instead of a data frame.
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
#' If `.dat` is present, `...` contains `length == 1` strings, and one of the `...` strings is not the name
#' of a column in `.dat`,
#' `FUN` is called WITHOUT the argument whose column is missing.
#' I.e., that argument is treated as missing.
#' If `FUN` works despite the missing argument, execution proceeds.
#' If `FUN` cannot handle the missing argument, an error will occur in `FUN`.
#'
#' It is suggested that `FUN` is able to handle empty data gracefully,
#' returning an empty result with the same names as when
#' non-empty data are fed to `FUN`.
#' Attempts are made to handle zero-row data (in `.dat` or `...`)
#' gracefully.
#' First, `FUN` is called with the empty (but named) data.
#' If `FUN` can handle empty data without error,
#' the result is returned.
#' If `FUN` errors when fed empty data, `FUN` is called with an empty
#' argument list in the hopes that `FUN` has reasonable default values.
#' If that fails,
#' `.dat` is returned unmodified (if not `NULL`)
#' or the data in `...` is returned.
#'
#' @param .dat A list of named items or a data frame.
#' @param FUN The function to be applied to `.dat`.
#' @param ... Named arguments to be passed by name to `FUN`.
#' @param .warn_missing_FUN_args A boolean that tells
#'        whether to warn of missing arguments to `FUN`.
#'        Default is `TRUE`.
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
#' # .dat is returned unmodified, unless FUN can handle empty data.
#' DF3 <- DF2[0, ]
#' DF3
#' matsindf_apply(DF3, FUN = example_fun, a = "a", b = "b")
matsindf_apply <- function(.dat = NULL, FUN, ..., .warn_missing_FUN_args = TRUE){
  types <- matsindf_apply_types(.dat, FUN, ...)

  if (!types$.dat_null) {
    if (!types$.dat_list) {
      # If we get here, we have a value for .dat that doesn't make sense.
      stop(".dat must be NULL, a data frame, or a list in matsindf_apply(), was ", class(.dat))
    }
  }

  DF <- build_matsindf_apply_data_frame(.dat = .dat, FUN = FUN, ... = ..., types = types)

  # Deal gracefully with zero-row DF.
  if (nrow(DF) == 0 | ncol(DF) == 0) {
    return(handle_empty_data(.dat = .dat, FUN = FUN, DF = DF, types = types))
  }

  DF_only_needed_args <- DF |>
    dplyr::select(dplyr::any_of(types$FUN_arg_all_names))

  # Send one row at a time to FUN
  new_data <- DF_only_needed_args |>
    as.list() |>
    purrr::list_transpose(simplify = FALSE) |>
    # Each row is now a column
    lapply(FUN = function(this_row) {
      # Call FUN on one row at a time.
      do.call(what = FUN, args = this_row)
    }) |>
    # Re-transpose to get back to original orientation.
    purrr::list_transpose(simplify = FALSE)

  if (!types$.dat_df & !types$all_dots_longer_than_1) {
    # .dat is not a data frame and
    # at least some ... arguments are length 1.
    if (all(sapply(new_data, should_unlist))) {
      # If we get here, the sub-items for each computed variable
      # are lists AND they all have length 1 (or are a single matrix)
      # In this situation, we can unlist the inner lists
      # to remove one layer of listing.
      # Doing so makes natural data frames or lists.
      new_data <- lapply(new_data, unlist, recursive = FALSE)
    } else {
      # We can unlist at the top level.
      new_data <- unlist(new_data, recursive = FALSE)
    }
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

  # If we don't have the right sizes, try to modify the out list to get the right number of items.
  out <- out |>
    purrr::modify_if(.p = should_unlist, .f = unlist, recursive = FALSE)

  if (types$.dat_list & !types$.dat_df) {
    # Return as a list.
    return(out)
  }

  # We want a data frame with all of the incoming data included.
  # Convert to a tibble, which is much better at handling list columns.
  tibble::as_tibble(out, .name_repair = "minimal")
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
#' @param .warn_missing_FUN_args A boolean that tells
#'        whether to warn of missing arguments to `FUN`.
#'        Default is `TRUE`.
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
#'                      a = c(1, 2), b = c(3, 4))
#' matsindf_apply_types(.dat = NULL, FUN = identity_fun,
#'                      a = list(1, 2), b = list(3, 4))
matsindf_apply_types <- function(.dat = NULL, FUN, ..., .warn_missing_FUN_args = TRUE) {

  # Check .dat, FUN, and ... ----------------------------------------------

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

  # Set some logical values for the outgoing list -----------------------------

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

  where_to_find_args <- where_to_get_args(.dat, FUN = FUN, ... = ...)
  keep_args <- where_to_find_args |>
    build_keep_args()
  # Double check that each named argument has only one and only one source.
  args_ok <- !any(duplicated(unlist(keep_args)))
  assertthat::assert_that(args_ok, msg = paste("In matsindf::matsindf_apply(), the following named arguments to FUN were not removed from ..., or defaults:",
                                               paste(keep_args[duplicated(keep_args)], collapse = ", ")))

  # Check that required args are present and that no extra args are specified in ...  --------------------------------

  # Look in two directions.
  # (1) Are all needed args to FUN available?
  # Double-check that all arguments needed for FUN are available.
  if (.warn_missing_FUN_args) {
    # The arguments that we have available are from keep_args$dots,
    # the names of keep_args$.dat (because the columns of .dat will be later renamed to the names of keep_args$.dat), and
    # keep_args$fun_defaults.
    args_available <- c(keep_args$dots, names(keep_args$.dat), keep_args$FUN)
    # Decide if all required arguments are present.
    all_required_args_present <- all(FUN_arg_all_names %in% args_available)
    if (!all_required_args_present) {
      missing_args <- FUN_arg_all_names[!(FUN_arg_all_names %in% args_available)]
      msg <- paste0("In matsindf::matsindf_apply(), the following named arguments to FUN were not found in any of .dat, ..., or defaults to FUN: ",
                    paste(missing_args, collapse = ", "),
                    ". Set .warn_missing_FUN_args = FALSE to suppress this warning if you know what you are doing.")
      warning(msg)
    }
  }
  # (2) Do we have any extra args?
  # Extra args would come from unneeded args in ... .
  extra_dots_args_present <- any(!(names(dots) %in% FUN_arg_all_names))
  if (extra_dots_args_present) {
    names_dots <- names(dots)
    extra_dots_args <- names_dots[!(names_dots %in% FUN_arg_all_names)]
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
#' @param .dat The value of the `.dat` argument to `matsindf_apply()`, as a list or a data frame.
#' @param FUN The function supplied to `matsindf_apply()`.
#' @param ... The `...` argument supplied to `matsindf_apply()`.
#' @param types The types for `matsindf_apply()`. Supply if already calculated externally.
#'              Default is `types = matsindf_apply_types(.dat, FUN = FUN, ... = ...)`.
#'
#' @return A data frame (actually, a `tibble`)
#'         with columns from `dots`, `.dat`, and the default values to `FUN`,
#'         according to precedence rules for `matsindf_apply()`.
#'
#' @export
build_matsindf_apply_data_frame <- function(.dat = NULL, FUN, ..., types = matsindf_apply_types(.dat, FUN = FUN, ... = ...)) {

  dots <- list(...)

  dots_df <- dots |>
    # Deal with any NULL args
    handle_null_args() |>
    # Make a tibble out of the ... arguments
    tibble::as_tibble() |>
    # And select only those columns that we want to keep
    dplyr::select(dplyr::all_of(types$keep_args$dots))

  .dat_df <- .dat |>
    # Deal with any NULL args
    handle_null_args() |>
    # Make a tibble out of the .dat arguments
    tibble::as_tibble() |>
    # Keep only the arguments we want.
    dplyr::select(dplyr::all_of(types$keep_args$.dat))

  if (!is.null(types$keep_args$.dat)) {
    .dat_df <- .dat_df |>
      magrittr::set_names(names(types$keep_args$.dat))
  }

  # Make a tibble out of the default arguments
  defaults_df <- types$FUN_arg_default_values |>
    # If a default argument value is NULL,
    # wrap it in a list to allow as_tibble() to work correctly.
    purrr::modify_if(.p = is.null, .f = function(null_col) {list(null_col)}) |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(types$keep_args$FUN))

  if (ncol(dots_df) == 0 & nrow(.dat_df) == 0 & nrow(defaults_df) == 0) {
    # The incoming data could have no rows in .dat_df and defautls_df
    # when no incoming data are available.
    # We could have 1 row and zero columns of dots_df,
    # due to single-name arguments.
    # If this is true, we want to cbind .dat_df and defaults_df together
    # and return.
    # Doing so preserves the column names in .dat_df and defaults_df.
    return(dplyr::bind_cols(.dat_df, defaults_df))
  }

  df_list <- list(dots_df, .dat_df, defaults_df)
  df_list_length <- length(df_list)

  next_i <- 0
  # Cycle through all data frames, looking for the first one.
  for (i in 1:df_list_length) {
    if (nrow(df_list[[i]]) > 0) {
      out <- df_list[[i]]
      next_i <- i+1
      break
    }
  }

  if (next_i > 0) {
    # Start after the one we just found and
    # cbind the other tibbles.
    for (i in next_i:df_list_length) {
      if (nrow(df_list[[i]]) > 0) {
        out <- dplyr::bind_cols(out, df_list[[i]])
      }
    }
  } else {
    # No data frames had any rows.
    return(dplyr::bind_cols(df_list))
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
    out <- no_default
  }
  out
}


#' Build a list of arguments to keep
#'
#' In the process of building data frames of arguments to `FUN`,
#' we need to decide which arguments to keep from each source,
#' `...`, `.dat`, and defaults to `FUN`.
#' This function does that work in one place.
#'
#' @param where_to_find_args A list created by `where_to_get_args()`.
#'
#' @return A list with names `.dat`, `dots`, and `FUN` which
#'         gives items to keep from each source.
build_keep_args <- function(where_to_find_args) {
  args_to_keep <- function(where_to_find_args, which_source) {
    where_to_find_args |>
      purrr::keep(.p = function(this_FUN_arg) {
        if (is.null(this_FUN_arg)) {
          return(FALSE)
        }
        this_FUN_arg[["source"]] == which_source
      }) |>
      purrr::list_transpose() |>
      purrr::pluck("arg_name")
  }

  keep_args_dots <- args_to_keep(where_to_find_args, which_source = "...")
  keep_args_.dat <- args_to_keep(where_to_find_args, which_source = ".dat")
  keep_args_FUN <- args_to_keep(where_to_find_args, which_source = "FUN")

  # Return everything in a list
  list(.dat = keep_args_.dat, FUN = keep_args_FUN, dots = keep_args_dots)
}


#' Tell whether a column can be unlisted
#'
#' When evaluating each row of a data frame in `matsindf_apply()`,
#' the result will be a `tibble` with list columns.
#' This function tells whether a column can be unlisted.
#' This is internal helper function and should not be called externally.
#'
#' @param this_col The column to be checked.
#'                 Or a `data.frame`, in which case every column is checked.
#'
#' @return A boolean. `TRUE` if the column can be unlisted, `FALSE` otherwise.
#'         When `this_col` is a `data.frame`, a named boolean vector,
#'         one entry for each column.
should_unlist <- function(this_col) {
  if (is.data.frame(this_col)) {
    return(sapply(this_col, should_unlist))
  }
  if (!is.list(this_col)) {
    return(FALSE)
  }
  # Check if everything in the list is a matrix.
  # If so, don't unlist it.
  is_mat <- sapply(this_col, matsbyname::is_matrix_or_Matrix)
  if (all(is_mat)) {
    return(FALSE)
  }
  # Check if everything in the list is a data frame.
  # If so, don't unlist it.
  # Assume the caller is creating a nested data frame.
  is_df <- sapply(this_col, is.data.frame)
  if (all(is_df)) {
    return(FALSE)
  }
  # Check if everything in the list is length 1.
  # If so, we can unlist.
  lengths <- sapply(this_col, length)
  if (all(lengths == lengths[1])) {
    # We could get here if all items are NULL, because
    # length(NULL) is 0.
    # When all items are NULL, we do not want to simplify,
    # because the column will become NULL itself,
    # probably against the wishes of the caller.
    if (all(sapply(this_col, is.null)) & length(this_col) > 1) {
      return(FALSE)
    } else if (all(lengths > 1)) {
      # Probably have special list objects (models, Sankey diagrams, etc.)
      # Don't want to unlist this.
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  return(FALSE)
}


#' Gracefully handle empty data
#'
#' When empty data are provided to `matsindf_apply()`,
#' care must be take with the return value.
#' This function assembles the correct zero-row data frame or
#' zero-length lists.
#'
#' @param .dat The `.dat` argument to `matsindf_apply()`.
#' @param FUN The `FUN` argument to `matsindf_apply()`.
#' @param DF The assembled `DF` inside `matsindf_apply()`.
#' @param types The `types` object assembled inside `matsindf_apply()`.
#'
#' @return The appropriate return value from `matsindf_apply()`,
#'         either a zero-length list or a zero-row data frame.
handle_empty_data <- function(.dat = NULL, FUN, DF, types) {
  out <- tryCatch({
      # Call FUN. Let it try to handle a zero-row data frame.
      res <- do.call(what = FUN, args = DF)
      if (types$.dat_df) {
        res <- dplyr::bind_cols(.dat, tibble::as_tibble(res)) |>
          tibble::as_tibble()
      } else if (types$.dat_list) {
        res <- c(.dat, res)
      }
      return(res)
    },
    error = function(e1) {
      tryCatch(
        # Calling with an empty data frame failed. Try to call FUN with an empty argument list.
        do.call(what = FUN, args = list()),
        # If that fails, just return .dat unmodified.
        error = function(e2) {
          if (!types$.dat_null) {
            return(.dat)
          }
          return(as.list(DF))
        }
      )
    }
  )
  return(out)
}


#' Gracefully handle `NULL` arguments
#'
#' When `NULL` is passed as an element of the `.dat` or `...` arguments
#' to `matsindf_apply()`, special care must be taken.
#' This function helps in those situations.
#'
#' @param .arg One of `.dat` or `...` (as a list) arguments to `matsindf_apply()`.
#'
#' @return A list representation of `.arg` with `NULL` values handled appropriately.
handle_null_args <- function(.arg) {
  # Get lengths of each item in .arg
  # But do so in a way that preserves any NULL elements.
  lengths <- sapply(.arg, function(this_.arg) {
    if (matsbyname::is_matrix_or_Matrix(this_.arg)) {
      return(1)
    } else if (is.null(this_.arg)) {
      return(NULL)
    } else {
      return(length(this_.arg))
    }
  })

  # See if all non-NULL items have the same length
  compact_lengths <- purrr::compact(lengths)
  if (length(compact_lengths) == 0) {
    all_same_length <- TRUE
  } else {
    all_same_length <- all(compact_lengths == compact_lengths[[1]])
    if (! all_same_length) {
      stop("Different lengths in handle_null_args()")
    }
    null_replacement <- vector("list", compact_lengths[[1]])
  }

  .arg |>
    # If we have single matrices in the list,
    # they should we wrapped in list() to prevent
    # expanding to more columns than we want.
    purrr::modify_if(.p = matsbyname::is_matrix_or_Matrix, .f = function(m) {list(m)}) |>
    purrr::modify_if(.p = is.null, .f = function(n) {null_replacement})
}


#' Decide where to get each argument to FUN
#'
#' The precedence rules for where to obtain values for the `FUN` argument to
#' `matsindf_apply()` are codified here.
#' The rules are:
#' * Precedence order: `...`, `.dat`, defaults arguments to `FUN`
#'   (highest priority to lowest priority).
#' * If an element of `...` is a character string of length `1`,
#'   the element of `...` provides a mapping between
#'   an item in `.dat` (with same name as the value of the character string of length `1`)
#'   to an argument of `FUN` (with the same name as the name of the character string of length `1`).
#' * If the value of the character string of length `1` is not a name in `.dat`,
#'   the default arguments to `FUN` are checked in this order.
#'     - If the name of a default argument to `FUN` is the same as the value of the
#'       string of length `1` argument in `...`, a mapping occurs.
#'     - If a mapping is not possible,
#'       the default arg to `FUN` is used directly.
#'
#' @param .dat The `.dat` argument to `matsindf_apply()`.
#' @param FUN The `FUN` argument to `matsindf_apply()`.
#' @param ... The `...` argument to `matsindf_apply()`.
#'
#' @return A named list wherein the names are the argument names to `FUN`.
#'         Values are character vectors with 2 elements.
#'         The first element is named `source` and provides
#'         the argument to `matsindf_apply()` from which the named argument should be found,
#'         one of ".dat", "FUN", or "...".
#'         The second element is named `arg_name` and provides
#'         the variable name or argument name in the source that contains the input data
#'         for the argument to `FUN`.
#'
#' @examples
#' example_fun <- function(a = 1, b) {
#'   list(c = a + b, d = a - b)
#' }
#' # b is not available anywhere, likely causing an error later
#' matsindf:::where_to_get_args(FUN = example_fun)
#' # b is now available in ...
#' matsindf:::where_to_get_args(FUN = example_fun, b = 2)
#' # b is now available in .dat
#' matsindf:::where_to_get_args(list(b = 2), FUN = example_fun)
#' # b now comes from ..., because ... takes precedence over .dat
#' matsindf:::where_to_get_args(list(b = 2), FUN = example_fun, b = 3)
#' # Mapping from c in .dat to b in FUN
#' matsindf:::where_to_get_args(list(c = 2),
#'                              FUN = example_fun, b = "c")
#' # Redirect from an arg in ... to a different default to FUN
#' matsindf:::where_to_get_args(FUN = example_fun, b = "a")
#' # b is found in FUN, not in .dat, because the mapping (b = "a")
#' # is not available in .dat
#' matsindf:::where_to_get_args(list(b = 2), FUN = example_fun, b = "a")
where_to_get_args <- function(.dat = NULL, FUN, ...) {
  .dat_arg_names <- names(.dat)
  # Names of all FUN arguments
  FUN_arg_names <- names(formals(FUN))
  FUN_arg_names <- FUN_arg_names |>
    magrittr::set_names(FUN_arg_names)
  # Names of FUN args that have default values
  FUN_arg_names_with_defaults <- get_useable_default_args(FUN, which = "names")
  dots <- list(...)
  dots_arg_names <- names(dots)
  dots_arg_char1 <- lapply(dots, function(this_dot) {
    if (is.null(this_dot)) {
      return(FALSE)
    } else {
      return(is.character(this_dot) & length(this_dot) == 1)
    }
  })

  # Cycle through each arg of FUN
  lapply(FUN_arg_names, function(this_FUN_arg_name) {
    if (this_FUN_arg_name %in% dots_arg_names) {
      # The argument to FUN is present in ...
      if (dots_arg_char1[[this_FUN_arg_name]]) {
        # We have a single character.
        # Look for this argument in .dat and defaults to FUN.
        # according to the precedence order provided in the documentation.
        dots_arg_val <- dots[[this_FUN_arg_name]]
        if (dots_arg_val %in% .dat_arg_names) {
          # The length-1 string value of this_FUN_arg_name is present
          # in .dat.
          return(c(source = ".dat", arg_name = dots_arg_val))
        } else if (dots_arg_val %in% FUN_arg_names_with_defaults) {
          # The length-1 string value of this_FUN_arg_name is NOT present
          # in .dat.
          # Check if the length-1 string value of this_FUN_arg_name exists
          # in the names of the default arguments to FUN.
          # If so, use that.
          return(c(source = "FUN", arg_name = dots_arg_val))
        } else if (this_FUN_arg_name %in% FUN_arg_names_with_defaults) {
          # The length-1 string value of this_FUN_arg_name is NOT present
          # in .dat.
          # Nor was the length-1 string value of this_FUN_arg_name
          # present in the names of default args to FUN.
          # So, instead, look for this_FUN_arg_name in the names of the
          # default arguments to FUN.
          return(c(source = "FUN", arg_name = this_FUN_arg_name))
        } else {
          # The length-1 string value of this_FUN_arg_name is NOT present
          # in .dat, nor is this_FUN_arg_name the name of a default
          # argument to FUN.
          # Simply return NULL.
          return(NULL)
        }
      } else {
        # We don't have a single character.
        # Use this arg from ... directly.
        return(c(source = "...", arg_name = this_FUN_arg_name))
      }
    } else if (this_FUN_arg_name %in% .dat_arg_names) {
      return(c(source = ".dat", arg_name = this_FUN_arg_name))
    } else if (this_FUN_arg_name %in% FUN_arg_names_with_defaults) {
      return(c(source = "FUN", arg_name = this_FUN_arg_name))
    }
    # Couldn't find this_FUN_arg anywhere.
    return(NULL)
  })
}
