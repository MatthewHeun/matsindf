
test_that("index_column works as expected", {
  DF1 <- tibble::tibble(Country = c("US", "US", "US"), Year = c(1980, 1981, 1982), var = c(10, 20, 40))
  expected1 <- DF1 %>%
    dplyr::mutate(
      var_indexed = c(1, 2, 4)
    )
  # Group on the indexing variable to obtain an error.
  expect_error(index_column(DF1 %>% dplyr::group_by(var), var_to_index = "var"),
               "Indexing variable 'var' in groups of .DF in index_column.")
  # Group on the time variable to obtain an error.
  expect_error(index_column(DF1 %>% dplyr::group_by(Year), var_to_index = "var"),
               "Time variable 'Year' in groups of .DF in index_column.")
  # Group on the correct variable to achieve success.
  expect_equal(index_column(DF1 %>% dplyr::group_by(Country), var_to_index = "var") %>% dplyr::ungroup(), expected1)

  # Test with 2 groups.
  DF2 <- DF1 %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Country = as.character(Country)
    ) %>%
    dplyr::bind_rows(data.frame(Country = c("GH", "GH", "GH"), Year = c(2011, 2012, 2013), var = c(1, 2, 4), stringsAsFactors = FALSE))
  expected2 <- DF2 %>%
    dplyr::mutate(
      var_indexed = c(1, 2, 4, 1, 2, 4)
    )
  # Index on the (default) first year in each group.
  expect_equal(index_column(DF2 %>% dplyr::group_by(Country), var_to_index = "var") %>% dplyr::ungroup(), expected2)
  # Index on a specified year
  DF2half <- DF2 %>%
    dplyr::mutate(
      Year = c(2011, 2012, 2013, 2011, 2012, 2013)
    )
  expected2half <- DF2half %>%
    dplyr::mutate(
      var_indexed = c(0.5, 1, 2, 0.5, 1, 2)
    )
  expect_equal(index_column(DF2half %>% dplyr::group_by(Country), var_to_index = "var", index_time = 2012) %>% dplyr::ungroup(),
               expected2half)

  # Test when the variable to be indexed is a column of a data frame containing matrices.
  # In this case, we expect an element-by-element division of the matrices to occur
  DF3 <- data.frame(
    Country  = rep("US", times = 12),
    Year     = rep(c(1980, 1981, 1982), each = 4),
    matname  = rep("m", times = 12),
    rowname  = rep(c("r1", "r2"), times = 6),
    colname  = rep(c("c1", "c2"), each = 2),
    matvals  = rep(c(10, 20, 40), each = 4),
    rowtypes = "row",
    coltypes = "col",
    stringsAsFactors = FALSE
  ) %>%
    dplyr::group_by(Country, Year, matname) %>%
    collapse_to_matrices(matnames = "matname",  matvals = "matvals",
                         rownames = "rowname",  colnames = "colname",
                         rowtypes = "rowtypes", coltypes = "coltypes")
  expected3 <- data.frame(
    Country = rep("US", times = 12),
    Year    = rep(c(1980, 1981, 1982), each = 4),
    matname = rep("m", times = 12),
    rowname = rep(c("r1", "r2"), times = 6),
    colname = rep(c("c1", "c2"), each = 2),
    matvals_indexed = rep(c(1, 2, 4), each = 4),
    rowtypes = "row",
    coltypes = "col",
    stringsAsFactors = FALSE
  ) %>%
    dplyr::group_by(Country, Year, matname) %>%
    collapse_to_matrices(matnames = "matname",  matvals = "matvals_indexed",
                         rownames = "rowname",  colnames = "colname",
                         rowtypes = "rowtypes", coltypes = "coltypes") %>%
    # Add the matvals column
    dplyr::mutate(
      matvals = DF3$matvals
    ) %>%
    # Put in the expected order
    dplyr::select(Country, Year, matname, matvals, matvals_indexed)

  expect_equal(index_column(DF3 %>% dplyr::group_by(Country, matname), var_to_index = "matvals") %>% as.data.frame(stringsAsFactors = FALSE), expected3)
})


test_that("rowcolval_to_mat() (collapse) works as expected", {
  # Establish some matrices that we expect to see.
  expected_mat <- matrix(c(11, 12,
                           0,  22),
                         nrow = 2, ncol = 2, byrow = TRUE,
                         dimnames = list(c("p1", "p2"), c("i1", "i2")))
  expected_mat_with_types <- expected_mat %>%
    matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries")

  # Create a data frame that can be converted to a matrix.
  rowcolval <- data.frame(Country  = c("GH", "GH", "GH"),
                          rows = c( "p1",  "p1", "p2"),
                          cols = c( "i1",  "i2", "i2"),
                          vals = c(  11  ,  12,   22 ),
                          stringsAsFactors = FALSE)
  A <- rowcolval_to_mat(rowcolval, rownames = "rows", colnames = "cols", matvals = "vals", rowtypes = NULL, coltypes = NULL)
  expect_true(inherits(A, "matrix"))
  expect_equal(A, expected_mat)
  expect_null(matsbyname::rowtype(A)) # rowtype has not been set
  expect_null(matsbyname::coltype(A)) # coltype has not been set

  # Provide single row and column types to be applied to all entries.
  B <- rowcolval_to_mat(rowcolval, rownames = "rows", colnames = "cols", matvals = "vals",
                        rowtypes  = "Products", coltypes  = "Industries")
  expect_equal(B, expected_mat_with_types)

  # Provide row and column types in the data frame and specify columns in the call to rowcolval_to_mat.
  C <- rowcolval %>%
    dplyr::bind_cols(data.frame(rt = c("Products", "Products", "Products"),
                                ct = c("Industries", "Industries", "Industries"),
                                stringsAsFactors = FALSE)) %>%
    rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
                     rowtypes = "rt", coltypes = "ct")
  expect_equal(C, expected_mat_with_types)

  # Also works for single values if both the rownames and colnames columns contain NA
  rowcolval2 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA),
                           rowtypes = c(NA), coltypes = c(NA), vals = c(2),
                           stringsAsFactors = FALSE)
  D <- rowcolval2 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
                                       rowtypes = "rowtype", coltypes = "coltype")
  expect_equal(D, 2)

  # Try without rowtype or coltype columns in the data frame.
  rowcolval3 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA), vals = c(2), stringsAsFactors = FALSE)
  E <- rowcolval3 %>% rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals")
  expect_equal(E, 2)

  # Fails when rowtype or coltype not all same. In rowcolval4, column rt is not all same.
  rowcolval4 <- rowcolval %>%
    dplyr::bind_cols(data.frame(rt = c("Products", "Industries", "Products"),
                                ct = c("Industries", "Industries", "Industries"),
                                stringsAsFactors = FALSE))
  expect_error(rowcolval_to_mat(rowcolval4,
                                rownames = "rows", colnames = "cols",
                                matvals = "vals",
                                rowtypes = "rt", coltypes = "ct"), "Not all values in rt \\(rowtype\\) were same as first entry: Products")
  rowcolval5 <- rowcolval %>%
    dplyr::bind_cols(data.frame(rt = c("Products", "Products", "Products"),
                                ct = c("Industries", "Products", "Industries"),
                                stringsAsFactors = FALSE))
  expect_error(rowcolval_to_mat(rowcolval5,
                                rownames = "rows", colnames = "cols",
                                matvals = "vals",
                                rowtypes = "rt", coltypes = "ct"), "Not all values in ct \\(coltype\\) were same as first entry: Industries")
})


test_that("rowcolval_to_mat() (collapse) works with Matrix objects", {
  # Establish some matrices that we expect to see.
  expected_mat <- matsbyname::Matrix(c(11, 12,
                                       0,  22),
                                     nrow = 2, ncol = 2, byrow = TRUE,
                                     dimnames = list(c("p1", "p2"), c("i1", "i2")))
  expected_mat_with_types <- expected_mat %>%
    matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries")

  # Create a data frame that can be converted to a matrix.
  rowcolval <- data.frame(Country  = c("GH", "GH", "GH"),
                          rows = c( "p1",  "p1", "p2"),
                          cols = c( "i1",  "i2", "i2"),
                          vals = c(  11  ,  12,   22 ),
                          stringsAsFactors = FALSE)
  A <- rowcolval_to_mat(rowcolval, rownames = "rows", colnames = "cols", matvals = "vals",
                        rowtypes = NULL, coltypes = NULL, matrix_class = "Matrix")
  expect_true(inherits(A, "Matrix"))
  expect_equal(A, expected_mat)
  expect_null(matsbyname::rowtype(A)) # rowtype has not been set
  expect_null(matsbyname::coltype(A)) # coltype has not been set

  # Provide single row and column types to be applied to all entries.
  B <- rowcolval_to_mat(rowcolval, rownames = "rows", colnames = "cols", matvals = "vals",
                        rowtypes  = "Products", coltypes  = "Industries",
                        matrix_class = "Matrix")
  expect_true(matsbyname::is.Matrix(B))
  expect_equal(B, expected_mat_with_types)

  # Provide row and column types in the data frame and specify columns in the call to rowcolval_to_mat.
  C <- rowcolval %>%
    dplyr::bind_cols(data.frame(rt = c("Products", "Products", "Products"),
                                ct = c("Industries", "Industries", "Industries"),
                                stringsAsFactors = FALSE)) %>%
    rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
                     rowtypes = "rt", coltypes = "ct",
                     matrix_class = "Matrix")
  expect_equal(C, expected_mat_with_types)

  # Also works for single values if both the rownames and colnames columns contain NA
  rowcolval2 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA),
                           rowtypes = c(NA), coltypes = c(NA), vals = c(2),
                           stringsAsFactors = FALSE)
  D <- rowcolval2 %>%
    rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
                     rowtypes = "rowtype", coltypes = "coltype",
                     matrix_class = "Matrix")
  expect_equal(D, 2)

  # Try without rowtype or coltype columns in the data frame.
  rowcolval3 <- data.frame(Country = c("GH"), rows = c(NA), cols = c(NA), vals = c(2), stringsAsFactors = FALSE)
  E <- rowcolval3 %>%
    rowcolval_to_mat(rownames = "rows", colnames = "cols", matvals = "vals",
                     matrix_class = "Matrix")
  expect_equal(E, 2)

  # Fails when rowtype or coltype not all same. In rowcolval4, column rt is not all same.
  rowcolval4 <- rowcolval %>%
    dplyr::bind_cols(data.frame(rt = c("Products", "Industries", "Products"),
                                ct = c("Industries", "Industries", "Industries"),
                                stringsAsFactors = FALSE))
  expect_error(rowcolval_to_mat(rowcolval4,
                                rownames = "rows", colnames = "cols",
                                matvals = "vals",
                                rowtypes = "rt", coltypes = "ct",
                                matrix_class = "Matrix"),
               "Not all values in rt \\(rowtype\\) were same as first entry: Products")
  rowcolval5 <- rowcolval %>%
    dplyr::bind_cols(data.frame(rt = c("Products", "Products", "Products"),
                                ct = c("Industries", "Products", "Industries"),
                                stringsAsFactors = FALSE))
  expect_error(rowcolval_to_mat(rowcolval5,
                                rownames = "rows", colnames = "cols",
                                matvals = "vals",
                                rowtypes = "rt", coltypes = "ct",
                                matrix_class = "Matrix"),
               "Not all values in ct \\(coltype\\) were same as first entry: Industries")
})


test_that("rowcolval_to_mat() deprecation works as expected", {
  rowcolval <- data.frame(Country  = c("GH", "GH", "GH"),
                          rows = c( "p1",  "p1", "p2"),
                          cols = c( "i1",  "i2", "i2"),
                          vals = c(  11  ,  12,   22 ),
                          stringsAsFactors = FALSE)
  expect_warning(rowcolval_to_mat(rowcolval, rownames = "rows", colnames = "cols", matvals = "vals",
                                  rowtypes = NULL, coltypes = NULL,
                                  matrix.class = "matrix"))
})


test_that("mat_to_rowcolval() (expand) works with Matrix objects", {
  # This is the matrix we expect to obtain.
  expected_mat <- matsbyname::Matrix(c(11, 12,
                                       0,  22),
                                     nrow = 2, ncol = 2, byrow = TRUE,
                                     dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>%
    matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries")

  # This is the data frame that we'll use the construct the matrix
  data <- data.frame(rows = c( "p1",  "p1", "p2"),
                     cols = c( "i1",  "i2", "i2"),
                     vals = c(  11  ,  12,   22 ),
                     rt = c("Products", "Products", "Products"),
                     ct = c("Industries", "Industries", "Industries"),
                     stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      rows = as.character(rows),
      cols = as.character(cols),
      rt = as.character(rt),
      ct = as.character(ct)
    ) %>%
    magrittr::set_rownames(NULL)

  # Construct the matrix that we'll convert later to a data frame.
  A <- data %>%
    rowcolval_to_mat(rownames = "rows", colnames = "cols",
                     rowtypes = "rt",   coltypes = "ct", matvals = "vals",
                     matrix_class = "Matrix")
  expect_true(matsbyname::is.Matrix(A))
  expect_equal(A, expected_mat)

  # Verify that if we feed garbage into the function, we obtain an error
  expect_error(mat_to_rowcolval("A",
                                rownames = "rows", colnames = "cols",
                                rowtypes = "rt", coltypes = "ct",
                                matvals = "vals",
                                drop = 0) %>% magrittr::set_rownames(NULL),
               "Unknown type of .matrix in mat_to_rowcolval A of class character and length 1")

  # Verify that we can convert the matrix to a data frame.
  expect_equal(mat_to_rowcolval(A,
                                rownames = "rows", colnames = "cols",
                                rowtypes = "rt", coltypes = "ct",
                                matvals = "vals",
                                drop = 0) %>%
                 magrittr::set_rownames(NULL),
               data)

  # Try when rowtype and coltype are not specified.
  A_trimmed <- A %>%
    matsbyname::setrowtype(NULL) %>% matsbyname::setcoltype(NULL)
  expect_equal(mat_to_rowcolval(A_trimmed,
                                rownames = "rows", colnames = "cols",
                                matvals = "vals",
                                drop = 0) %>%
                 magrittr::set_rownames(1:nrow(.)),
               data %>% dplyr::mutate(rt = NULL, ct = NULL))


  # Verify that drop works correctly.
  expect_equal(
    mat_to_rowcolval(A,
                     rownames = "rows", colnames = "cols",
                     rowtypes = "rt", coltypes = "ct",
                     matvals = "vals",
                     drop = 0) %>%
      rownames() %>% as.numeric(),
    # Rownames are 1, 3, 4, because row 2 (p2, i1) has an entry of 0.
    c(1, 3, 4))
})


test_that("add_UKEnergy2000_matnames() works as expected", {
  UKEnergy2000_withUVY <- UKEnergy2000 %>%
    matsindf:::add_UKEnergy2000_matnames()
  # We have saved a previous result for the add_UKEnergy2000_matnames function
  # with the following code:
  # UKEnergy2000_with_UVY <- UKEnergy2000 %>% add_matnames()
  # saveRDS(UKEnergy2000_with_UVY, file = "tests/UKEnergy2000_with_UVY.rds")
  # Load it for comparison.
  expected_with_UVY <- readRDS("UKEnergy2000_with_UVY.rds")
  expect_equal(UKEnergy2000_withUVY, expected_with_UVY)
})


test_that("add_UKEnergy2000_row_col_meta() works as expected", {
  UKEnergy2000_with_metadata <- UKEnergy2000 %>%
    matsindf:::add_UKEnergy2000_matnames() %>%
    matsindf:::add_UKEnergy2000_row_col_meta()
  # We have saved a previous result for the add_row_col_meta function with the following code:
  # UKEnergy2000_with_metadata <- UKEnergy2000_with_UVY %>% add_row_col_meta()
  # saveRDS(UKEnergy2000_with_metadata, file = "tests/UKEnergy2000_with_metadata.rds")
  # Load it for comparison.
  expected_with_metadata <- readRDS("UKEnergy2000_with_metadata.rds")
  expect_equal(UKEnergy2000_with_metadata, expected_with_metadata)
})


test_that("verify_cols_missing() errors as expected", {
  DF <- data.frame(A = 1:4, B = 11:14, stringsAsFactors = FALSE)
  # Try with a non-vector for newcols (a data frame is not a vector)
  expect_null(verify_cols_missing(DF, newcols = DF))
  expect_error(verify_cols_missing(DF, "A"),
               "column\\(s\\) 'A' is \\(are\\) already column names in data frame 'DF'")
})


test_that("error messages about column names works as expected", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  newcols <- c("c", "d", "a", "b")
  expect_error(verify_cols_missing(df, newcols),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))

  expect_silent(verify_cols_missing(df, c("d")))

  # Try with a list in newcols
  expect_silent(verify_cols_missing(df, list("d", "e")))
  expect_error(verify_cols_missing(df, list("a", "c")),
               Hmisc::escapeRegex("column(s) 'a' is (are) already column names in data frame 'df'"))
})


test_that("verify_cols_missing() works when either strings or names are provided", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  # Try with strings
  newcols <- c("a", "b")
  expect_error(verify_cols_missing(df, newcols),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))
  # Try with names
  newcolnames <- lapply(newcols, as.name)
  expect_error(verify_cols_missing(df, newcolnames),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))
})


test_that("verify_cols_missing() works with a single value", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  expect_silent(verify_cols_missing(df, as.name("c")))
  expect_error(verify_cols_missing(df, as.name("a")),
               Hmisc::escapeRegex("column(s) 'a' is (are) already column names in data frame 'df'"))
})


test_that("everything_except() works as expected for symbols", {
  DF <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6), stringsAsFactors = FALSE)
  expect_equal(everything_except(DF, "a"), c(as.name("b"), as.name("c")))
  expect_equal(everything_except(DF, "a"), sapply(c("b", "c"), as.name, USE.NAMES = FALSE))
  # Ensure all columns of .DF are returned if ... is empty or NULL.
  expect_equal(DF %>% everything_except(), sapply(c("a", "b", "c"), as.name, USE.NAMES = FALSE))
  expect_equal(everything_except(DF, NULL), sapply(c("a", "b", "c"), as.name, USE.NAMES = FALSE))
  # Try an empty vector
  expect_equal(everything_except(DF, c()), sapply(c("a", "b", "c"), as.name, USE.NAMES = FALSE))
  # Try an empty list
  expect_equal(everything_except(DF, list()), sapply(c("a", "b", "c"), as.name, USE.NAMES = FALSE))
  # Ensure that it works with strings
  expect_equal(everything_except(DF, "a"), sapply(c("b", "c"), as.name, USE.NAMES = FALSE))
  expect_equal(everything_except(DF, "a", "b"), sapply("c", as.name, USE.NAMES = FALSE))
  expect_equal(everything_except(DF, "c"), sapply(c("a", "b"), as.name, USE.NAMES = FALSE))
  # Now try a vector of strings
  expect_equal(everything_except(DF, c("a", "c")), sapply("b", as.name, USE.NAMES = FALSE))
  expect_equal(everything_except(DF, c("a")), sapply(c("b", "c"), as.name, USE.NAMES = FALSE))
  # Try a list.  Should still work.
  expect_equal(everything_except(DF, list("a")), sapply(c("b", "c"), as.name, USE.NAMES = FALSE))
})


test_that("everything_except() works as expected for strings", {
  DF <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6), stringsAsFactors = FALSE)
  expect_equal(everything_except(DF, "a", .symbols = FALSE), c("b", "c"))
  # Ensure all columns of .DF are returned if ... is empty or NULL.
  expect_equal(DF %>% everything_except(.symbols = FALSE), c("a", "b", "c"))
  expect_equal(everything_except(DF, NULL, .symbols = FALSE), c("a", "b", "c"))
  # Try an empty vector
  expect_equal(everything_except(DF, c(), .symbols = FALSE), c("a", "b", "c"))
  # Try an empty list
  expect_equal(everything_except(DF, list(), .symbols = FALSE), c("a", "b", "c"))
  # Ensure that it works with strings
  expect_equal(everything_except(DF, "a", .symbols = FALSE), c("b", "c"))
  expect_equal(everything_except(DF, "a", "b", .symbols = FALSE), "c")
  expect_equal(everything_except(DF, "c", .symbols = FALSE), c("a", "b"))
  # Now try a vector of strings
  expect_equal(everything_except(DF, c("a", "c"), .symbols = FALSE), "b")
  expect_equal(everything_except(DF, c("a"), .symbols = FALSE), c("b", "c"))
  # Try a list.  Should still work.
  expect_equal(everything_except(DF, list("a"), .symbols = FALSE), c("b", "c"))
})


test_that("group_by_everything_except works as expected", {
  DF <- tibble::tibble(a = c(1, 2), b = c(3, 4), c = c(5, 6))
  # Ensure everything is in the grouping variables grouped if ... is empty or NULL.
  expect_equal(group_by_everything_except(DF) %>% dplyr::group_vars(), c("a", "b", "c"))
  expect_equal(group_by_everything_except(DF, NULL) %>% dplyr::group_vars(), c("a", "b", "c"))
  # Try an empty vector
  expect_equal(group_by_everything_except(DF, c()) %>% dplyr::group_vars(), c("a", "b", "c"))
  # Try an empty list
  expect_equal(group_by_everything_except(DF, list()) %>% dplyr::group_vars(), c("a", "b", "c"))
  # Ensure that it works with strings
  expect_equal(group_by_everything_except(DF, "c") %>% dplyr::group_vars(), c("a", "b"))
  # Now try a vector of strings
  expect_equal(group_by_everything_except(DF, c("a", "c")) %>% dplyr::group_vars(), "b")
  expect_equal(group_by_everything_except(DF, c("a")) %>% dplyr::group_vars(), c("b", "c"))
  # Try a list.  Should still work.
  expect_equal(group_by_everything_except(DF, list("a")) %>% dplyr::group_vars(), c("b", "c"))

  # Test that things go as expected when groups already exist.
  DF %>%
    dplyr::group_by(a) %>%
    group_by_everything_except("b") %>%  # Resets groups
    dplyr::group_vars() %>%
    expect_equal(c("a", "c"))
  DF %>%
    dplyr::group_by(a) %>%
    group_by_everything_except("a", .add = TRUE) %>%  # Adds to groups
    dplyr::group_vars() %>%
    expect_equal(c("a", "b", "c"))

  # Test that everything works when the excluded column is NOT in the data frame.
  expect_equal(group_by_everything_except(DF, c("a", "z")) %>% dplyr::group_vars(), c("b", "c"))
  expect_equal(group_by_everything_except(DF, c("x", "y", "z")) %>% dplyr::group_vars(), c("a", "b", "c"))

  # Test that it works when you supply a reference to a string.
  a_var <- "a"
  expect_equal(group_by_everything_except(DF, a_var) %>% dplyr::group_vars(), c("b", "c"))
})


test_that("df_to_msg() works as expected", {
  msg <- data.frame(a = c(1, 2, 3), b = c("a", "b", "c")) %>%
    df_to_msg()
  expect_equal(msg, "a, b\n====\n1, a\n2, b\n3, c")
})


test_that("matrix_cols() works as expected", {
  tidy <- tibble::tibble(matrix = c("V1", "V1", "V1", "V2", "V2"),
                         row = c("i1", "i1", "i2", "i1", "i2"),
                         col = c("p1", "p2", "p2", "p1", "p2"),
                         vals = c(1, 2, 3, 4, 5)) %>%
    dplyr::mutate(
      rowtypes = "Industries",
      coltypes  = "Products"
    ) %>%
    dplyr::group_by(matrix)
  matsdf <- tidy %>%
    collapse_to_matrices(matnames = "matrix", matvals = "vals",
                         rownames = "row", colnames = "col",
                         rowtypes = "rowtypes", coltypes = "coltypes") %>%
    dplyr::mutate(
      integer = 42,
      string = "hello world"
    )
  expect_equal(matrix_cols(matsdf), c(vals = 2))

  # Add a row without a matrix
  df <- tibble::tribble(~matrix, ~vals,
                        "None", list(42))
  matsdf2 <- dplyr::bind_rows(matsdf, df)
  expect_equal(matrix_cols(matsdf2, .drop_names = TRUE), integer())
  expect_equal(matrix_cols(matsdf2, .drop_names = TRUE, .any = TRUE), 2)
})


test_that("matrix_cols() works with Matrix objects", {
  tidy <- tibble::tibble(matrix = c("V1", "V1", "V1", "V2", "V2"),
                         row = c("i1", "i1", "i2", "i1", "i2"),
                         col = c("p1", "p2", "p2", "p1", "p2"),
                         vals = c(1, 2, 3, 4, 5)) %>%
    dplyr::mutate(
      rowtypes = "Industries",
      coltypes  = "Products"
    ) %>%
    dplyr::group_by(matrix)
  matsdf <- tidy %>%
    collapse_to_matrices(matnames = "matrix", matvals = "vals",
                         rownames = "row", colnames = "col",
                         rowtypes = "rowtypes", coltypes = "coltypes",
                         matrix_class = "Matrix") %>%
    dplyr::mutate(
      integer = 42,
      string = "hello world"
    )
  expect_equal(matrix_cols(matsdf), c(vals = 2))

  # Add a row without a matrix
  df <- tibble::tribble(~matrix, ~vals,
                        "None", list(42))
  matsdf2 <- dplyr::bind_rows(matsdf, df)
  expect_equal(matrix_cols(matsdf2, .drop_names = TRUE), integer())
  expect_equal(matrix_cols(matsdf2, .drop_names = TRUE, .any = TRUE), 2)
})



