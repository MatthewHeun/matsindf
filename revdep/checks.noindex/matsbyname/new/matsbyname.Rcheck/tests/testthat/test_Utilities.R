# This file contains tests for functions in Utilities.R.

test_that("errors are generated when organize_args is called with baloney", {
  expect_error(matsbyname:::organize_args(b = 42), 
               "Missing argument a with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = NULL, b = 42), 
               "Null argument a with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = 42), 
               "Missing argument b with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = 42, b = NULL), 
               "Null argument b with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = matrix(1), b = matrix(1), match_type = "bogus"), 
               "Unknown match_type bogus in organize_args.")
  expect_error(matsbyname:::organize_args(a = matrix(1) %>% setcoltype("col"), 
                                          b = matrix(1) %>% setrowtype("bogus"), 
                                          match_type = "matmult"))
})


test_that("oddball match_type works as expected", {
  expect_equal(matsbyname:::organize_args(a = 1, b = 2, match_type = "none"), 
               list(a = 1, b = 2))
  expect_error(matsbyname:::organize_args(a = matrix(1), b = matrix(2), match_type = "bogus"), 
               "Unknown match_type bogus in organize_args.")
  expect_equal(matsbyname:::organize_args(a = matrix(1), b = matrix(2), match_type = "none"),
               list(a = matrix(1), b = matrix(2)))
})


test_that("an error is generated when no retain or remove patterns are default", {
  # Check with non-NULL values for a.
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_error(m %>% select_rows_byname(), 
               "neither retain_pattern nor remove_pattern are different from default")
  expect_error(m %>% select_cols_byname(), 
               "neither retain_pattern nor remove_pattern are different from default")
})


test_that("selecting rows and columns works even when everything is removed", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try to remove all rows
  expect_null(select_rows_byname(m, remove_pattern = "^r"))
  # Try to remove all columns
  expect_null(select_cols_byname(m, remove_pattern = "^c"))
})


test_that("selecting rows and columns works even when there is a NULL situation", {
  # Check the degenerate condition.
  expect_null(select_rows_byname(a = NULL))
  expect_null(select_cols_byname(a = NULL))
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try with rows
  expect_equal(m %>% select_rows_byname(retain_pattern = "r1"), 
               matrix(c(1, 3), ncol = 2, dimnames = list("r1", c("c1", "c2"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_rows_byname(retain_pattern = "r3"))
  # Try with columns
  expect_equal(m %>% select_cols_byname(retain_pattern = "c1"), 
               matrix(1:2, nrow = 2, dimnames = list(c("r1", "r2"), "c1")) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_cols_byname(retain_pattern = "c3"))
})


test_that("setting row and column names works even when there is a NULL situation", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try with rows
  expect_equal(m %>% setrownames_byname(c("a", "b")), 
               matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("c1", "c2"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  expect_null(setrownames_byname(NULL, c("a", "b")))
  # Try with columns
  expect_equal(m %>% setcolnames_byname(c("a", "b")), 
               matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("a", "b"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  expect_null(setcolnames_byname(NULL, c("a", "b")))
})
  

test_that("bad margins in clean_byname work as expected", {
  m <- matrix(c(0, 0, 0, 1, 2, 3), nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_error(clean_byname(m, margin = 42), 
               "margin = 42 in clean_byname\\(\\). Must be 1 or 2.")
  
})


test_that("cleaning both rows and cols works as expected", {
  m <- matrix(c(0, 0, 0, 1, 2, 3), nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(clean_byname(m), 
               matrix(1:3, nrow = 3, ncol = 1, dimnames = list(c("r1", "r2", "r3"), "c2")))
})


test_that("cleaning a vector works as expected", {
  v <- matrix(c(0, 
                0, 
                0, 
                42), nrow = 4, dimnames = list(c("r1", "r2", "r3", "r4"), c("c1")))
  expect_equal(clean_byname(v), 
               matrix(42, dimnames = list(c("r4"), c("c1"))))
})


test_that("cleaning works with unnamed rows and/or columns", {
  v <- matrix(c(0, 
                0, 
                0, 
                42), nrow = 4, dimnames = list(c("r1", "r2", "r3", "r4")))
  expect_equal(clean_byname(v), 
               matrix(42, nrow = 1, byrow = TRUE, dimnames = list(c("r4"), c(NULL))))
  
  
  unnamed <- matrix(c(1, 2, 0,
                      3, 4, 0,
                      5, 6, 0,
                      0, 0, 0), nrow = 4, byrow = TRUE)
  expect_equal(clean_byname(unnamed), matrix(c(1, 2,
                                               3, 4, 
                                               5, 6), nrow = 3, byrow = TRUE))
})


test_that("cleaning works with tolerance", {
  unnamed <- matrix(c(1, 2, 0.1,
                      3, 4, -0.1,
                      5, 6, 0.05,
                      0.01, -0.01, -0.05), nrow = 4, byrow = TRUE)
  expect_equal(clean_byname(unnamed, tol = 0.1), matrix(c(1, 2,
                                                          3, 4, 
                                                          5, 6), nrow = 3, byrow = TRUE))
  # Tighten tolerance to get different result.
  expect_equal(clean_byname(unnamed, tol = 0.0999), matrix(c(1, 2, 0.1, 
                                                             3, 4, -0.1,
                                                             5, 6, 0.05), nrow = 3, byrow = TRUE))
})


test_that("iszero_byname works as expected", {
  m <- matrix(0, nrow = 3, ncol = 2)
  expect_true(iszero_byname(m))
  n <- matrix(1, nrow = 42, ncol = 5)
  expect_false(iszero_byname(n))
})


test_that("getting row names works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  expect_equal(getrownames_byname(m), c("i1", "i2"))
  # This also works for lists
  expect_equal(getrownames_byname(list(m,m)), list(c("i1", "i2"), c("i1", "i2")))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(getrownames_byname(DF$m), list(c("i1", "i2"), c("i1", "i2")))
})


test_that("getting column names works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  expect_equal(getcolnames_byname(m), c("p1", "p2", "p3"))
  # This also works for lists
  expect_equal(getcolnames_byname(list(m,m)), list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(getcolnames_byname(DF$m), list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
})


test_that("setrownames_byname works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setrownames_byname(m, c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("a", "b")) %>% rownames(), 
               c("a", "b"))
  expect_equal(m %>% setrownames_byname(rownames(m2)) %>% rownames(), c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("c", "d")) %>% rownames(), c("c", "d"))
  expect_null(m %>% setrownames_byname(NULL) %>% rownames())
  expect_equal(m %>% setrownames_byname(c(NA, NA)) %>% rownames(), c(NA_character_, NA_character_))
  # The function should convert the constant to a matrix and apply the row name
  expect_equal(2 %>% setrownames_byname("row"), 
               matrix(2, nrow = 1, ncol = 1, dimnames = list(c("row"), NULL)))
})


test_that("setcolnames_byname works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  expect_equal(m %>% setcolnames_byname(c("a", "b", "c")) %>% colnames(), 
               c("a", "b", "c"))
  expect_equal(m %>% setcolnames_byname(c("d", "e", "f")) %>% colnames(), c("d", "e", "f"))
  expect_null(m %>% setcolnames_byname(NULL) %>% colnames())
  expect_equal(m %>% setcolnames_byname(c(NA, NA, NA)) %>% colnames(), c(NA_character_, NA_character_, NA_character_))
  # The function should convert the constant to a matrix and apply the col name
  expect_equal(2 %>% setcolnames_byname("col"), 
               matrix(2, nrow = 1, ncol = 1, dimnames = list(NULL, c("col"))))
})


test_that("setting row names works as expected", {
  m1 <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setrownames_byname(m1, c("a", "b"))
  expect_equal(rownames(m2), c("a", "b"))
  m3 <- setrownames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
  expect_equal(rownames(m3), c("c", "d"))
  m4 <- m1 %>% setrownames_byname(NULL)
  expect_null(rownames(m4))
  m5 <- m1 %>% setrownames_byname(c(NA, NA))
  expect_equal(rownames(m5), c(NA_character_, NA_character_))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setrownames_byname(l1, rownames = list(c("a", "b")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("a", "b")))
  # Try without using a named argument (rownames) to see if they are inferred
  l2 <- setrownames_byname(l1, list(c("a", "b")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("a", "b")))
  # Try without a list. This should fail, because a is applied to the first matrix and b is applied to the second matrix.
  # But each matrix needs 2 rownames.
  expect_error(setrownames_byname(l1, c("a", "b")), 
               "length of 'dimnames' \\[1\\] not equal to array extent")
  
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setrownames_byname(mcol, list(c("r1", "r2")))
    )
  expect_equal(rownames(DF2$mcol2[[1]]), c("r1", "r2"))
  expect_equal(rownames(DF2$mcol2[[2]]), c("r1", "r2"))
  DF3 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setrownames_byname(mcol, list(c("r3", "r4")))
    )
  expect_equal(list(rownames(DF3$mcol2[[1]]), rownames(DF3$mcol2[[2]])), list(c("r3", "r4"), c("r3", "r4")))
})


test_that("setting row names works with different names for each matrix", {
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  mlist <- list(m, m, m)
  new_rownames <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  renamed <- setrownames_byname(mlist, rownames = new_rownames)
  expect_equal(getrownames_byname(renamed), new_rownames)
  
  # Try this in a data frame
  DF <- data.frame(mcol = I(mlist), rownames_col = I(new_rownames))
  DF_renamed <- DF %>% 
    dplyr::mutate(
      mcol_2 = setrownames_byname(mcol, rownames = rownames_col)
    )
  expect_equal(getrownames_byname(DF_renamed$mcol_2), new_rownames)
})


test_that("setting col names works as expected", {
  m1 <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setcolnames_byname(m1, c("a", "b", "c"))
  expect_equal(colnames(m2), c("a", "b", "c"))
  m3 <- setcolnames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("d", "e", "f"))
  expect_equal(colnames(m3), c("d", "e", "f"))
  m4 <- m1 %>% setcolnames_byname(NULL)
  expect_null(colnames(m4))
  m5 <- m1 %>% setcolnames_byname(c(NA, NA, NA))
  expect_equal(colnames(m5), c(NA_character_, NA_character_, NA_character_))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setcolnames_byname(l1, c("a", "b", "c"))
  expect_equal(list(colnames(l2[[1]]), colnames(l2[[2]])), list(c("a", "b", "c"), c("a", "b", "c")))
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(colnames(DF2$mcol2[[1]]), c("c1", "c2", "c3"))
  expect_equal(colnames(DF2$mcol2[[2]]), c("c1", "c2", "c3"))
  DF3 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(list(colnames(DF3$mcol2[[1]]), colnames(DF3$mcol2[[2]])), list(c("c1", "c2", "c3"), c("c1", "c2", "c3")))
})


test_that("setting column names works with different names for each matrix", {
  m <- matrix(c(1, 2,
                3, 4, 
                5, 6), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  mlist <- list(m, m, m)
  new_colnames <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  renamed <- setcolnames_byname(mlist, colnames = new_colnames)
  expect_equal(getcolnames_byname(renamed), new_colnames)
  
  # Try this in a data frame
  DF <- data.frame(mcol = I(mlist), colnames_col = I(new_colnames))
  DF_renamed <- DF %>% 
    dplyr::mutate(
      mcol_2 = setcolnames_byname(mcol, colnames = colnames_col)
    )
  expect_equal(getcolnames_byname(DF_renamed$mcol_2), new_colnames)
})


test_that("renaming rows to prefix or suffix works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("c1", "c2")))
  expected <- m
  rownames(expected) <- c("a", "r2", "r3")
  actual <- rename_to_pref_suff_byname(m, keep = "pref", margin = 1, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  expected <- m
  rownames(expected) <- c("b", "", "")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", margin = 1, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), keep = "suff", margin = 1, notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))
})


test_that("renaming columns to prefix or suffix works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  expected <- m
  colnames(expected) <- c("a", "c")
  actual <- rename_to_pref_suff_byname(m, keep = "pref", margin = 2, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  expected <- m
  colnames(expected) <- c("b", "d")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))
  
  # Check that row and column types are preserved
  m <- m %>% setrowtype("Rows -> Cols") %>% setcoltype("Cols -> Rows")
  res <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res), "Cols")
  expect_equal(coltype(res), "Rows")
})


test_that("renaming rows and columns to prefix or suffix works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  expected <- m
  rownames(expected) <- c("a", "r2", "r3")
  colnames(expected) <- c("a", "c")
  # Default is margin = c(1, 2)
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  expected <- m
  rownames(expected) <- c("b", "", "")
  colnames(expected) <- c("b", "d")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), margin = list(c(1, 2)), keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))

  # Check that row and column types are preserved
  m <- m %>% setrowtype("Rows") %>% setcoltype("Cols")
  res <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res), "Rows")
  expect_equal(coltype(res), "Cols")
})


test_that("renaming rows and cols to pref and suff also changes rowtype and coltype", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "c -> d", "e -> f"), c("g -> h", "i -> j"))) %>% 
    setrowtype("Industry -> Product") %>% setcoltype("Product -> Industry")
  res <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(rownames(res), c("a", "c", "e"))
  expect_equal(colnames(res), c("g", "i"))
  expect_equal(rowtype(res), "Industry")
  expect_equal(coltype(res), "Product")

  res2 <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rownames(res2), c("b", "d", "f"))
  expect_equal(colnames(res2), c("h", "j"))
  expect_equal(rowtype(res2), "Product")
  expect_equal(coltype(res2), "Industry")
})


test_that("changing row and column type correctly ignores missing suffixes", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "c -> d", "e -> f"), c("g -> h", "i -> j"))) %>% 
    setrowtype("Rows") %>% setcoltype("Product -> Industry")
  res <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res), "Rows")
  expect_equal(coltype(res), "Product")

  res_2 <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res_2), "")
  expect_equal(coltype(res_2), "Industry")
})


test_that("setting identical row/col names is OK", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "a -> c", "r3"), c("a -> b", "a -> z")))
  expected <- m
  rownames(expected) <- c("a", "a", "r3")
  colnames(expected) <- c("a", "a")
  # The next call will create duplicate row names and column names in m. 
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  # Interestingly the command View(actual) or View(expected)
  # shows row names that aren't true. 
  # The 2nd row of actual and expected is shown as "a.1", 
  # but the underlying objects have simply "a".
  # (The column names are shown correctly.)
  # This test ensures that R isn't messing with the actual row and column names on the objects.
  expect_equal(actual, expected)
})


test_that("renaming with full prefix identifiers works as expected.", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a [b]", "c [d]", "e [f]"), c("g [h]", "i [j]")))
  expected <- m
  dimnames(expected) <- list(c("a", "c", "e"), c("g", "i"))
  # The next call will create duplicate row names and column names in m. 
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::bracket_notation)
  expect_equal(actual, expected)
  
  expected2 <- m
  dimnames(expected2) <- list(c("a", "c", "e"), c("g [h]", "i [j]"))
  actual2 <- rename_to_pref_suff_byname(m, keep = "pref", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(actual2, expected2)
  
  expected3 <- m
  dimnames(expected3) <- list(c("a [b]", "c [d]", "e [f]"), c("g", "i"))
  actual3 <- rename_to_pref_suff_byname(m, keep = "pref", margin = 2, notation = RCLabels::bracket_notation)
  expect_equal(actual3, expected3)
  
  expected4 <- m
  dimnames(expected4) <- list(c("b", "d", "f"), c("h", "j"))
  actual4 <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::bracket_notation)
  expect_equal(actual4, expected4)

  expected5 <- m
  dimnames(expected5) <- list(c("b", "d", "f"), c("g [h]", "i [j]"))
  actual5 <- rename_to_pref_suff_byname(m, keep = "suff", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(expected5, actual5)

  expected6 <- m
  dimnames(expected6) <- list(c("a [b]", "c [d]", "e [f]"), c("h", "j"))
  actual6 <- rename_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::bracket_notation)
  expect_equal(expected6, actual6)
  
  # Try with a list
  actual_list <- rename_to_pref_suff_byname(list(m, m), keep = "pref", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(actual_list[[1]], expected2)
  expect_equal(actual_list[[2]], expected2)
  
  # Try in a data frame
  DF <- tibble::tibble(m = list(m, m, m, m, m, m), keep = c("pref", "pref", "pref", "suff", "suff", "suff"), 
                       margin = list(c(1, 2), 1, 2, c(1, 2), 1, 2),
                       notation = list(RCLabels::bracket_notation),
                       expected = list(expected, expected2, expected3, expected4, expected5, expected6))
  
  res <- DF %>% 
    dplyr::mutate(
      actual = rename_to_pref_suff_byname(m, keep = keep, margin = margin, 
                                          notation = notation)
    )
  expect_equal(res$actual, res$expected)
})


test_that("setrowtype and rowtype works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products")
  expect_null(U %>% setrowtype(NULL) %>% rowtype())
  expect_equal(rowtype(U), "Products")
  # This also works for lists
  Ul <- setrowtype(list(U,U), rowtype = "Products")
  expect_equal(rowtype(Ul), list("Products", "Products"))
  Ul2 <- setrowtype(list(U,U), rowtype = "Junk")
  expect_equal(rowtype(Ul2), list("Junk", "Junk"))
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setrowtype(DF$U, "Products")
  expect_equal(rowtype(DF2), list("Products", "Products"))
  DF3 <- DF %>% dplyr::mutate(newcol = setrowtype(U, "Products"))
  expect_equal(DF3$newcol %>% rowtype, list("Products", "Products"))
})


test_that("setcoltype and coltype works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setcoltype("Industries")
  expect_null(U %>% setcoltype(NULL) %>% coltype())
  expect_equal(coltype(U), "Industries")
  # This also works for lists
  Ul <- setcoltype(list(U,U), coltype = "Industries")
  expect_equal(coltype(Ul), list("Industries", "Industries"))
  Ul2 <- setcoltype(list(U,U), coltype = "Junk")
  expect_equal(coltype(Ul2), list("Junk", "Junk"))
  # Check that it works when the lists are not same structure as a.
  Ul3 <- setcoltype(list(U,U), coltype = list("Junk", "Junk"))
  expect_equal(coltype(Ul3), list("Junk", "Junk"))
  Ul4 <- setcoltype(list(U,U,U), coltype = list("Junk", "Junk", "Bogus"))
  expect_equal(coltype(Ul4), list("Junk", "Junk", "Bogus"))
  Ul5 <- setcoltype(list(U,U,U), coltype = c("Bogus"))
  expect_equal(coltype(Ul5), list("Bogus", "Bogus", "Bogus"))
  Ul6 <- setcoltype(list(U,U,U), coltype = list("Bogus"))
  expect_equal(coltype(Ul5), list("Bogus", "Bogus", "Bogus"))
  # This one should fail, becuase length of coltype is neither 1 nor length(a), namely 3.
  expect_error(setcoltype(list(U,U,U), coltype = list("Bogus", "Bogus")), 
               "In prepare_.FUNdots\\(\\), when both 'a' and '.FUNdots' are lists, each top-level argument in .FUNdots must have length = 1 or length = length\\(a\\) \\(= 3\\). Found length = 2 for argument 'coltype', which is a list.")
  
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setcoltype(DF$U, "Industries")
  expect_equal(coltype(DF2), list("Industries", "Industries"))
  DF3 <- DF %>% dplyr::mutate(newcol = setcoltype(U, "Industries"))
  expect_equal(DF3$newcol %>% coltype, list("Industries", "Industries"))
})


test_that("nrow_byname() works as expected.", {

  # First, test with a single 2x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  nrow_byname(U) %>%
    expect_equal(2)

  # Second, test with a 3x2 matrix:
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2")
  U2 <- matrix(1:3, ncol = 2, nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  nrow_byname(U2) %>%
    expect_equal(3)

  # Third, test with a 4x3 matrix:
  productnames <- c("p1", "p2", "p3", "p4")
  industrynames <- c("i1", "i2", "i3")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  nrow_byname(U3) %>%
    expect_equal(4)

  # Try with a list
  nrow_with_list <- nrow_byname(list(U, U2, U3))
  expect_equal(nrow_with_list[[1]], 2)
  expect_equal(nrow_with_list[[2]], 3)
  expect_equal(nrow_with_list[[3]], 4)
  
  # Fourth, test with a data frame with U, U2, and U3 in a column:
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )

  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3

  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002

  number_rows <- matsbyname::nrow_byname(dfUs$matrix_byname)

  number_rows[[1]] %>%
    testthat::expect_equal(2)
  number_rows[[2]] %>%
    testthat::expect_equal(3)
  number_rows[[3]] %>%
    testthat::expect_equal(4)


  # Now trying with mutate:
  a <- dfUs %>%
    dplyr::mutate(
      number_of_rows = matsbyname::nrow_byname(matrix_byname)
    )

  testthat::expect_equal(a$number_of_rows[[1]], 2)
  testthat::expect_equal(a$number_of_rows[[2]], 3)
  testthat::expect_equal(a$number_of_rows[[3]], 4)
})


test_that("ncol_byname() works as expected.", {
  
  # First, test with a single 2x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  ncol_byname(U) %>% 
    expect_equal(2)
  
  # Second, test with a 3x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matrix(1:3, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  ncol_byname(U2) %>% 
    expect_equal(3)
  
  # Third, test with a 4x3 matrix:
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  ncol_byname(U3) %>% 
    expect_equal(4)
  
  
  # Try with a list
  ncol_with_list <- ncol_byname(list(U, U2, U3))
  expect_equal(ncol_with_list[[1]], 2)
  expect_equal(ncol_with_list[[2]], 3)
  expect_equal(ncol_with_list[[3]], 4)
  
  
  # Fourth, test with a data frame with both U, U2, and U3:
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  number_cols <- ncol_byname(dfUs$matrix_byname)
  
  number_cols[[1]] %>% 
    expect_equal(2)
  number_cols[[2]] %>% 
    expect_equal(3)
  number_cols[[3]] %>% 
    expect_equal(4)
  
  
  # Now trying with mutate:
  a <- dfUs %>% 
    dplyr::mutate(
      number_of_cols = ncol_byname(matrix_byname)
    )
  
  expect_equal(a$number_of_cols[[1]], 2)
  expect_equal(a$number_of_cols[[2]], 3)
  expect_equal(a$number_of_cols[[3]], 4)
})


test_that("create_matrix_byname() works as expected", {
  
  single_mat_with_types <- create_matrix_byname(1 %>% setrowtype("testing_rowtype") %>% setcoltype("testing_coltype"),
                                                nrow = 1, ncol = 1,
                                                dimnames = list("r1", "c1"))
  # "all" retains row and column types
  expect_equal(single_mat_with_types, matrix(1, dimnames = list("r1", "c1")) %>%
                 setrowtype("testing_rowtype") %>%
                 setcoltype("testing_coltype"))

  # Test with row and column types
  single_mat_with_types <- create_matrix_byname(1 %>% setrowtype("rt") %>% setcoltype("ct"),
                                                nrow = 1, ncol = 1,
                                                dimnames = list("r1", "c1"))
  expect_equal(rowtype(single_mat_with_types), "rt")
  expect_equal(coltype(single_mat_with_types), "ct")
  
  single_mat_2 <- create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                                       dimnames = list(c("r1", "r2"), "c1"))
  
  expect_equal(single_mat_2, matrix(c(1,2), nrow = 2, ncol = 1,
                                    dimnames = list(c("r1", "r2"), "c1")))
  
  # Try with a list
  list_of_mats <- create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")))
  
  expect_equal(list_of_mats[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(list_of_mats[[2]], matrix(2, dimnames = list("R1", "C1")))

  list_of_mats <- create_matrix_byname(list(1, 2) %>% setrowtype("testing_rowtypes") %>% setcoltype("testing_coltypes"),
                                       nrow = list(1, 1), ncol = list(1,1),
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")))

  expect_equal(list_of_mats[[1]], matrix(1, dimnames = list("r1", "c1")) %>%
                 setrowtype("testing_rowtypes") %>%
                 setcoltype("testing_coltypes"))
  expect_equal(list_of_mats[[2]], matrix(2, dimnames = list("R1", "C1")) %>%
                 setrowtype("testing_rowtypes") %>%
                 setcoltype("testing_coltypes"))
  
  # Test with a list of different dimensions
  list_of_mats_2 <- create_matrix_byname(list(1, c(2, 3, 4, 5)), nrow = list(1, 2), ncol = list(1,2), 
                                         dimnames = list(list("r1", "c1"), list(c("R1", "R2"), c("C1", "C2"))))
  
  expect_equal(list_of_mats_2[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(list_of_mats_2[[2]], matrix(c(2, 3, 4, 5), nrow = 2, ncol = 2, dimnames = list(c("R1", "R2"), c("C1", "C2")), byrow = FALSE))
  
  # Try in a data frame
  df1 <- data.frame(
    dat = I(list()),
    nrows = I(list()),
    ncols = I(list()),
    dimnms = I(list())
  )
  df1[[1, "dat"]] <- 1
  df1[[2, "dat"]] <- 2
  df1[[3, "dat"]] <- c(1,2,3,4, 5, 6)
  df1[[1, "nrows"]] <- 1
  df1[[2, "nrows"]] <- 1
  df1[[3, "nrows"]] <- 2
  df1[[1, "ncols"]] <- 1
  df1[[2, "ncols"]] <- 1
  df1[[3, "ncols"]] <- 3
  df1[[1, "dimnms"]] <- list("r1", "c1")
  df1[[2, "dimnms"]] <- list("R1", "C1")
  df1[[3, "dimnms"]] <- list(c("r1", "r2"), c("c1", "c2", "c3"))

  res1 <- df1 %>%
    dplyr::mutate(
      mat_col = create_matrix_byname(dat,
                                     nrow = nrows,
                                     ncol = ncols,
                                     byrow = TRUE,
                                     dimnames = dimnms))
  expect_equal(res1$mat_col[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(res1$mat_col[[2]], matrix(2, dimnames = list("R1", "C1")))
  expect_equal(res1$mat_col[[3]], matrix(c(1, 2, 3,
                                           4, 5, 6), 
                                         byrow = TRUE,
                                         nrow = 2, ncol = 3,
                                         dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))))
  
  # Next, tests using a data frame with U matrices
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matrix(1:3, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list()),
    dat = I(list()),
    number_of_rows = I(list()),
    number_of_cols = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  dfUs_added_matrix <- dfUs %>% 
    dplyr::mutate(
      dat = I(list(1)),
      number_of_rows = I(matsbyname::nrow_byname(matrix_byname)),
      number_of_cols = I(matsbyname::ncol_byname(matrix_byname)),
      row_names = matsbyname::getrownames_byname(matrix_byname),
      col_names = matsbyname::getcolnames_byname(matrix_byname),
      dimension_names = purrr::map2(.x = row_names, .y = col_names, .f = list),
      row_types_col = I(list("testing_rowtypes")),
      col_types_col = I(list("testing_coltypes"))
    )
  
  res2 <- dfUs_added_matrix %>% 
    dplyr::mutate(
      new_matrix = matsbyname::create_matrix_byname(
        dat,
        nrow = number_of_rows,
        ncol = number_of_cols,
        dimnames = dimension_names
      )
    )
  
  expect_equal(
    res2$new_matrix[[1]],
    matrix(1, ncol = 2, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2")))
  )
  
  expect_equal(
    res2$new_matrix[[2]],
    matrix(1, ncol = 3, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3")))
  )
  
  expect_equal(
    res2$new_matrix[[3]],
    matrix(1, ncol = 4, nrow = 3, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2", "i3", "i4")))
  )
  
  res3 <- dfUs_added_matrix %>% 
    dplyr::mutate(
      new_matrix = matsbyname::create_matrix_byname(
        dat,
        nrow = number_of_rows,
        ncol = number_of_cols,
        dimnames = dimension_names
      )
    )
  
  expect_equal(
    res3$new_matrix[[1]],
    matrix(1, ncol = 2, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2")))
  )
  
  expect_equal(
    res3$new_matrix[[2]],
    matrix(1, ncol = 3, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3")))
  )
  
  expect_equal(
    res3$new_matrix[[3]],
    matrix(1, ncol = 4, nrow = 3, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2", "i3", "i4")))
  )
})


test_that("create_rowvec_byname() works as expected", {
  # Try with a single number
  single_vec <- create_rowvec_byname(c(c1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"), rowname = "r1")
  expect_equal(single_vec, matrix(1, dimnames = list("r1", "c1")) %>% setrowtype("rt") %>% setcoltype("ct"))
  
  # Test with dimnames
  sv_dimnames <- create_rowvec_byname(1, dimnames = list("r1", "c1"))
  expect_equal(sv_dimnames, matrix(1, dimnames = list("r1", "c1")))
  
  # Try with a vector of numbers
  vector_vec <- create_rowvec_byname(c(c1 = 1, c2 = 2), rowname = "r1")
  expect_equal(vector_vec, matrix(c(1,2), ncol = 2, byrow = TRUE, dimnames = list("r1", c("c1", "c2"))))
  
  # Try with a list of vectors
  vv_vec <- create_rowvec_byname(list(c(c1 = 1, c2 = 2), c(C1 = 3, C2 = 4, C3 = 5)),
                                 rowname = list("r1", "R1"))
  expect_equal(vv_vec[[1]], matrix(c(1,2), ncol = 2, dimnames = list("r1", c("c1", "c2"))))
  
  # Try in a data frame
  dat <- list(c(c1 = 1), c(C1 = 2, C2 = 3), c(c1 = 1, c2 = 2, c3 = 3, c4 = 4, c5 = 5, c6 = 6))
  rnms <- list("r1", "R1", "r1")
  
  df1 <- tibble::tibble(dat, rnms)
  res1 <- df1 %>%
    dplyr::mutate(
      rowvec_col = create_rowvec_byname(dat, rowname = rnms)
    )
  expect_equal(res1$rowvec_col[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(res1$rowvec_col[[2]], matrix(c(2, 3), ncol = 2, dimnames = list("R1", c("C1", "C2"))))
  expect_equal(res1$rowvec_col[[3]], matrix(c(1, 2, 3, 4, 5, 6), 
                                            nrow = 1, ncol = 6,
                                            dimnames = list("r1", c("c1", "c2", "c3", "c4", "c5", "c6"))))
  
  # Try in data frame with dimnames and named vector.  See which one wins.
  dimnms <- list(list("r01", "c01"), list("R01", c("C01", "C02")), list("r01", c("c01", "c02", "c03", "c04", "c05", "c06")))
  df2 <- tibble::tibble(dat, rnms, dimnms)
  res2 <- df2 %>% 
    dplyr::mutate(
      rowvec_col = create_rowvec_byname(dat, dimnames = dimnms, rowname = rnms)
    )
  # Explicitly setting dimnames should win.
  expect_equal(res2$rowvec_col[[1]], matrix(1, dimnames = list("r01", "c01")))
  expect_equal(res2$rowvec_col[[2]], matrix(c(2, 3), ncol = 2, dimnames = list("R01", c("C01", "C02"))))
  expect_equal(res2$rowvec_col[[3]], matrix(c(1, 2, 3, 4, 5, 6), 
                                            nrow = 1, ncol = 6,
                                            dimnames = list("r01", c("c01", "c02", "c03", "c04", "c05", "c06"))))
})


test_that("create_colvec_byname() works as expected", {
  # Try with a single number
  single_vec <- create_colvec_byname(c(r1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"),
                                     colname = "c1")
  expect_equal(single_vec, matrix(1, dimnames = list("r1", "c1")) %>% setrowtype("rt") %>% setcoltype("ct"))
  
  # Try with a vector of numbers
  vector_vec <- create_colvec_byname(c(r1 = 1, r2 = 2), colname = "c1")
  expect_equal(vector_vec, matrix(c(1,2), nrow = 2, dimnames = list(c("r1", "r2"), "c1")))
  
  # Try with a list of vectors
  vv_vec <- create_colvec_byname(list(c(r1 = 1, r2 = 2), c(R1 = 3, R2 = 4, R3 = 5)),
                                 colname = list("c1", "C1"))
  expect_equal(vv_vec[[1]], matrix(c(1,2), nrow = 2, dimnames = list(c("r1", "r2"), "c1")))
  
  # Try in a data frame
  dat <- list(c(r1 = 1), c(R1 = 2, R2 = 3), c(r1 = 1, r2 = 2, r3 = 3, r4 = 4, r5 = 5, r6 = 6))
  cnms <- list("c1", "C1", "c1")
  
  df1 <- tibble::tibble(dat, cnms)
  res1 <- df1 %>%
    dplyr::mutate(
      colvec_col = create_colvec_byname(dat, colname = cnms)
    )
  expect_equal(res1$colvec_col[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(res1$colvec_col[[2]], matrix(c(2, 3), nrow = 2, dimnames = list(c("R1", "R2"), "C1")))
  expect_equal(res1$colvec_col[[3]], matrix(c(1, 2, 3, 4, 5, 6), 
                                            nrow = 6, ncol = 1,
                                            dimnames = list(c("r1", "r2", "r3", "r4", "r5", "r6"), "c1")))
  
  # Try in data frame with dimnames and named vector.  See which one wins.
  dimnms <- list(list("r01", "c01"), list(c("R01", "R02"), "C01"), list(c("r01", "r02", "r03", "r04", "r05", "r06"), "c01"))
  df2 <- tibble::tibble(dat, cnms, dimnms) 
  res2 <- df2 %>% 
    dplyr::mutate(
      colvec_col = create_colvec_byname(dat, dimnames = dimnms, colname = cnms)
    )
  # Explicitly setting dimnames should win.
  expect_equal(res2$colvec_col[[1]], matrix(1, dimnames = list("r01", "c01")))
  expect_equal(res2$colvec_col[[2]], matrix(c(2, 3), nrow = 2, dimnames = list(c("R01", "R02"), "C01")))
  expect_equal(res2$colvec_col[[3]], matrix(c(1, 2, 3, 4, 5, 6), 
                                            nrow = 6, ncol = 1,
                                            dimnames = list(c("r01", "r02", "r03", "r04", "r05", "r06"), "c01")))
})


test_that("kvec_from_template_byname() works as expected", {
  m <- matrix(42, nrow = 4, ncol = 2,
              dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2")))
  expect_equal(kvec_from_template_byname(m, colname = "mycol"), matrix(1, nrow = 4, ncol = 1, 
                                                                       dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  expect_equal(kvec_from_template_byname(m, colname = "myrow", column = FALSE), matrix(1, nrow = 1, ncol = 2, 
                                                                                       dimnames = list("myrow", c("c1", "c2"))))
  
  # Try in a data frame.
  df1 <- tibble::tibble(m = list(m, m), cnme = "mycol", rnme = "myrow", clmn = TRUE, k = c(42, 43), 
                        rtype = "rt", ctype = c("ct1", "ct2"))
  
  res1 <- df1 %>% 
    dplyr::mutate(
      irow = kvec_from_template_byname(m, colname = rnme, column = FALSE),
      icol = kvec_from_template_byname(m, colname = cnme, column = clmn), 
      kcol = kvec_from_template_byname(m, k = k, colname = cnme), 
      with_rt_ct = kvec_from_template_byname(m %>% setrowtype(rtype) %>% setcoltype(ctype), colname = cnme)
    )
  
  expect_equal(res1$irow[[1]], matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  expect_equal(res1$irow[[2]], matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  expect_equal(res1$icol[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$icol[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$icol[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  # Try with non-1 value for k
  expect_equal(res1$kcol[[1]], matrix(42, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$kcol[[2]], matrix(43, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  # Test that row and column types are transferred correctly
  expect_equal(res1$with_rt_ct[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                 setrowtype("rt") %>% setcoltype("ct1"))
  expect_equal(res1$with_rt_ct[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                 setrowtype("rt") %>% setcoltype("ct2"))
})


test_that("kvec_from_template_byname() function passes old i_byname tests", {
  
  # First, test with single values.
  single_mat <- create_matrix_byname(1, nrow = 1, ncol = 1,
                                     dimnames = list("r1", "c1"))
  
  expect_equal(
    kvec_from_template_byname(single_mat, colname = "output_column"), 
    matrix(1, dimnames = list("r1", "output_column"))
  )
  
  single_mat_2 <- create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                                       dimnames = list(c("r1", "r2"), "c1"))
  expect_equal(
    kvec_from_template_byname(single_mat_2, colname = "output_column"), 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "output_column"))
  )
  
  # Second, test with a list
  list_of_mats <- create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")))
  
  res_list <- kvec_from_template_byname(list_of_mats, colname = "output_column")
  
  expect_equal(
    res_list[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  expect_equal(
    res_list[[2]], 
    matrix(1, dimnames = list("R1", "output_column"))
  )
  
  # Test with a list of different dimensions
  list_of_mats_2 <- create_matrix_byname(list(1, c(2, 3, 4, 5)), nrow = list(1, 2), ncol = list(1,2), 
                                         dimnames = list(list("r1", "c1"), list(c("R1", "R2"), c("C1", "C2"))))
  
  res_list_2 <- kvec_from_template_byname(list_of_mats_2, colname = "output_column")
  
  expect_equal(
    res_list_2[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  expect_equal(
    res_list_2[[2]], 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("R1", "R2"), "output_column"))
  )
  
  
  # Third test with data frames:
  
  # Creating data frame of matrices, with a year column and a matrix column:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matrix(1:3, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  # Now creating the unity vector
  res <- dfUs %>% 
    dplyr::mutate(
      unity_vec = kvec_from_template_byname(matrix_byname, colname = "Product")
    )
  
  # Checking number of coefficients in each vector
  expect_equal(nrow(res$unity_vec[[1]]), 2)
  expect_equal(nrow(res$unity_vec[[2]]), 2)
  expect_equal(nrow(res$unity_vec[[3]]), 3)
  
  # Checking rowtypes
  expect_equal(res$unity_vec[[1]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[2]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[3]] %>% rowtype(), "Products")
  
  # Checking coltypes
  expect_equal(res$unity_vec[[1]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[2]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[3]] %>% coltype(), "Industries")
  
  # Checking single coefficient values
  expect_equal(res$unity_vec[[1]][["p1", "Product"]], 1)
  expect_equal(res$unity_vec[[2]][["p2", "Product"]], 1)
  expect_equal(res$unity_vec[[3]][["p3", "Product"]], 1)
  
  # Checking sums
  res2 <- res %>%
    dplyr::mutate(
      sum_unity = matsbyname::sumall_byname(unity_vec)
    )
  # Check
  expect_equal(res2$sum_unity[[1]], 2)
  expect_equal(res2$sum_unity[[2]], 2)
  expect_equal(res2$sum_unity[[3]], 3)
})


test_that("vec_from_store_byname() works as expected with single matrices", {
  a <- matrix(42, nrow = 2, ncol = 3, 
              dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  v <- matrix(1:10, nrow = 10, ncol = 1, 
              dimnames = list(paste0("r", 1:10) %>% rev(), "c1")) %>%
    setrowtype("rt") %>% setcoltype("ct")
  expect_equal(vec_from_store_byname(a = a, v = v), 
               matrix(c(10, 9), nrow = 2, ncol = 1, 
                      dimnames = list(c("r1", "r2"), "c1")) %>%
                 setrowtype("rt") %>% setcoltype("ct"))
})


test_that("vec_from_store_byname() works as expected with single matrices and nouns", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Hydro", 
                                "Crude oil",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "Brown coal"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v, a_piece = "noun"), 
               matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single matrices and pref suff", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Hydro", 
                                "Crude oil",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "Brown coal"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  # Try with prefixes
  expect_equal(vec_from_store_byname(a, v, a_piece = "pref"), 
               matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  # Try with suffixes
  v2 <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "from e in f", 
                                "Hydro", 
                                "Crude oil",
                                "from b in c", 
                                "Hard coal (if no detail)", 
                                "from Production in USA"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v2, a_piece = "suff"), 
               matrix(c(5, 2, 7), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single matrices and prepositions", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "USA", 
                                "c",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expect_equal(vec_from_store_byname(a, v, a_piece = "in"), 
               matrix(c(4, 7, 3), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  

  v2 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail)", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v2, a_piece = "from"), 
               matrix(c(7, 4, 3), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  # Try when the preposition (in this case "to") is not present in a.
  expect_equal(vec_from_store_byname(a, v, a_piece = "to"), 
               matrix(c(NA_real_, NA_real_, NA_real_), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  
  # Try when we use different pieces of a and v.
  a3 <- matrix(42, nrow = 3, ncol = 5, 
               dimnames = list(c("Electricity [from b in GBR]", 
                                 "Coal [from e in f]", 
                                 "Crude oil [from Production in USA]"), 
                               c("Main activity producer electricity plants", 
                                 "Wind turbines", 
                                 "Oil refineries", 
                                 "Coal mines", 
                                 "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v3 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity [from USA]", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail) [from GBR]", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expect_error(vec_from_store_byname(a3, v3, a_piece = "in", v_piece = "from"), 
               "v_pieces must be unique in vec_from_store_byname")
              
               
  v4 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity [from USA]", 
                                 "Peat [from nowhere]", 
                                 "Production [from GHA]", 
                                 "e [from ZAF]",
                                 "Coal [from AUS]", 
                                 "Hard coal (if no detail) [from GBR]", 
                                 "b [from Nebraska]"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
               
  expect_equal(vec_from_store_byname(a3, v4, a_piece = "in", v_piece = "from"), 
                matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in GBR]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works when a row vector is desired.", {
  a <- matrix(42, nrow = 3, ncol = 2, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Wind turbines", 
                                "Oil wells"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Wind turbines", 
                                "c",
                                "Oil wells", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "eta")) %>%
    setrowtype("Industry") %>% setcoltype("eta")
  
  expect_equal(vec_from_store_byname(a, v, a_piece = "pref", column = FALSE), 
               matrix(c(3, 5), nrow = 1, ncol = 2, 
                      dimnames = list("eta", 
                                      c("Wind turbines", 
                                        "Oil wells"))) %>%
                 setrowtype("eta") %>% setcoltype("Industry"))
  
  # See if it works with a row vector for v.
  v_row <- matrix(1:7, nrow = 1, ncol = 7, 
                  dimnames = list("eta", 
                                  c("Electricity", 
                                    "Peat", 
                                    "Wind turbines", 
                                    "c",
                                    "Oil wells", 
                                    "Hard coal (if no detail)", 
                                    "f"))) %>%
    setrowtype("eta") %>% setcoltype("Industry")
  expect_equal(vec_from_store_byname(a, v_row, a_piece = "pref", column = FALSE), 
               matrix(c(3, 5), nrow = 1, ncol = 2, 
                      dimnames = list("eta", 
                                      c("Wind turbines", 
                                        "Oil wells"))) %>%
                 setrowtype("eta") %>% setcoltype("Industry"))
  
})


test_that("vec_from_store_byname() works with lists", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in GBR]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  
  a_list <- list(a, a, a)
  v_list <- list(v, v, v)
  expected_list <- list(expected, expected, expected)
  
  # Try with notation and prepositions already wrapped in lists.
  res <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from", 
                        notation = list(RCLabels::bracket_notation), 
                        prepositions = list(RCLabels::prepositions))  
  expect_equal(res, expected_list)
  
  # Try with notation and prepositions not already wrapped in lists.

  res2 <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from")
  expect_equal(res2, expected_list)

})


test_that("vec_from_store_byname() works in a data frame", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in GBR]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  df <- tibble::tibble(a = list(a, a, a), 
                       v = list(v, v, v), 
                       expected = list(expected, expected, expected))
  
  with_res <- df %>%
    dplyr::mutate(
      actual = vec_from_store_byname(a = a, v = v, a_piece = "in", v_piece = "from")
    )
  expect_equal(with_res$actual, with_res$expected)
})


test_that("rename_to_piece_byname() works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  res1 <- rename_to_piece_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
  expected1 <- m
  dimnames(expected1) <- list(c("a", "r2", "r3"), c("a", "c"))
  expect_equal(res1, expected1)
  
  res2 <- rename_to_piece_byname(m, piece = "suff", notation = RCLabels::arrow_notation)
  expected2 <- m
  dimnames(expected2) <- list(c("b", "", ""), c("b", "d"))
  expect_equal(res2, expected2)
  
  # Check that it works for different margins.
  res3 <- rename_to_piece_byname(m, piece = "pref", margin = 1,
                                 notation = RCLabels::arrow_notation)
  expected3 <- m
  dimnames(expected3) <- list(c("a", "r2", "r3"), c("a -> b", "c -> d"))
  expect_equal(res3, expected3)

  res4 <- rename_to_piece_byname(m, piece = "suff", margin = 2,
                                 notation = RCLabels::arrow_notation)
  expected4 <- m
  dimnames(expected4) <- list(c("a -> b", "r2", "r3"), c("b", "d"))
  expect_equal(res4, expected4)
    
  # Check that it works in a list.
  res5 <- rename_to_piece_byname(list(m, m), piece = list("pref", "suff"), 
                                 margin = list(1, 2),
                                 notation = RCLabels::arrow_notation)
  expected5 <- list(expected3, expected4)
  expect_equal(res5, expected5)
  
  # Check that margins can be determined from types.
  m2 <- m %>%
    setrowtype("rows") %>% setcoltype("cols")
  res6 <- rename_to_piece_byname(m2, piece = "pref", margin = "rows",
                                 notation = RCLabels::arrow_notation)
  expected6 <- m2
  dimnames(expected6) <- list(c("a", "r2", "r3"), c("a -> b", "c -> d"))
  expect_equal(res6, expected6)
  
  res7 <- rename_to_piece_byname(m2, piece = "suff", margin = "rows",
                                 notation = RCLabels::arrow_notation)
  expected7 <- m2
  dimnames(expected7) <- list(c("b", "", ""), c("a -> b", "c -> d"))
  expected7 <- expected7 %>%
    setrowtype("")
  expect_equal(res7, expected7)
})


test_that("margin_from_types_byname() works as expected", {
  # Try with a single matrix
  m <- matrix(1) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  expect_equal(margin_from_types_byname(m, "Product"), 1)
  expect_equal(margin_from_types_byname(m, "Industry"), 2)
  expect_equal(margin_from_types_byname(m, c("Product", "Industry")), c(1, 2))
  expect_equal(margin_from_types_byname(m, c("Industry", "Product")), c(1, 2))
  
  # Try with a type that isn't in the row or column types.
  expect_equal(margin_from_types_byname(m, "bogus"), NA_integer_)
  
  # Try with one type that IS in the row or column types and one type that is not.
  expect_equal(margin_from_types_byname(m, c("bogus", "Product")), 1)
  
  # Try with a non-character types argument
  expect_equal(margin_from_types_byname(m, c(1, 2)), c(1, 2))
  
  # Try with a list of matrices
  expect_equal(margin_from_types_byname(list(m, m), types = "Product"), 
               list(1, 1))
  expect_equal(margin_from_types_byname(list(m, m), types = "Industry"), 
               list(2, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Product", "Product")), 
               list(1, 1))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Industry", "Industry")), 
               list(2, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Product", "Industry")), 
               list(1, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = list("Product", "Industry")), 
               list(1, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = list(c("Product", "Industry"))), 
               list(c(1, 2), c(1, 2)))
  expect_equal(margin_from_types_byname(list(m, m), types = list(c("Product", "Industry"), 
                                                                 c("Product", "Industry"))), 
               list(c(1, 2), c(1, 2)))
    
  # Try in a data frame.
  m2 <- matrix(2) %>%
    setrowtype("Industry") %>% setcoltype("Product")
  df <- tibble::tibble(m = list(m, m2), 
                       types1 = list("Product", "Industry"), 
                       types2 = list(c("Product", "Industry"), "Industry"), 
                       types3 = "bogus")
  res <- df %>%
    dplyr::mutate(
      margin1 = margin_from_types_byname(m, types1), 
      margin2 = margin_from_types_byname(m, types2), 
      margin3 = margin_from_types_byname(m, types3)
    )
  
  expect_equal(res$margin1, list(1, 1))
  expect_equal(res$margin2, list(c(1, 2), 1))
  expect_equal(res$margin3, list(NA_integer_, NA_integer_))
})

