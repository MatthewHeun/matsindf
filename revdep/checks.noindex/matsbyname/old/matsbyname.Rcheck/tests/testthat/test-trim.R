test_that("trim_rows_cols() works in degenerate cases", {
  # When a is NULL, we expect NULL as a result
  a <- matrix(c(1, 2, 3, 
                4, 5, 6, 
                7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
  mat <- matrix(c(1, 2,
                  3, 4), nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1", "r3"), c("c1", "c3")))

  expect_null(trim_rows_cols(NULL, mat))
  
  # When mat is NULL, we expect a returned unmodified
  expect_equal(trim_rows_cols(a, NULL), a)
  
  # When mat has NULL dimnames, a is returned unmodified
  mat2 <- mat
  dimnames(mat2) <- NULL
  expect_warning(res <- trim_rows_cols(a, mat2), "NULL names in trim_rows_cols, despite 'mat_mat' being specified. Returning 'a_mat' unmodified.")
  expect_equal(res, a)
  
  # `mat` has `NULL` for dimnames on `margin`, an error is returned.
  mat3 <- mat
  dimnames(mat3) <- list(c("r1", "r3"), NULL)
  expect_error(trim_rows_cols(a, mat3), "NULL dimnames for margin = 2 on 'mat")

  mat4 <- mat
  dimnames(mat4) <- list(NULL, c("c1", "c3"))
  expect_error(trim_rows_cols(a, mat4), "NULL dimnames for margin = 1 on 'mat")
})


test_that("errors are triggered with erroneous input", {
  expect_error(trim_rows_cols(a = NULL, mat = NULL), 
               "Both a and mat are NULL in complete_rows_cols")
  expect_error(trim_rows_cols(a = data.frame(a = 1), mat = matrix(1)), 
               "a cannot be a data frame in complete_rows_cols")
})


test_that("trim_rows_cols() works with a single number", {
  a <- 1
  mat <- matrix(42, dimnames = list("r1", "c1"))
  expect_error(trim_rows_cols(a, mat), regexp = "a_mat must be a matrix in matsbyname::trim_rows_cols")
})


test_that("trim_rows_cols() works as expected with single matrices", {
  a <- matrix(c(1, 2, 3, 
                4, 5, 6, 
                7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  mat <- matrix(c(1, 2, 3,
                  4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                dimnames = list(c("r1", "bogus"), c("c1", "bogus", "c2"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  # Test trimming rows
  res1 <- trim_rows_cols(a, mat, margin = 1, warn_if_a_incomplete = FALSE)
  expect_equal(res1, matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE, 
                            dimnames = list(c("r1"), c("c1", "c2", "c3"))) %>% 
                 setrowtype("rowtype") %>% setcoltype("coltype"))
  
  # Test trimming cols
  res2 <- trim_rows_cols(a, mat, margin = 2, warn_if_a_incomplete = FALSE)
  expect_equal(res2, matrix(c(1, 2, 
                              4, 5, 
                              7, 8), nrow = 3, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))) %>% 
                 setrowtype("rowtype") %>% setcoltype("coltype"))
  
  # Test trimming both rows and cols
  res3 <- trim_rows_cols(a, mat, warn_if_a_incomplete = FALSE)
  expect_equal(res3, matrix(c(1, 2), nrow = 1, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1"), c("c1", "c2"))) %>% 
                 setrowtype("rowtype") %>% setcoltype("coltype"))
})


test_that("trim_rows_cols() works when a list is given", {
  a <- matrix(c(1, 2, 3, 
                4, 5, 6, 
                7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  mat <- matrix(c(1, 2, 3,
                  4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                dimnames = list(c("r1", "bogus"), c("c1", "bogus", "c2"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  a_list <- list(a, a)
  mat_list <- list(mat, mat)
  
  res <- matrix(c(1, 2), nrow = 1, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1"), c("c1", "c2"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  expect_equal(trim_rows_cols(a_list, mat, margin = list(c(1,2), c(1,2)), warn_if_a_incomplete = FALSE), 
               list(res, res))
  
  expect_equal(trim_rows_cols(a_list, list(mat, mat), margin = list(c(1,2), c(1,2)), warn_if_a_incomplete = FALSE), 
               list(res, res))
  
  # Try with an unbalanced list
  expect_warning(trim_rows_cols(a_list, list(mat, mat, mat), margin = list(c(1,2), c(1,2))), 
                 "longer argument not a multiple of length of shorter")
  
  # Try with a being a list and mat being NULL.
  # Should get 2 a matrices.
  expect_equal(trim_rows_cols(a_list, NULL, margin = list(c(1,2), c(1,2))), 
               a_list)
})


test_that("trim_rows_cols() warns when a does not contain all items in mat", {
  R <- matrix(c(1, 2, 3, 
                4, 5, 6, 
                7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  phi <- matrix(c(1, 1.05, 0.931), nrow = 1, ncol = 3, 
                dimnames = list("r1", c("c1", "c2", "c3"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  # Test trimming columns. Everything is present, so nothing should be trimmed, and
  # there should be no warning.
  res1 <- trim_rows_cols(phi, R, margin = 2)
  expect_equal(res1, phi)  
  
  # Now try with a smaller R. 
  # This time, columns of phi should be trimmed.
  R2 <- R[, c(1,2), drop = FALSE] %>% setrowtype("rowtype") %>% setcoltype("coltype")
  res2 <- trim_rows_cols(phi, R2, margin = 2)
  expected2 <- matrix(c(1, 1.05), nrow = 1, dimnames = list("r1", c("c1", "c2"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  expect_equal(res2, expected2)
  
  # Now try with a smaller phi.  This is the case where we want a warning.
  # We want to be sure that phi has columns that match all columns in R,
  # else the calculation will not be correct.
  phi3 <- phi[ , c(2,3), drop = FALSE] %>% setrowtype("rowtype") %>% setcoltype("coltype")
  expect_warning(res3 <- trim_rows_cols(phi3, R, margin = 2), "In trim_rows_cols, 'a' is missing the following rows or columns relative to 'mat': c1")
  
  # Try it with the warning turned off
  res4 <- trim_rows_cols(phi3, R, margin = 2, warn_if_a_incomplete = FALSE)
  expected4 <- matrix(c(1.05, 0.931), nrow = 1, dimnames = list("r1", c("c2", "c3"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  expect_equal(res4, expected4)
})


test_that("trim_rows_cols() respects pieces", {
  R <- matrix(c(1, 2, 3,  
                4, 5, 6, 
                7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1 [from Resources]", "c2 [from USA]", "c3 [from GHA]"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  phi <- matrix(c(1, 1.05, 0.931, 42), nrow = 1, ncol = 4, 
                dimnames = list("r1", c("x [of c1]", "y [of c2]", "z [of c3]", "bogus"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  res1 <- trim_rows_cols(a = phi, mat = R, margin = 2, a_piece = "of", mat_piece = "pref")
  expected1 <- matrix(c(1, 1.05, 0.931), nrow = 1, 
                      dimnames = list("r1", c("x [of c1]", "y [of c2]", "z [of c3]"))) %>%
    setrowtype("rowtype") %>% setcoltype("coltype")
  expect_equal(res1, expected1)
  
  # Try with the noun instead of the prefix
  res2 <- trim_rows_cols(a = phi, mat = R, margin = 2, a_piece = "of", mat_piece = "noun")
  # Should obtain the same result.
  expect_equal(res2, expected1)

})
