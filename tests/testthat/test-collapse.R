# Contains tests for the matsindf package.


test_that("small example works as expected", {
  tidy <- tibble::tibble(matrix = c("V1", "V1", "V1", "V2", "V2"),
                 row = c("i1", "i1", "i2", "i1", "i2"),
                 col = c("p1", "p2", "p2", "p1", "p2"),
                 vals = c(1, 2, 3, 4, 5)) %>%
    dplyr::mutate(
      rowtypes = "Industries",
      coltypes  = "Products"
    )
  # Group on too many columns and expect an error.
  expect_error(collapse_to_matrices(tidy %>% dplyr::group_by(matrix, row),
                               matnames = "matrix", matvals = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = "rowtypes", coltypes = "coltypes"),
               "row is/are grouping variable/s. Cannot group on rownames, colnames, rowtypes, coltypes, or matvals in argument .DF of collapse_to_matrices.")
  # Try with NULL rowtypes but non-NULL coltypes and expect an error.
  expect_error(collapse_to_matrices(tidy %>% dplyr::group_by(matrix),
                               matnames = "matrix", matvals = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = NULL, coltypes = "coltypes"),
               "One of rowtypes or coltypes was non-NULL while the other was NULL. Both need to be NULL or both need to be non-NULL in collapse_to_matrices.")
  # Try with NULL coltypes but non-NULL rowtypes and expect an error.
  expect_error(collapse_to_matrices(tidy %>% dplyr::group_by(matrix),
                               matnames = "matrix", matvals = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = "rowtypes", coltypes = NULL),
               "One of rowtypes or coltypes was non-NULL while the other was NULL. Both need to be NULL or both need to be non-NULL in collapse_to_matrices.")
  # Group on the right things and expect success.
  mats <- collapse_to_matrices(tidy %>% dplyr::group_by(matrix),
                               matnames = "matrix", matvals = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = "rowtypes", coltypes = "coltypes")
  # Check that groups are discarded.
  expect_equal(length(dplyr::group_vars(mats)), 0)
  # Check that factors are not created for String columns.
  expect_false(is.factor(mats$matrix))
  # Test for V1
  expect_equal(mats$vals[[1]], matrix(c(1, 2, 0, 3), nrow = 2, ncol = 2, byrow = TRUE,
                                            dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>%
    matsbyname::setrowtype("Industries") %>% matsbyname::setcoltype("Products"))
  # Test for V2
  expect_equal(mats$vals[[2]], matrix(c(4, 0, 0, 5), nrow = 2, ncol = 2, byrow = TRUE,
                                      dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>%
                 matsbyname::setrowtype("Industries") %>% matsbyname::setcoltype("Products"))
  # Now expand everything back out, just for good measure
  tidy2 <- mats %>%
    expand_to_tidy(matnames = "matrix", matvals = "vals",
                   rownames = "row", colnames = "col",
                   rowtypes = "rowtypes", coltypes = "coltypes", drop = 0) # %>%
    # No need to convert to factors. R4.0.0 has stringsAsFactors = FALSE by default.
    # dplyr::mutate(
    #   # The original tidy data frame had factors
    #   row = as.factor(row),
    #   col = as.factor(col)
    # )
  expect_equal(tidy2, tidy)

  # Try the test when we are missing the rowtype and coltype columns
  tidy_trimmed <- tidy %>%
    dplyr::mutate(
      rowtypes = NULL,
      coltypes = NULL
    )
  mats_trimmed <- collapse_to_matrices(tidy %>% dplyr::group_by(matrix),
                                       matnames = "matrix", matvals = "vals",
                                       rownames = "row", colnames = "col",
                                       rowtypes = NULL, coltypes = NULL)
  # Test for V1
  expect_equal(mats_trimmed$vals[[1]], matrix(c(1, 2, 0, 3), nrow = 2, ncol = 2, byrow = TRUE,
                                              dimnames = list(c("i1", "i2"), c("p1", "p2"))))
  # Test for V2
  expect_equal(mats_trimmed$vals[[2]], matrix(c(4, 0, 0, 5), nrow = 2, ncol = 2, byrow = TRUE,
                                              dimnames = list(c("i1", "i2"), c("p1", "p2"))))
})


test_that("collapse_to_matrices() works as expected", {
  ptype <- "Products"
  itype <- "Industries"
  tidy <- data.frame(Country  = c("GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
                     Year     = c(1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
                     matrix   = c("U",  "U",  "Y",  "Y",  "Y",  "V",  "V",  "U",  "U",  "Y",  "Y", "eta", "eta"),
                     row      = c("p1", "p2", "p1", "p2", "p2", "i1", "i2", "p1", "p1", "p1", "p2",   NA,    NA),
                     col      = c("i1", "i2", "i1", "i2", "i3", "p1", "p2", "i1", "i2", "i1", "i2",   NA,    NA),
                     rowtypes = c(ptype, ptype, ptype, ptype, ptype, itype, itype, ptype, ptype, ptype, ptype, NA, NA),
                     coltypes = c(itype, itype, itype, itype, itype, ptype, ptype, itype, itype, itype, itype, NA, NA),
                     vals     = c(11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,   11 ,   12 ,   11 ,   22,   0.2, 0.3),
                     stringsAsFactors = FALSE) %>%
    dplyr::group_by(Country, Year, matrix)
  mats <- collapse_to_matrices(tidy, matnames = "matrix", matvals = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = "rowtypes", coltypes = "coltypes")
  A <- matrix(c(11, 0,
                0, 22),
              nrow = 2, ncol = 2, byrow = TRUE,
              dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>%
    matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries")

  # Check that the single values turned out OK
  expect_equal((mats %>% dplyr::filter(Country == "GH", matrix == "eta"))$vals[[1]], 0.2 )
  expect_equal((mats %>% dplyr::filter(Country == "US", matrix == "eta"))$vals[[1]], 0.3 )

  # Check that GH U turned out OK
  expect_equal((mats %>% dplyr::filter(Country == "GH", matrix == "U"))$vals[[1]], A)
  # Check that US U turned out OK
  expect_equal((mats %>% dplyr::filter(Country == "US", matrix == "U"))$vals[[1]],
               matrix(c(11, 12),
                      nrow = 1, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1"), c("i1", "i2"))) %>%
                 matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries"))
  # Check that GH V turned out OK
  expect_equal((mats %>% dplyr::filter(Country == "GH", matrix == "V"))$vals[[1]], A %>% matsbyname::transpose_byname())
  # Check that GH Y turned out OK
  expect_equal((mats %>% dplyr::filter(Country == "GH", matrix == "Y"))$vals[[1]],
               matrix(c(11, 0, 0,
                        0, 22, 23),
                      nrow = 2, ncol = 3, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2", "i3"))) %>%
                 matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries"))
  # Check that US Y turned out OK
  expect_equal((mats %>% dplyr::filter(Country == "US", matrix == "Y"))$vals[[1]], A)
  # Check that groups are discarded.
  expect_equal(length(dplyr::group_vars(mats)), 0)
})


test_that("collapse_to_matrices() works correctly when row and col types are NULL", {
  tidy <- tibble::tibble(matrix = c("V1", "V1", "V1", "V2", "V2"),
                         row = c("i1", "i1", "i2", "i1", "i2"),
                         col = c("p1", "p2", "p2", "p1", "p2"),
                         vals = c(1, 2, 3, 4, 5))

  mats <- collapse_to_matrices(tidy %>% dplyr::group_by(matrix),
                               matnames = "matrix", matvals = "vals",
                               rownames = "row", colnames = "col",
                               rowtypes = NULL, coltypes = NULL)

  expect_null(mats$vals[[1]] %>% matsbyname::rowtype())
  expect_null(mats$vals[[1]] %>% matsbyname::coltype())
  expect_null(mats$vals[[2]] %>% matsbyname::rowtype())
  expect_null(mats$vals[[2]] %>% matsbyname::coltype())

  # Now rely on the new default expressions for rowtypes and coltypes.
  mats2 <- collapse_to_matrices(tidy %>% dplyr::group_by(matrix),
                               matnames = "matrix", matvals = "vals",
                               rownames = "row", colnames = "col")

  expect_null(mats2$vals[[1]] %>% matsbyname::rowtype())
  expect_null(mats2$vals[[1]] %>% matsbyname::coltype())
  expect_null(mats2$vals[[2]] %>% matsbyname::rowtype())
  expect_null(mats2$vals[[2]] %>% matsbyname::coltype())

})


test_that("new defaults for rowtypes and coltypes arguments work as expected", {
  tidy <- data.frame(Country  = c("GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
                     Year     = c(1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
                     matrix   = c("U",  "U",  "Y",  "Y",  "Y",  "V",  "V",  "U",  "U",  "Y",  "Y", "eta", "eta"),
                     row      = c("p1", "p2", "p1", "p2", "p2", "i1", "i2", "p1", "p1", "p1", "p2",   NA,    NA),
                     col      = c("i1", "i2", "i1", "i2", "i3", "p1", "p2", "i1", "i2", "i1", "i2",   NA,    NA),
                     vals     = c(11  ,  22,    11 ,   22 ,   23 ,   11 ,   22 ,   11 ,   12 ,   11 ,   22,   0.2, 0.3)) %>%
    dplyr::group_by(Country, Year, matrix)
  # Do not specify the rowtypes or coltypes arguments.
  # They should default to NULL.
  mats <- collapse_to_matrices(tidy, matnames = "matrix", matvals = "vals",
                               rownames = "row", colnames = "col") %>%
    tidyr::pivot_wider(names_from = matrix, values_from = vals)
  expect_null(mats$U[[1]] |> matsbyname::rowtype())
  expect_null(mats$U[[2]] |> matsbyname::rowtype())
  expect_null(mats$V[[1]] |> matsbyname::rowtype())
  expect_null(mats$V[[2]])
  expect_null(mats$Y[[1]] |> matsbyname::rowtype())
  expect_null(mats$Y[[2]] |> matsbyname::rowtype())
})


test_that("collapse_to_matrices() works with various matnames arguments", {
  tidy <- tibble::tibble(row = c("i1", "i1", "i2"),
                         col = c("p1", "p2", "p2"),
                         vals = c(1, 2, 3))

  # Try wtih NULL
  mats <- collapse_to_matrices(tidy, matnames = NULL,
                               matvals = "vals", rownames = "row", colnames = "col")
  expect_equal(mats$vals[[1]], matrix(c(1, 2,
                                        0, 3), byrow = TRUE, nrow = 2, ncol = 2,
                                      dimnames = list(c("i1", "i2"), c("p1", "p2"))))

  # Try with unspecified
  mats2 <- collapse_to_matrices(tidy, matvals = "vals", rownames = "row", colnames = "col")
  expect_equal(mats2$vals[[1]], matrix(c(1, 2,
                                         0, 3), byrow = TRUE, nrow = 2, ncol = 2,
                                       dimnames = list(c("i1", "i2"), c("p1", "p2"))))
})
