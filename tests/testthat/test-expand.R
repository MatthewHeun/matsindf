# Contains tests for the matsindf package.


test_that("expand_to_tidy() works as expected", {
  ptype <- "Products"
  itype <- "Industries"
  tidy <- data.frame(Country  = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
                     Year     = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
                     matrix   = c(   "U",   "U",   "Y",   "Y",   "Y",   "V",   "V",   "U",   "U",   "Y",   "Y", "eta", "eta"),
                     row      = c(  "p1",  "p2",  "p1",  "p2",  "p2",  "i1",  "i2",  "p1",  "p1",  "p1",  "p2", NA, NA),
                     col      = c(  "i1",  "i2",  "i1",  "i2",  "i3",  "p1",  "p2",  "i1",  "i2",  "i1",  "i2", NA, NA),
                     rowtypes = c(ptype, ptype, ptype, ptype, ptype, itype, itype, ptype, ptype, ptype, ptype, NA, NA),
                     coltypes = c(itype, itype, itype, itype, itype, ptype, ptype, itype, itype, itype, itype, NA, NA),
                     vals     = c(   11  ,  12,    13 ,   14 ,   15 ,   16 ,   17 ,   49 ,   50 ,   51 ,   52,   0.2, 0.3),
                     stringsAsFactors = FALSE) %>%
    dplyr::group_by(Country, Year, matrix)
  mats <- collapse_to_matrices(tidy, matnames = "matrix", rownames = "row", colnames = "col",
                               rowtypes = "rowtypes", coltypes = "coltypes",
                               matvals = "vals") %>%
    dplyr::ungroup()
  # For the first tests, do not drop 0 values.
  A <- expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
                      rownames = "rows", colnames = "cols",
                      rowtypes = "rt",   coltypes = "ct")
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 11)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i2"))$vals[[1]], 12)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 13)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i3"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 14)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i3"))$vals[[1]], 15)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p1"))$vals[[1]], 16)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p2"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p1"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p2"))$vals[[1]], 17)

  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 49)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 50)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 51)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 52)

  expect_equal((A %>% dplyr::filter(Country == "GH", matrix == "eta"))$vals[[1]], 0.2)
  expect_equal((A %>% dplyr::filter(Country == "US", matrix == "eta"))$vals[[1]], 0.3)

  # For this second set of tests, drop 0 values.
  B <- expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
                      rownames = "rows", colnames = "cols",
                      rowtypes = "rt",   coltypes = "ct",
                      drop = 0)

  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 11)
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i2"))$vals[[1]], 12)
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 13)
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i3"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 14)
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i3"))$vals[[1]], 15)
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p1"))$vals[[1]], 16)
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p2"))$vals[[1]], 17)

  expect_equal((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 49)
  expect_equal((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 50)
  expect_equal((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 51)
  expect_error((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 52)

  expect_equal((B %>% dplyr::filter(Country == "GH", matrix == "eta"))$vals[[1]], 0.2)
  expect_equal((B %>% dplyr::filter(Country == "US", matrix == "eta"))$vals[[1]], 0.3)
})


test_that("expand_to_tidy() works with Matrix objects", {
  ptype <- "Products"
  itype <- "Industries"
  tidy <- data.frame(Country  = c( "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "GH",  "US",  "US",  "US",  "US", "GH", "US"),
                     Year     = c( 1971,  1971,  1971,  1971,  1971,  1971,  1971,  1980,  1980,  1980,  1980, 1971, 1980),
                     matrix   = c(   "U",   "U",   "Y",   "Y",   "Y",   "V",   "V",   "U",   "U",   "Y",   "Y", "eta", "eta"),
                     row      = c(  "p1",  "p2",  "p1",  "p2",  "p2",  "i1",  "i2",  "p1",  "p1",  "p1",  "p2", NA, NA),
                     col      = c(  "i1",  "i2",  "i1",  "i2",  "i3",  "p1",  "p2",  "i1",  "i2",  "i1",  "i2", NA, NA),
                     rowtypes = c(ptype, ptype, ptype, ptype, ptype, itype, itype, ptype, ptype, ptype, ptype, NA, NA),
                     coltypes = c(itype, itype, itype, itype, itype, ptype, ptype, itype, itype, itype, itype, NA, NA),
                     vals     = c(   11  ,  12,    13 ,   14 ,   15 ,   16 ,   17 ,   49 ,   50 ,   51 ,   52,   0.2, 0.3),
                     stringsAsFactors = FALSE) %>%
    dplyr::group_by(Country, Year, matrix)
  mats <- collapse_to_matrices(tidy, matnames = "matrix", rownames = "row", colnames = "col",
                               rowtypes = "rowtypes", coltypes = "coltypes",
                               matvals = "vals",
                               matrix_class = "Matrix") %>%
    dplyr::ungroup()
  # For the first tests, do not drop 0 values.
  A <- expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
                      rownames = "rows", colnames = "cols",
                      rowtypes = "rt",   coltypes = "ct")
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 11)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i2"))$vals[[1]], 12)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 13)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i3"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 14)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i3"))$vals[[1]], 15)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p1"))$vals[[1]], 16)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p2"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p1"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p2"))$vals[[1]], 17)

  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 49)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 50)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 51)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]], 0)
  expect_equal((A %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 52)

  expect_equal((A %>% dplyr::filter(Country == "GH", matrix == "eta"))$vals[[1]], 0.2)
  expect_equal((A %>% dplyr::filter(Country == "US", matrix == "eta"))$vals[[1]], 0.3)

  # For this second set of tests, drop 0 values.
  B <- expand_to_tidy(mats, matnames = "matrix", matvals = "vals",
                      rownames = "rows", colnames = "cols",
                      rowtypes = "rt",   coltypes = "ct",
                      drop = 0)

  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 11)
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "U", rows == "p2", cols == "i2"))$vals[[1]], 12)
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 13)
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p1", cols == "i3"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 14)
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "Y", rows == "p2", cols == "i3"))$vals[[1]], 15)
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p1"))$vals[[1]], 16)
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i1", cols == "p2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% dplyr::filter(Country == "GH", Year == 1971, matrix == "V", rows == "i2", cols == "p2"))$vals[[1]], 17)

  expect_equal((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i1"))$vals[[1]], 49)
  expect_equal((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "U", rows == "p1", cols == "i2"))$vals[[1]], 50)
  expect_equal((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i1"))$vals[[1]], 51)
  expect_error((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p1", cols == "i2"))$vals[[1]],
               "subscript out of bounds")
  expect_error((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i1"))$vals[[1]],
               "subscript out of bounds")
  expect_equal((B %>% dplyr::filter(Country == "US", Year == 1980, matrix == "Y", rows == "p2", cols == "i2"))$vals[[1]], 52)

  expect_equal((B %>% dplyr::filter(Country == "GH", matrix == "eta"))$vals[[1]], 0.2)
  expect_equal((B %>% dplyr::filter(Country == "US", matrix == "eta"))$vals[[1]], 0.3)
})


test_that("expand_to_tidy() works as expected without rowtype, coltype", {
  m1 <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  m2 <- matsbyname::hadamardproduct_byname(m1, 10)
  df <- data.frame(matnames = c("m1", "m2"), m = I(list(m1, m2)), stringsAsFactors = FALSE)
  class(df$m) <- class(df$m)[-match("AsIs", class(df$m))]
  tidy <- expand_to_tidy(df, matnames = "matrix", matvals = "m", rownames = "row", colnames = "col") %>%
    as.data.frame()
  expected_df <- data.frame(
    matnames = c(rep("m1", 4), rep("m2", 4)),
    row = rep(c("r1", "r2"), 4),
    col = c(rep("c1", 2), rep("c2", 2)),
    m = c(1, 3, 2, 4, 10, 30, 20, 40)
  ) %>%
    dplyr::mutate(
      row = as.character(row),
      col = as.character(col)
    )
  expect_equal(tidy, expected_df)
})


test_that("expand_to_tidy() works as expected without rowtype, coltype for Matrix objects", {
  m1 <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(c("r1", "r2"), c("c1", "c2")))
  m2 <- matsbyname::hadamardproduct_byname(m1, 10)
  df <- data.frame(matnames = c("m1", "m2"), m = I(list(m1, m2)), stringsAsFactors = FALSE)
  class(df$m) <- class(df$m)[-match("AsIs", class(df$m))]
  tidy <- expand_to_tidy(df, matnames = "matrix", matvals = "m", rownames = "row", colnames = "col") %>%
    as.data.frame()
  expected_df <- data.frame(
    matnames = c(rep("m1", 4), rep("m2", 4)),
    row = rep(c("r1", "r2"), 4),
    col = c(rep("c1", 2), rep("c2", 2)),
    m = c(1, 3, 2, 4, 10, 30, 20, 40)
  ) %>%
    dplyr::mutate(
      row = as.character(row),
      col = as.character(col)
    )
  expect_equal(tidy, expected_df)
})


test_that("expand_to_tidy() works with a list of matrices", {
  m1 <- matrix(c(1,2), nrow = 2, ncol = 1, dimnames = list(c("i1", "i2"), "p1")) %>%
    matsbyname::setrowtype("industries") %>% matsbyname::setcoltype("products")
  m2 <- matsbyname::transpose_byname(m1 * 10)
  result <- expand_to_tidy(list(m1 = m1, m2 = m2),
                           matnames = "matnames", matvals = "matvals",
                           rownames = "rownames", colnames = "colnames",
                           rowtypes = "rt", coltypes = "ct")
  expected <- tibble::tibble(
    matnames = c("m1", "m1", "m2", "m2"),
    rownames = c("i1", "i2", "p1", "p1"),
    colnames = c("p1", "p1", "i1", "i2"),
    matvals = c(1, 2, 10, 20),
    rt = c("industries", "industries", "products", "products"),
    ct = c("products", "products", "industries", "industries")
  )
  expect_equal(result, expected)
})


test_that("expand_to_tidy() works with a list of Matrix objects", {
  m1 <- matsbyname::Matrix(c(1,2), nrow = 2, ncol = 1,
                           dimnames = list(c("i1", "i2"), "p1")) %>%
    matsbyname::setrowtype("industries") %>% matsbyname::setcoltype("products")
  m2 <- m1 %>%
    matsbyname::hadamardproduct_byname(10) %>%
    matsbyname::transpose_byname()
  result <- expand_to_tidy(list(m1 = m1, m2 = m2),
                           matnames = "matnames", matvals = "matvals",
                           rownames = "rownames", colnames = "colnames",
                           rowtypes = "rt", coltypes = "ct")
  expected <- tibble::tibble(
    matnames = c("m1", "m1", "m2", "m2"),
    rownames = c("i1", "i2", "p1", "p1"),
    colnames = c("p1", "p1", "i1", "i2"),
    matvals = c(1, 2, 10, 20),
    rt = c("industries", "industries", "products", "products"),
    ct = c("products", "products", "industries", "industries")
  )
  expect_equal(result, expected)
})


test_that("expand_to_tidy() works if some arguments are unspecified", {
  m1 <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  df <- data.frame(m = I(list(m1)))

  tidy <- expand_to_tidy(df, matvals = "m", rownames = "row", colnames = "col")

  expect_equal(tidy, data.frame(row = c("r1", "r2", "r1", "r2"),
                                col = c("c1", "c1", "c2", "c2"),
                                m = c(1, 3, 2 ,4)))
})


test_that("expand_to_tidy() works if some arguments are unspecified with Matrix objects", {
  m1 <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(c("r1", "r2"), c("c1", "c2")))
  df <- data.frame(m = I(list(m1)))

  tidy <- expand_to_tidy(df, matvals = "m", rownames = "row", colnames = "col")

  expect_equal(tidy, data.frame(row = c("r1", "r2", "r1", "r2"),
                                col = c("c1", "c1", "c2", "c2"),
                                m = c(1, 3, 2 ,4)))
})


test_that("expand_to_tidy() works will NULL in a list", {
  mat <- Matrix::Matrix(data = c(1:4),
                        nrow = 2,
                        dimnames = list(c("r1", "r2"),
                                        c("c1", "c2")))
  matlist <- list(m1 = mat, m2 = NULL, NULL, m3 = mat)
  res <- expand_to_tidy(matlist)
  expect_equal(nrow(res), 8)
})


test_that("expand_to_tidy() works with NULL in a data frame", {
  m1 <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(c("r1", "r2"), c("c1", "c2")))
  df <- data.frame(a = c(1, 2, 3), m = I(list(m1, NULL, m1)))

  tidy <- df |>
    expand_to_tidy(matvals = "m", rownames = "row", colnames = "col")
  expected <- tibble::tibble(a = c(1, 1, 1, 1, 3, 3, 3, 3),
                             row = c("r1", "r2", "r1", "r2",
                                     "r1", "r2", "r1", "r2"),
                             col = c("c1", "c1", "c2", "c2",
                                     "c1", "c1", "c2", "c2"),
                             m = c(1, 3, 2, 4,
                                   1, 3, 2, 4))
  expect_equal(tidy, expected)

  df2 <- tibble::tibble(a = c(1, 2, 3), m = list(m1, NULL, m1))
  tidy2 <- df2 |>
    expand_to_tidy(matvals = "m", rownames = "row", colnames = "col")
  expect_equal(tidy2, expected)
})


test_that("expand_to_tidy() returns NULL when all arguments are NULL", {
  matlist <- list(m1 = NULL, m2 = NULL, NULL, m3 = NULL)
  expect_null(expand_to_tidy(matlist))
})


# test_that("expand_to_tidy() is much faster", {
#   # Build a big data frame to collapse into small matrices
#   # n_mats <- 1000
#   n_mats <- 100
#   n_rows_mat <- 3
#   n_cols_mat <- 2
#   df <- data.frame(
#     rownames = paste0("r", 1:n_rows_mat) |>
#       rep(n_cols_mat) |> # in each matrix
#       rep(n_mats), # for all matrices
#     colnames = paste0("c", 1:n_cols_mat) |>
#       rep(n_rows_mat) |> # in each matrix
#       rep(n_mats), # for all matrices
#     matvals = 1:(n_rows_mat*n_cols_mat) |>
#       rep(n_mats),
#     matnames = paste0("m", 1:n_mats) |>
#       rep(n_rows_mat * n_cols_mat) |>
#       sort(),
#     rowtypes = "rtype",
#     coltypes = "ctype"
#   ) |>
#     dplyr::group_by(matnames) |>
#     collapse_to_matrices(matrix_class = "Matrix")
#
#   exec_time_secs <- df |>
#     expand_to_tidy() |>
#     system.time() |>
#     magrittr::extract2("user.self")
#
#   # As of 14 Jan 2024, it takes about 1.4 secs per 1000 matrices.
#   prev_time_per_matrix <- 1.4 / 1000 # seconds/matrix
#   current_time_per_matrix <- exec_time_secs / n_mats
#   speedup <- prev_time_per_matrix / current_time_per_matrix
#   # At this point, I have not been able to speed up the expansion process.
#   expect_true(speedup > 0.9)
# })
