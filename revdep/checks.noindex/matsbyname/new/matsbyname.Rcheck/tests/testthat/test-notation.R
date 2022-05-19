# Contains tests for notation functions in the byname package.



test_that("switch_notation_byname() works as expected", {
  # Switch row names
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a [b]", "c [d]"), c("e", "f"))) %>% 
    setrowtype("Industries [Products]") %>% setcoltype("cols")
  e <- m
  rownames(e) <- c("a -> b", "c -> d")
  e <- e %>% setrowtype("Industries -> Products")
  expect_equal(switch_notation_byname(m, margin = 1, from = RCLabels::bracket_notation, to = RCLabels::arrow_notation), e)
  
  # Switch column names
  m2 <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]"))) %>% 
    setrowtype("rows") %>% setcoltype("Industries [Products]")
  e2 <- m2
  colnames(e2) <- c("d -> c", "f -> e")
  e2 <- e2 %>% setcoltype("Industries -> Products")
  expect_equal(switch_notation_byname(m2, margin = 2, from = RCLabels::bracket_notation, to = RCLabels::arrow_notation), e2)

  # Also flip the prefix and suffix. Verify that changes are made in the coltype, too.
  m3 <- matrix(c(1, 2, 
                 3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]"))) %>% 
    setrowtype("rows") %>% setcoltype("Industries [Products]")
  e3 <- m3
  colnames(e3) <- c("c -> d", "e -> f")
  e3 <- e3 %>% setcoltype("Products -> Industries")
  expect_equal(switch_notation_byname(m3, from = RCLabels::bracket_notation, to = RCLabels::arrow_notation, flip = TRUE), e3)
  
  # Switch both row and column names
  m4 <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("b [a]", "d [c]"), c("f [e]", "h [g]"))) %>% 
    setrowtype("Products [Industries]") %>% setcoltype("Industries [Products]")
  e4 <- m4
  rownames(e4) <- c("a -> b", "c -> d")
  colnames(e4) <- c("e -> f", "g -> h")
  e4 <- e4 %>% setrowtype("Industries -> Products") %>% setcoltype("Products -> Industries")
  expect_equal(switch_notation_byname(m4, 
                                      from = RCLabels::bracket_notation, 
                                      to = RCLabels::arrow_notation, 
                                      flip = TRUE), 
               e4)
  
  # Try with a list
  expect_equal(switch_notation_byname(list(m4, m4), margin = list(c(1, 2)), 
                                              from = list(RCLabels::bracket_notation), 
                                              to = list(RCLabels::arrow_notation), 
                                              flip = TRUE), 
               list(e4, e4))
  
  # Try degenerate case of no row and column types.
  m5 <- matrix(c(1, 2, 
                 3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a -> b", "c -> d"), c("c1", "c2")))
  
  expected <- m5
  rownames(expected) <- c("b [a]", "d [c]")
  actual <- switch_notation_byname(m5, from = RCLabels::arrow_notation, to = RCLabels::bracket_notation, flip = TRUE)
  expect_equal(rownames(actual), rownames(expected))
  expect_null(rowtype(actual))
  expect_null(coltype(actual))
  expect_equal(actual, expected)
})


test_that("switch_notation_byname() works well when flip is a list", {
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]"))) %>% 
    setrowtype("rows") %>% setcoltype("Industries [Products]")
  e <- m
  colnames(e) <- c("c -> d", "e -> f")
  e <- e %>% setcoltype("Products -> Industries")
  expect_equal(switch_notation_byname(m, from = RCLabels::bracket_notation, to = RCLabels::arrow_notation, flip = list(TRUE)), e)
})




