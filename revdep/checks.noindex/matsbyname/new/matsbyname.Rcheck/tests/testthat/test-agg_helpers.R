test_that("agg_table_to_agg_map() works as expected", {
  bands <- tibble::tribble(~few_col, ~many_col, 
                           "The Beatles", "John", 
                           "The Beatles", "Paul", 
                           "The Beatles", "George", 
                           "The Beatles", "Ringo", 
                           # Test duplicates and NA
                           "The Beatles", "Ringo",
                           "The Beatles", NA, 
                           "Rolling Stones", "Mick", 
                           "Rolling Stones", "Keith",
                           "Rolling Stones", "Ronnie",
                           "Rolling Stones", "Bill",
                           "Rolling Stones", "Charlie")
  res <- agg_table_to_agg_map(bands, 
                               few_colname = "few_col",
                               many_colname = "many_col")
  expect_equal(res, list(`Rolling Stones` = c("Mick", "Keith", "Ronnie", "Bill", "Charlie"),
                         `The Beatles` = c("John", "Paul", "George", "Ringo")))
})


test_that("agg_map_to_agg_table() works as expected", {
  agg_map <- list(`The Beatles` = c("John", "Paul", "George", "Ringo"), 
                  `Rolling Stones` = c("Mick", "Keith", "Ronnie", "Bill", "Charlie"))
  res <- agg_map_to_agg_table(agg_map, few_colname = "band", many_colname = "member")
  expect_equal(res, 
               tibble::tribble(~band, ~member, 
                               "The Beatles", "John", 
                               "The Beatles", "Paul", 
                               "The Beatles", "George", 
                               "The Beatles", "Ringo", 
                               "Rolling Stones", "Mick", 
                               "Rolling Stones", "Keith", 
                               "Rolling Stones", "Ronnie", 
                               "Rolling Stones", "Bill", 
                               "Rolling Stones", "Charlie"))
})
