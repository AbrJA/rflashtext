test_that("multiplication works", {
  processor <- keyword_processor$new()
  expect_equal(processor$set_key_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"), info = TRUE), c(TRUE, TRUE))
  expect_equal(processor$get_words(keys = "LA"), "Los Angeles")
  expect_equal(processor$get_words(keys = "CAL"), NA_character_)
})
