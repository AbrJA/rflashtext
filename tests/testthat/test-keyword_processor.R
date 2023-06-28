test_that("old_processor works", {
  old_processor <- keyword_processor$new()
  expect_message(keyword_processor$new())
  # New
  expect_error(keyword_processor$new(ignore_case = TRUE, dict = c("a", "b")))
  expect_warning(keyword_processor$new(ignore_case = TRUE, dict = list(NY = "New York")))
  # Show
  expect_identical(old_processor$show_attrs(attrs = "dict_size"), 0L)
  expect_identical(old_processor$show_attrs(attrs = c("dict_size", "size")), 0L)
  # Add
  expect_silent(old_processor$add_keys_words(keys = c("NY", "LA", "N Y"), words = c("New York", "Los Angeles", "New York")))
  expect_warning(old_processor$add_keys_words(keys = "NY", words = "New York City"))
  # Contain
  expect_identical(old_processor$contain_keys(keys = c("LA", "CA", "NY")), c(TRUE, FALSE, TRUE))
  # Get
  expect_identical(old_processor$get_words(keys = c("LA", "CA", "NY")), c("Los Angeles", NA_character_, "New York City"))
  # Find
  expect_identical(old_processor$find_keys(sentence = "I live in LA but I like NY", span_info = FALSE),
               list(list(word = "Los Angeles"), list(word = "New York City")))
  # Replace
  expect_identical(old_processor$replace_keys(sentence = "I live in LA but I like NY"), "I live in Los Angeles but I like New York City")
  expect_identical(old_processor$replace_keys(sentence = "I live in LA but I like N Y"), "I live in Los Angeles but I like New York")
})
