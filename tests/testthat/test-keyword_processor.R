test_that("processor works", {
  expect_silent(processor <- keyword_processor$new())
  # New
  expect_error(keyword_processor$new(ignore_case = TRUE, dict = c("a", "b")))
  expect_warning(keyword_processor$new(ignore_case = TRUE, dict = list(NY = "New York")))
  # Show
  expect_equal(processor$show_attrs(attrs = "dict_size"), 0)
  expect_equal(processor$show_attrs(attrs = c("dict_size", "size")), 0)
  # Add
  expect_silent(processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles")))
  expect_warning(processor$add_keys_words(keys = "NY", words = "New York City"))
  # Contain
  expect_equal(processor$contain_keys(keys = c("LA", "CA", "NY")), c(TRUE, FALSE, TRUE))
  # Get
  expect_equal(processor$get_words(keys = c("LA", "CA", "NY")), c("Los Angeles", NA_character_, "New York City"))
  # Find
  expect_equal(processor$find_keys(sentence = "I live in LA but I like NY", span_info = FALSE),
               list(list(word = "Los Angeles"), list(word = "New York City")))
  # Replace
  expect_equal(processor$replace_keys(sentence = "I live in LA but I like NY"), "I live in Los Angeles but I like New York City")
})
