test_that("processor works", {
  processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York City", "Los Angeles"))
  expect_silent(KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York City", "Los Angeles")))
  # New
  # expect_error(KeywordProcessor$new(ignore_case = TRUE, trie = c("a", "b")))
  # expect_warning(KeywordProcessor$new(ignore_case = TRUE, trie = list(NY = "New York")))
  # Show
  # expect_equal(processor$show_attrs(attrs = "trie_size"), 0)
  # expect_equal(processor$show_attrs(attrs = c("trie_size", "size")), 0)
  # Add
  expect_silent(processor$add_keys_words(keys = c("CA", "TX"), words = c("California", "Texas")))
  # expect_warning(processor$add_keys_words(keys = "NY", words = "New York City"))
  # Contain
  expect_equal(processor$contain_keys(keys = c("LA", "CA", "NY", "MX")), c(TRUE, TRUE, TRUE, FALSE))
  # Get
  expect_equal(processor$get_words(keys = c("LA", "CA", "NY", "MX")), c("Los Angeles", "California", "New York City", NA_character_))
  # Find
  expect_equal(processor$find_keys(sentences = "I live in LA but I like NY", span_info = FALSE),
               list(list(word = c("Los Angeles", "New York City"))))
  # Replace
  expect_equal(processor$replace_keys(sentence = "I live in LA but I like NY"), "I live in Los Angeles but I like New York City")
})
