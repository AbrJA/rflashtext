test_that("processor works", {
  # Build processor
  processor <- KeywordProcessor$new(c("NY", "LA"), c("New York City", "Los Angeles"))
  trie <- processor$show_trie()
  new_processor <- KeywordProcessor$new(trie = trie)
  # New
  expect_error(KeywordProcessor$new())
  expect_error(KeywordProcessor$new(c("NY", "LA"), trie = trie))
  expect_warning(KeywordProcessor$new(words = c("NY", "LA"), trie = trie))
  expect_silent(KeywordProcessor$new(c("NY", "LA")))
  expect_silent(KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York City", "Los Angeles")))
  expect_silent(KeywordProcessor$new(trie = trie))
  expect_identical(processor$show_trie(), new_processor$show_trie())
  ## id
  expect_error(KeywordProcessor$new(c("NY", "LA"), id = 1))
  expect_error(KeywordProcessor$new(c("NY", "LA"), id = NA_character_))
  expect_error(KeywordProcessor$new(c("NY", "LA"), id = character()))
  expect_error(KeywordProcessor$new(c("NY", "LA"), id = character(1)))
  expect_error(KeywordProcessor$new(c("NY", "LA"), id = character(2)))
  expect_error(KeywordProcessor$new(c("NY", "LA"), id = "a"))
  expect_silent(KeywordProcessor$new(c("NY", "LA"), id = "end"))
  ## chars
  expect_error(KeywordProcessor$new(c("NY", "LA"), chars = 1))
  expect_error(KeywordProcessor$new(c("NY", "LA"), chars = NA_character_))
  expect_error(KeywordProcessor$new(c("NY", "LA"), chars = character()))
  expect_error(KeywordProcessor$new(c("NY", "LA"), chars = character(1)))
  expect_error(KeywordProcessor$new(c("NY", "LA"), chars = character(2)))
  expect_silent(KeywordProcessor$new(c("NY", "LA"), chars = paste0(letters, collapse = "")))
  ## ignore_case
  expect_error(KeywordProcessor$new(c("NY", "LA"), ignore_case = 1))
  expect_error(KeywordProcessor$new(c("NY", "LA"), ignore_case = NA))
  expect_error(KeywordProcessor$new(c("NY", "LA"), ignore_case = logical()))
  expect_error(KeywordProcessor$new(c("NY", "LA"), ignore_case = logical(2)))
  ## trie
  expect_error(KeywordProcessor$new(trie = 1))
  expect_error(KeywordProcessor$new(trie = NA_character_))
  expect_error(KeywordProcessor$new(trie = character()))
  expect_error(KeywordProcessor$new(trie = character(1)))
  expect_error(KeywordProcessor$new(trie = character(2)))
  expect_error(KeywordProcessor$new(trie = "a"))
  expect_error(KeywordProcessor$new(trie = "json"))
  expect_silent(KeywordProcessor$new(trie = "{}"))
  ## keys - words
  expect_error(KeywordProcessor$new(1:10))
  expect_error(KeywordProcessor$new(letters, letters[-1]))
  expect_error(KeywordProcessor$new(letters, seq_along(letters)))
  expect_silent(KeywordProcessor$new(NA_character_))
  # Show
  expect_silent(processor$show_trie())
  expect_identical(trie, new_processor$show_trie())
  # Add
  expect_error(processor$add_keys_words(1:10))
  expect_error(processor$add_keys_words(character()))
  expect_invisible(processor$add_keys_words(character(1)))
  expect_identical(trie, processor$show_trie())
  expect_error(processor$add_keys_words(c("CA", "TX"), c("California")))
  expect_silent(processor$add_keys_words(c("CA", "TX")))
  expect_silent(processor$add_keys_words(c("N Y", "CA"), c("New York", "California")))
  # Contain
  expect_error(processor$contain_keys(1))
  expect_error(processor$contain_keys(character()))
  expect_identical(processor$contain_keys(character(1)), FALSE)
  expect_identical(processor$contain_keys(NA_character_), FALSE)
  expect_identical(processor$contain_keys(keys = c("LA", "CA", "NY", "MX")), c(TRUE, TRUE, TRUE, FALSE))
  # Get
  expect_error(processor$get_words(1))
  expect_error(processor$get_words(character()))
  expect_identical(processor$get_words(character(1)), NA_character_)
  expect_identical(processor$get_words(NA_character_), NA_character_)
  expect_identical(processor$get_words(keys = c("LA", "CA", "NY", "MX")), c("Los Angeles", "California", "New York City", NA_character_))
  # Find
  expect_error(processor$find_keys(sentences = 1))
  expect_error(processor$find_keys(sentences = character()))
  expect_identical(processor$find_keys(sentences = character(1)), list(list(word = character(), start = integer(), end = integer())))
  expect_identical(processor$find_keys(sentences = NA_character_, span_info = FALSE), list(list(word = character())))
  expect_identical(processor$find_keys(sentences = "I live in LA but I like NY", span_info = FALSE),
                   list(list(word = c("Los Angeles", "New York City"))))
  expect_identical(processor$find_keys(sentences = c("I live in LA but I like N Y", "Have you been in CA?"), span_info = FALSE),
                   list(list(word = c("Los Angeles", "New York")), list(word = c("California"))))
  expect_identical(processor$find_keys(sentences = c("I live in LA but I like N Y", "Have you been in CA?")),
                   list(list(word = c("Los Angeles", "New York"), start = c(11L, 25L), end = c(12L, 27L)),
                        list(word = c("California"), start = c(18L), end = c(19L))))
  # Replace
  expect_error(processor$replace_keys(sentences = 1))
  expect_error(processor$replace_keys(sentences = character()))
  expect_identical(processor$replace_keys(sentences = character(1)), character(1))
  expect_identical(processor$replace_keys(sentences = NA_character_), NA_character_)
  expect_identical(processor$replace_keys(sentences = character(2)), character(2))
  expect_identical(processor$replace_keys(sentences = c("I live in LA but I like NY", "I live in LA but I like N Y")),
                   c("I live in Los Angeles but I like New York City", "I live in Los Angeles but I like New York"))
  # Advances use
  new_processor <- KeywordProcessor$new(NA_character_)
  expect_identical(new_processor$show_trie(), "null")
  added <- new_processor$add_keys_words("NY")
  expect_identical(added, 1L)
  expect_identical(new_processor$show_trie(), "{\"N\":{\"Y\":{\"_word_\":\"NY\"}}}")
})
