#' @importFrom R6 R6Class
#' @name KeywordProcessor
#' @title FlashText algorithm to find and replace words
#' @description Based on the python library \href{https://github.com/vi3k6i5/flashtext}{flashtext}. To see more details about the algorithm visit: \href{https://arxiv.org/abs/1711.00046}{FlashText}
#'
#' @export
#'
#' @examples
#' library(rflashtext)
#'
#' processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
#'
#' processor$contain_keys(keys = "NY")
#' processor$get_words(keys = "LA")
#'
#' processor$find_keys(sentences = "I live in LA but I like NY")
#' processor$replace_keys(sentences = "I live in LA but I like NY")
KeywordProcessor <- R6::R6Class(
  classname = "KeywordProcessor",
  public = list(
    #-----------------------------------------------------------------
    #' @field attrs list. Stores the attributes of the `KeywordProcessor` object.
    #'
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' processor$attrs
    attrs = list(
      id = NULL,
      ignore_case = NULL,
      word_chars = NULL,
      trie = NULL),
    #-----------------------------------------------------------------
    #' @description Initializes the `KeywordProcessor` object.
    #' @param id character. Used to name the end nodes of the `trie` dictionary.
    #' @param ignore_case logical. If `FALSE` the search is case sensitive. Default `TRUE`.
    #' @param word_chars character. Used to validate if a word continues. Default `paste0(c(letters, LETTERS, 0:9, "_"), collapse = "")` equivalent to `[a-zA-Z0-9_]`.
    #' @param keys character vector. Strings to identify (find/replace) in the text. Must be provided if `trie` is `NULL`.
    #' @param words character vector. Strings to be returned (find) or replaced (replace) when found the respective `keys`. Should have the same length as `keys`. If not provided, `words = keys`.
    #' @param trie character. JSON built character by character and needed for the search. It can be provided instead of `keys` and `words`.
    #'
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(word_chars = paste0(letters, collapse = ""), keys = c("NY", "LA"))
    #' processor$attrs
    initialize = function(id = "_word_",
                          ignore_case = FALSE,
                          word_chars = paste0(c(letters, LETTERS, 0:9, "_"), collapse = ""),
                          keys = NULL,
                          words = NULL,
                          trie = NULL) {
      if (is.null(keys) + is.null(trie) != 1L) stop("Use one argument `keys` or `trie`")
      if (is.null(trie) + is.null(words) == 0L) warning("`words` won't be used when `trie` is not null")
      stopifnot(is.character(id), !identical(id, NA_character_), length(id) == 1, !identical(id, ""))
      self$attrs$id <- id
      stopifnot(is.logical(ignore_case), !identical(ignore_case, NA), length(ignore_case) == 1)
      self$attrs$ignore_case <- ignore_case
      stopifnot(is.character(word_chars), !identical(word_chars, NA_character_), length(word_chars) == 1, !identical(word_chars, ""))
      self$attrs$word_chars <- word_chars
      stopifnot(is.null(trie) || is.character(trie))
      if (!is.null(trie)) {
        self$attrs$trie <- loadTrie(trie)
      } else {
        stopifnot(is.character(keys), length(keys) > 0)
        if (!is.null(words)) {
          stopifnot(is.character(words), length(keys) == length(words))
        } else {
          words <- keys
        }
        if (ignore_case) {
          keys <- tolower(keys)
        }
        self$attrs$trie <- buildTrie(keys, words, id)
      }
    },
    #-----------------------------------------------------------------
    #' @description Shows the `trie` dictionary used to find/replace `keys`.
    #' @return character. JSON string of the `trie` structure. It can be converted to list using `jsonlite::fromJSON`.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' processor$show_trie()
    show_trie = function() {
      if (is.null(self$attrs$trie)) stop("Create a trie dictionary first")
      dumpTrie(self$attrs$trie)
    },
    #-----------------------------------------------------------------
    #' @description Adds `keys` and `words` to the `trie` dictionary.
    #' @param keys character vector. Strings to identify (find/replace) in the text.
    #' @param words character vector. Strings to be returned (find) or replaced (replace) when found the respective `keys`. Should have the same length as `keys`. If not provided, `words = keys`.
    #'
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' processor$add_keys_words(keys = "CA", words = "California")
    #' processor$show_trie()
    add_keys_words = function(keys, words = NULL) {
      stopifnot(is.character(keys), length(keys) > 0)
      if (!is.null(words)) {
        stopifnot(is.character(words), length(keys) == length(words))
      } else {
        words <- keys
      }
      if (self$attrs$ignore_case) {
        keys <- tolower(keys)
      }
      invisible(addKeysWords(self$attrs$trie, keys, words, self$attrs$id))
    },
    #-----------------------------------------------------------------
    #' @description Checks if `keys` are in the `trie` dictionary.
    #' @param keys character vector. Strings to check if already are in the search `trie` dictionary.
    #'
    #' @return logical vector. `TRUE` if the `keys` are in the search `trie` dictionary.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' processor$contain_keys(keys = c("NY", "LA", "TX"))
    contain_keys = function(keys) {
      if (is.null(self$attrs$trie)) stop("Create a trie dictionary first")
      stopifnot(is.character(keys), length(keys) > 0)
      if (self$attrs$ignore_case) {
        keys <- tolower(keys)
      }
      containKeys(self$attrs$trie, keys, self$attrs$id)
    },
    #-----------------------------------------------------------------
    #' @description Gets the `words` for the `keys` found in the `trie` dictionary.
    #' @param keys character vector. Strings to get back the respective `words`.
    #'
    #' @return character vector. Respective `words`. If `keys` not found returns `NA_character_`.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' processor$get_words(keys = c("NY", "LA", "TX"))
    get_words = function(keys) {
      if (is.null(self$attrs$trie)) stop("Create a trie dictionary first")
      stopifnot(is.character(keys), length(keys) > 0)
      if (self$attrs$ignore_case) {
        keys <- tolower(keys)
      }
      getWords(self$attrs$trie, keys, self$attrs$id)
    },
    #-----------------------------------------------------------------
    #' @description
    #' @param sentences character vector. Text to find the `keys` previously defined.
    #' @param span_info logical. `TRUE` to retrieve the `words` and the position of the matches. `FALSE` to only retrieve the `words`. Default `TRUE`.
    #' @param word_chars character. Used to validate if a word continues. Default `paste0(c(letters, LETTERS, 0:9, "_"), collapse = "")` equivalent to `[a-zA-Z0-9_]`.
    #' @return list with the `words` corresponding to `keys` found in the `sentence`. Hint: Use `data.table::rbindlist(...)` to transform the list to a data frame.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' words_found <- processor$find_keys(sentences = "I live in LA but I like NY")
    #' words_found
    find_keys = function(sentences, span_info = TRUE) {
      if (is.null(self$attrs$trie)) stop("Create a trie dictionary first")
      stopifnot(is.character(sentences), length(sentences) > 0)
      if (self$attrs$ignore_case) {
        sentences <- tolower(sentences)
      }
      findKeys(self$attrs$trie, sentences, self$attrs$word_chars, self$attrs$id, span_info)
    },
    #-----------------------------------------------------------------
    #' @description
    #' @param sentences character vector. Text to replace the `keys` found by the corresponding `words`.
    #'
    #' @return character vector. Text with the `keys` replaced by the respective `words`.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' new_sentences <- processor$replace_keys(sentences = "I live in LA but I like NY")
    #' new_sentences
    replace_keys = function(sentences) {
      if (is.null(self$attrs$trie)) stop("Create a trie dictionary first")
      stopifnot(is.character(sentences), length(sentences) > 0)
      replaceKeys(self$attrs$trie, sentences, self$attrs$word_chars, self$attrs$id)
    }
    #-----------------------------------------------------------------
  ),
  cloneable = FALSE
)

#-----------------------------------------------------------------
#' @title build_trie
#' @param keys character vector. Strings to identify (find/replace) in the text.
#' @param words character vector. Strings to be returned (find) or replaced (replace) when found the respective `keys`. Should have the same length as `keys`. If not provided, `words = keys`.
#' @param id character.
#'
#' @return pointer. Pointer to the trie structure.
#' @export
#'
#' @examples
#' library(rflashtext)
#'
#' trie <- build_trie(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
#' trie
build_trie = function(keys, words = NULL, id = "_word_") {
  stopifnot(is.character(keys), length(keys) > 0)
  if (!is.null(words)) {
    stopifnot(is.character(words), length(keys) == length(words))
  } else {
    words <- keys
  }
  buildTrie(keys, words, id)
}

#-----------------------------------------------------------------
#' @title find_keys
#' @param trie pointer. Pointer to a trie structure.
#' @param sentences character vector. Text to find the `keys` previously defined.
#' @param word_chars character. Used to validate if a word continues. Default `paste0(c(letters, LETTERS, 0:9, "_"), collapse = "")` equivalent to `[a-zA-Z0-9_]`.
#' @param id character.
#' @param span_info logical. `TRUE` to retrieve the `words` and the position of the matches. `FALSE` to only retrieve the `words`. Default `TRUE`.
#'
#' @return list with the `words` corresponding to `keys` found in the `sentence`. Hint: Use `data.table::rbindlist(...)` to transform the list to a data frame.
#' @export
#'
#' @examples
#' library(rflashtext)
#'
#' trie <- build_trie(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
#' find_keys(trie, sentence = "I live in LA but I like NY")
find_keys = function(trie, sentences, word_chars = paste0(c(letters, LETTERS, 0:9, "_"), collapse = ""), id = "_word_", span_info = TRUE) {
  if (is.null(trie)) stop("Create a trie dictionary first")
  stopifnot(is.character(sentences), length(sentences) > 0)
  findKeys(trie, sentences, word_chars, id, span_info)
}

#-----------------------------------------------------------------
#' @title replace_keys
#' @param trie pointer. Pointer to a trie structure.
#' @param sentences character vector. Text to replace the `keys` found by the corresponding `words`.
#' @param word_chars character. Used to validate if a word continues. Default `paste0(c(letters, LETTERS, 0:9, "_"), collapse = "")` equivalent to `[a-zA-Z0-9_]`.
#' @param id character.
#'
#' @return character vector. Text with the `keys` replaced by the respective `words`.
#' @export
#'
#' @examples
#' library(rflashtext)
#'
#' trie <- build_trie(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
#' replace_keys(trie, sentence = "I live in LA but I like NY")
replace_keys = function(trie, sentences, word_chars = paste0(c(letters, LETTERS, 0:9, "_"), collapse = ""), id = "_word_") {
  if (is.null(trie)) stop("Create a trie dictionary first")
  stopifnot(is.character(sentences), length(sentences) > 0)
  replaceKeys(trie, sentences, word_chars, id)
}
