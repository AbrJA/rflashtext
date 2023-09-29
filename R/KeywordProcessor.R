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
      trie = NULL,
      id = NULL,
      chars = NULL,
      ignore_case = NULL),
    #-----------------------------------------------------------------
    #' @description Initializes the `KeywordProcessor` object.
    #' @param keys character vector. Strings to identify (find/replace) in the text. Must be provided if `trie` is `NULL`.
    #' @param words character vector. Strings to be returned (find) or replaced (replace) when found the respective `keys`. Should have the same length as `keys`. If not provided, `words = keys`.
    #' @param trie character. JSON built character by character and needed for the search. It can be provided instead of `keys` and `words`.
    #' @param id character. Used to name the end nodes of the `trie` dictionary.
    #' @param chars character. Used to validate if a word continues. Default `paste0(c(letters, LETTERS, 0:9, "_"), collapse = "")` equivalent to `[a-zA-Z0-9_]`.
    #' @param ignore_case logical. If `FALSE` the search is case sensitive. Default `TRUE`.
    #'
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- KeywordProcessor$new(chars = paste0(letters, collapse = ""), keys = c("NY", "LA"))
    #' processor$attrs
    initialize = function(keys = NULL,
                          words = NULL,
                          trie = NULL,
                          id = "_word_",
                          chars = paste0(c(letters, LETTERS, 0:9, "_"), collapse = ""),
                          ignore_case = FALSE) {
      if (is.null(keys) + is.null(trie) != 1L) stop("Use one argument `keys` or `trie`")
      if (is.null(trie) + is.null(words) == 0L) warning("`words` won't be used when `trie` is not null")
      stopifnot(is.character(id), length(id) == 1, !identical(id, NA_character_), nchar(id) > 1)
      self$attrs$id <- id
      stopifnot(is.logical(ignore_case), length(ignore_case) == 1, !identical(ignore_case, NA))
      self$attrs$ignore_case <- ignore_case
      stopifnot(is.character(chars), length(chars) == 1, !identical(chars, NA_character_), nchar(chars) > 0)
      self$attrs$chars <- chars
      if (is.null(trie)) {
        stopifnot(is.character(keys), length(keys) > 0)
        if (!is.null(words)) stopifnot(is.character(words), length(keys) == length(words)) else words <- keys
        if (ignore_case) keys <- tolower(keys)
        self$attrs$trie <- buildTrie(keys, words, id)
      } else {
        stopifnot(is.character(trie), length(trie) == 1, !identical(trie, NA_character_), nchar(trie) > 1)
        self$attrs$trie <- loadTrie(trie)
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
      if (!is.null(words)) stopifnot(is.character(words), length(keys) == length(words)) else words <- keys
      if (self$attrs$ignore_case) keys <- tolower(keys)
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
      if (self$attrs$ignore_case) keys <- tolower(keys)
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
      if (self$attrs$ignore_case) keys <- tolower(keys)
      getWords(self$attrs$trie, keys, self$attrs$id)
    },
    #-----------------------------------------------------------------
    #' @description Finds `keys` in the sentences using the search `trie` dictionary.
    #' @param sentences character vector. Text to find the `keys` previously defined.
    #' @param span_info logical. `TRUE` to retrieve the `words` and the position of the matches. `FALSE` to only retrieve the `words`. Default `TRUE`.
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
      if (self$attrs$ignore_case) sentences <- tolower(sentences)
      findKeys(self$attrs$trie, sentences, self$attrs$chars, self$attrs$id, span_info)
    },
    #-----------------------------------------------------------------
    #' @description Replaces `keys` found in the sentences by the corresponding `words`.
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
      if (self$attrs$ignore_case) sentences <- tolower(sentences)
      replaceKeys(self$attrs$trie, sentences, self$attrs$chars, self$attrs$id)
    }
    #-----------------------------------------------------------------
  ),
  cloneable = FALSE
)
