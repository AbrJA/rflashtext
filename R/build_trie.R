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
