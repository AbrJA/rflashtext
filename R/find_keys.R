#-----------------------------------------------------------------
#' @title Find keys
#' @description Finds `keys` in the sentences using the search `trie` dictionary.
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
