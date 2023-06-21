#-----------------------------------------------------------------
#' @title Replace keys
#' @description Replaces `keys` found in the sentences by the corresponding `words`.
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
