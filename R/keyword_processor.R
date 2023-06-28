#' @importFrom R6 R6Class
#' @name keyword_processor
#' @title FlashText algorithm to find and replace words
#' @description Based on the python library \href{https://github.com/vi3k6i5/flashtext}{flashtext}. To see more details about the algorithm visit: \href{https://arxiv.org/abs/1711.00046}{FlashText}
#'
#' @export
#'
#' @examples
#' library(rflashtext)
#'
#' processor <- keyword_processor$new()
#' processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
#'
#' processor$contain_keys(keys = "NY")
#' processor$get_words(keys = "LA")
#'
#' processor$find_keys(sentence = "I live in LA but I like NY")
#' processor$replace_keys(sentence = "I live in LA but I like NY")
keyword_processor <- R6::R6Class(
  classname = "keyword_processor",
  public = list(
    #-----------------------------------------------------------------
    #' @param ignore_case logical. If `FALSE` the search is case sensitive. Default `TRUE`.
    #' @param word_chars character vector. Used to validate if a word continues. Default `c(letters, LETTERS, 0:9, "_")` equivalent to `[a-zA-Z0-9_]`.
    #' @param dict list. Internally built character by character and needed for the search. Recommended to let the default value `NULL`.
    #'
    #' @return invisible. Assign to a variable to inspect the output. Logical. `TRUE` if all went good.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- keyword_processor$new(ignore_case = FALSE, word_chars = letters)
    #' processor
    initialize = function(ignore_case = TRUE, word_chars = c(letters, LETTERS, 0:9, "_"), dict = NULL) {
      stopifnot(is.logical(ignore_case) && length(ignore_case) != 0)
      stopifnot(is.character(word_chars) && !identical(word_chars, "") && !identical(word_chars, NA_character_) && length(word_chars) != 0)
      stopifnot(is.null(dict) || is.list(dict))
      message("Use `KeywordProcessor` instead for better performance")
      initiated <- private$set_attr(id = "_word_", ignore_case = ignore_case, word_chars = word_chars, dict = dict)
      invisible(initiated)
    },
    #' @param attrs character vector. Options are subsets of `c("all", "id", "word_chars", "dict", "ignore_case", "dict_size")`. Default `"all"`.
    #'
    #' @return list with the values of the `attrs`. Useful to save `dict` and reuse it or to check the `dict_size`.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- keyword_processor$new()
    #' processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' processor$show_attrs(attrs = "dict_size")
    #' processor$show_attrs(attrs = "dict")
    show_attrs = function(attrs = "all") {
      if ("all" %in% attrs) {
        return(private$attrs)
      }
      attrs <- attrs[attrs %in% names(private$attrs)]
      if (length(attrs) == 1) {
        return(private$attrs[[attrs]])
      } else {
        return(private$attrs[attrs])
      }
    },
    #' @param keys character vector. Strings to identify (find/replace) in the text.
    #' @param words character vector. Strings to be returned (find) or replaced (replace) when found the respective `keys`. Should have the same length as `keys`. If not provided, `words = keys`.
    #'
    #' @return invisible. Assign to a variable to inspect the output. Logical vector. `FALSE` if `keys` are duplicated, the respective `words` will be updated.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- keyword_processor$new()
    #' processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' correct <- processor$add_keys_words(keys = c("NY", "CA"), words = c("New York City", "California"))
    #' # To check if there are duplicate keys
    #' correct
    add_keys_words = function(keys, words = NULL) {
      stopifnot(is.character(keys) && !identical(keys, "") && !identical(keys, NA_character_) && length(keys) != 0)
      len <- length(keys)
      if (!is.null(words)) {
        stopifnot(is.character(words) && len == length(words))
      } else {
        words <- keys
      }
      if (private$attrs$ignore_case) {
        keys <- tolower(keys)
      }
      added <- vector("logical", len)
      for (k in 1:len) {
        added[k] <- private$add_key_word(key = keys[k], word = words[k])
      }
      if (any(!added)) {
        warning("There are duplicate keys. To a better check assign the output to a variable.")
      }
      invisible(added)
    },
    #' @param keys character vector. Strings to check if already are on the search dictionary.
    #'
    #' @return logical vector. `TRUE` if the `keys` are on the search dictionary.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- keyword_processor$new()
    #' processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' processor$contain_keys(keys = c("NY", "LA", "TX"))
    contain_keys = function(keys) {
      stopifnot(is.character(keys) && !identical(keys, "") && !identical(keys, NA_character_) && length(keys) != 0)
      if (private$attrs$ignore_case) {
        keys <- tolower(keys)
      }
      len <- length(keys)
      contained <- vector("logical", len)
      for (k in 1:len) {
        contained[k] <- private$contain_key(key = keys[k])
      }
      return(contained)
    },
    #' @param keys character vector. Strings to get back the respective `words`.
    #'
    #' @return character vector. Respective `words`. If `keys` not found returns `NA_character_`.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- keyword_processor$new()
    #' processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' processor$get_words(keys = c("NY", "LA", "TX"))
    get_words = function(keys) {
      stopifnot(is.character(keys) && !identical(keys, "") && !identical(keys, NA_character_) && length(keys) != 0)
      if (private$attrs$ignore_case) {
        keys <- tolower(keys)
      }
      len <- length(keys)
      words <- vector("character", len)
      for (k in 1:len) {
        words[k] <- private$get_word(key = keys[k])
      }
      return(words)
    },
    #' @param sentence character. Text to find the `keys` previously defined. Not vectorized.
    #' @param span_info logical. `TRUE` to retrieve the `words` and the position of the matches. `FALSE` to only retrieve the `words`. Default `TRUE`.
    #'
    #' @return list with the `words` corresponding to `keys` found in the `sentence`. Hint: Use `do.call(rbind, ...)` to transform the list to a matrix.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- keyword_processor$new()
    #' processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' words_found <- processor$find_keys(sentence = "I live in LA but I like NY")
    #' do.call(rbind, words_found)
    find_keys = function(sentence, span_info = TRUE) {
      stopifnot(is.character(sentence) && length(sentence) == 1 && !identical(sentence, "") && !identical(sentence, NA_character_))
      if (private$attrs$ignore_case) {
        sentence <- tolower(sentence)
      }
      found <- private$find_key(sentence = sentence, span_info = span_info)
      return(found)
    },
    #' @param sentence character. Text to replace the `keys` found by the corresponding `words`. Not vectorized.
    #'
    #' @return character. Text with the `keys` replaced by the respective `words`.
    #' @export
    #'
    #' @examples
    #' library(rflashtext)
    #'
    #' processor <- keyword_processor$new()
    #' processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
    #' new_sentence <- processor$replace_keys(sentence = "I live in LA but I like NY")
    #' new_sentence
    replace_keys = function(sentence) {
      stopifnot(is.character(sentence) && length(sentence) == 1 && !identical(sentence, "") && !identical(sentence, NA_character_))
      new_sentence <- private$replace_key(sentence = sentence)
      return(new_sentence)
    }
    #-----------------------------------------------------------------
  ),
  private = list(
    #-----------------------------------------------------------------
    attrs = list(
      id = "_word_",
      word_chars = c(letters, LETTERS, 0:9, "_"),
      dict = list("_class_" = "keyword_dictionary"),
      ignore_case = TRUE,
      dict_size = 0L
    ),
    #-----------------------------------------------------------------
    set_attr = function(id, ignore_case, word_chars, dict) {
      if (is.null(dict)) {
        dict <- list("_class_" = "keyword_dictionary")
      } else if (!identical(dict[["_class_"]], "keyword_dictionary")) {
        warning("Invalid dictionary. Using an empty dictionary.")
        dict <- list("_class_" = "keyword_dictionary")
      }
      private$attrs$id <- id
      private$attrs$ignore_case <- ignore_case
      private$attrs$word_chars <- word_chars
      private$attrs$dict <- dict
      return(TRUE)
    },
    #-----------------------------------------------------------------
    add_key_word = function(key, word) {
      status <- FALSE
      dict <- private$attrs$dict
      key <- strsplit(key, split = "", fixed = TRUE)[[1]]
      counter <- 0
      for (letter in key) {
        counter <- counter + 1
        dict <- dict[[letter]]
        if (is.null(dict)) {
          break
        }
      }
      if (is.null(private$attrs$dict[[key[1:counter]]][[private$attrs$id]])) {
        status <- TRUE
        private$attrs$dict_size <- private$attrs$dict_size + 1
      }
      while (counter <= length(key)) {
        if (counter == length(key)) {
          private$attrs$dict[[key[1:counter]]][private$attrs$id] <- list(word)
        } else {
          private$attrs$dict[[key[1:counter]]] <- list()
        }
        counter <- counter + 1
      }
      return(status)
    },
    #-----------------------------------------------------------------
    contain_key = function(key) {
      dict <- private$attrs$dict
      key <- strsplit(key, split = "", fixed = TRUE)[[1]]
      counter <- 0
      for (letter in key) {
        counter <- counter + 1
        dict <- dict[[letter]]
        if (is.null(dict)) {
          break
        }
      }
      return(!is.null(dict[[private$attrs$id]]) && counter == length(key))
    },
    #-----------------------------------------------------------------
    get_word = function(key) {
      dict <- private$attrs$dict
      key <- strsplit(key, split = "", fixed = TRUE)[[1]]
      counter <- 0
      for (letter in key) {
        counter <- counter + 1
        dict <- dict[[letter]]
        if (is.null(dict)) {
          break
        }
      }
      if (!is.null(dict[[private$attrs$id]]) && counter == length(key)) {
        return(dict[[private$attrs$id]])
      }
      return(NA_character_)
    },
    #-----------------------------------------------------------------
    find_key = function(sentence, span_info) {
      words_found <- list()
      dict <- private$attrs$dict
      if (!length(dict)) {
        warning("No key-words in the dictionary. Add keys-words first")
        return(words_found)
      }
      start_pos <- 1
      end_pos <- 1
      reset_dict <- FALSE
      idx <- 1
      sentence <- strsplit(sentence, split = "", fixed = TRUE)[[1]]
      len <- length(sentence)
      counter <- 1
      while (idx <= len) {
        char <- sentence[idx]
        if (!char %in% private$attrs$word_chars) {
          if (!is.null(dict[[private$attrs$id]]) || !is.null(dict[[char]])) {
            sequence <- NULL
            longest_sequence <- NULL
            longer <- FALSE
            inner_char <- "<eof>"
            if (!is.null(dict[[private$attrs$id]])) {
              sequence <- dict[[private$attrs$id]]
              longest_sequence <- dict[[private$attrs$id]]
              end_pos <- idx
            }
            if (!is.null(dict[[char]])) {
              dict_cont <- dict[[char]]
              idy <- idx + 1
              while (idy <= len) {
                inner_char <- sentence[idy]
                if (!inner_char %in% private$attrs$word_chars && !is.null(dict_cont[[private$attrs$id]])) {
                  longest_sequence <- dict_cont[[private$attrs$id]]
                  end_pos <- idy
                  longer <- TRUE
                }
                if (!is.null(dict_cont[[inner_char]])) {
                  dict_cont <- dict_cont[[inner_char]]
                } else {
                  break
                }
                idy <- idy + 1
              }
              if ((idy > len || !inner_char %in% private$attrs$word_chars) && !is.null(dict_cont[[private$attrs$id]])) {
                longest_sequence <- dict_cont[[private$attrs$id]]
                end_pos <- idy
                longer <- TRUE
              }
              if (longer) {
                idx <- end_pos
              }
            }
            dict <- private$attrs$dict
            if (!is.null(longest_sequence)) {
              if (span_info) {
                words_found[[counter]] <- list(word = longest_sequence, start = start_pos, end = idx)
              } else {
                words_found[[counter]] <- list(word = longest_sequence)
              }
              counter <- counter + 1
            }
            reset_dict <- TRUE
          } else {
            dict <- private$attrs$dict
            reset_dict <- TRUE
          }
        } else if (!is.null(dict[[char]])) {
          dict <- dict[[char]]
        } else {
          dict <- private$attrs$dict
          reset_dict <- TRUE
          idy <- idx + 1
          while (idy <= len) {
            char <- sentence[idy]
            if (!char %in% private$attrs$word_chars) {
              break
            }
            idy <- idy + 1
          }
          idx <- idy
        }
        if (idx + 1 > len) {
          if (!is.null(dict[[private$attrs$id]])) {
            sequence <- dict[[private$attrs$id]]
            if (span_info) {
              words_found[[counter]] <- list(word = sequence, start = start_pos, end = idx)
            } else {
              words_found[[counter]] <- list(word = sequence)
            }
            counter <- counter + 1
          }
        }
        idx <- idx + 1
        if (reset_dict) {
          reset_dict <- FALSE
          start_pos <- idx
        }
      }
      return(words_found)
    },
    #-----------------------------------------------------------------
    replace_key = function(sentence) {
      words_found <- list()
      dict <- private$attrs$dict
      if (!length(dict)) {
        warning("No key-words in the dictionary. Add keys-words first")
        return(words_found)
      }
      new_sentence <- sentence
      new_sentence <- strsplit(new_sentence, split = "", fixed = TRUE)[[1]]
      if (private$attrs$ignore_case) {
        sentence <- tolower(sentence)
      }
      start_pos <- 1
      end_pos <- 1
      reset_dict <- FALSE
      idx <- 1
      sentence <- strsplit(sentence, split = "", fixed = TRUE)[[1]]
      len <- length(sentence)
      while (idx <= len) {
        char <- sentence[idx]
        if (!char %in% private$attrs$word_chars) {
          if (!is.null(dict[[private$attrs$id]]) || !is.null(dict[[char]])) {
            sequence <- NULL
            longest_sequence <- NULL
            longer <- FALSE
            inner_char <- "<eof>"
            if (!is.null(dict[[private$attrs$id]])) {
              sequence <- dict[[private$attrs$id]]
              longest_sequence <- dict[[private$attrs$id]]
              end_pos <- idx
            }
            if (!is.null(dict[[char]])) {
              dict_cont <- dict[[char]]
              idy <- idx + 1
              while (idy <= len) {
                inner_char <- sentence[idy]
                if (!inner_char %in% private$attrs$word_chars && !is.null(dict_cont[[private$attrs$id]])) {
                  longest_sequence <- dict_cont[[private$attrs$id]]
                  end_pos <- idy
                  longer <- TRUE
                }
                if (!is.null(dict_cont[[inner_char]])) {
                  dict_cont <- dict_cont[[inner_char]]
                } else {
                  break
                }
                idy <- idy + 1
              }
              if ((idy > len || !inner_char %in% private$attrs$word_chars) && !is.null(dict_cont[[private$attrs$id]])) {
                longest_sequence <- dict_cont[[private$attrs$id]]
                end_pos <- idy
                longer <- TRUE
              }
              if (longer) {
                idx <- end_pos
              }
            }
            dict <- private$attrs$dict
            if (!is.null(longest_sequence)) {
              new_sentence[start_pos:(idx - 1)] <- ""
              new_sentence[start_pos] <- longest_sequence
            }
            reset_dict <- TRUE
          } else {
            dict <- private$attrs$dict
            reset_dict <- TRUE
          }
        } else if (!is.null(dict[[char]])) {
          dict <- dict[[char]]
        } else {
          dict <- private$attrs$dict
          reset_dict <- TRUE
          idy <- idx + 1
          while (idy <= len) {
            char <- sentence[idy]
            if (!char %in% private$attrs$word_chars) {
              break
            }
            idy <- idy + 1
          }
          idx <- idy
        }
        if (idx + 1 > len) {
          if (!is.null(dict[[private$attrs$id]])) {
            sequence <- dict[[private$attrs$id]]
            new_sentence[start_pos:idx] <- ""
            new_sentence[start_pos] <- sequence
          }
        }
        idx <- idx + 1
        if (reset_dict) {
          reset_dict <- FALSE
          start_pos <- idx
        }
      }
      return(paste(new_sentence, collapse = ""))
    }
  ),
  cloneable = FALSE
)
