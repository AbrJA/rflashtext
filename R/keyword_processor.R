#' @name keyword_processor
#' @title Aho-Corasick algorithm to find and replace words
#' @description
#' @examples
#' library(rflashtext)
#'
#' processor <- keyword_processor$new()
#' processor$set_key_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"), info = FALSE)
#'
#' processor$contain_keys(keys = "NY")
#' processor$get_words(keys = "LA")
#'
#' processor$find_key_words(sentence = "I live in LA and I like NY")
#' processor$replace_key_words(sentence = "I live in LA and I like NY")
#' @export
keyword_processor <- R6::R6Class(
  classname = "keyword_processor",
  public = list(
    #-----------------------------------------------------------------
    init = function(id = "_key_", lower = TRUE, chars = c(letters, LETTERS, 0:9, "_"), dict = list()) {
      ifelse(is.null(dict), list(), dict)
      done <- private$set_attrs(id = id, lower = lower, chars = chars, dict = dict)
      return(done)
    },
    #-----------------------------------------------------------------
    contain_keys = function(keys) {
      stopifnot(is.character(keys) && !identical(keys, "") && !identical(keys, NA_character_) && length(keys) != 0)
      if(private$attrs$lower) {
        keys <- tolower(keys)
      }
      len <- length(keys)
      contained <- vector("logical", len)
      for(k in 1:len) {
        contained[k] <- private$contain_key(key = keys[k])
      }
      return(contained)
    },
    #-----------------------------------------------------------------
    get_words = function(keys) {
      stopifnot(is.character(keys) && !identical(keys, "") && !identical(keys, NA_character_) && length(keys) != 0)
      if(private$attrs$lower) {
        keys <- tolower(keys)
      }
      len <- length(keys)
      words <- vector("character", len)
      for(k in 1:len) {
        words[k] <- private$get_word(key = keys[k])
      }
      return(words)
    },
    #-----------------------------------------------------------------
    set_key_words = function(keys, words = NULL, info = FALSE) {
      stopifnot(is.character(keys) && !identical(keys, "") && !identical(keys, NA_character_) && length(keys) != 0)
      if(private$attrs$lower) {
        keys <- tolower(keys)
      }
      len <- length(keys)
      if(!is.null(words)) {
        stopifnot(is.character(words))
        stopifnot(len == length(words))
      } else {
        words <- keys
      }
      if(info) {
        added <- vector("logical", len)
        for(k in 1:len) {
          added[k] <- private$set_key_word(key = keys[k], word = words[k])
        }
        return(added)
      } else {
        for(k in 1:len) {
          warn <- FALSE
          added <- private$set_key_word(key = keys[k], word = words[k])
          if(!added){
            warn <- TRUE
          }
        }
        if(warn) {
          warning("Some key-words were not added maybe, due to duplicates. Use info = TRUE to check it")
        }
        return(TRUE)
      }
    },
    #-----------------------------------------------------------------
    find_key_words = function(sentence, info = TRUE) {
      stopifnot(is.character(sentence) && !identical(sentence, "") && !identical(sentence, NA_character_) && length(sentence) != 0)
      if(private$attrs$lower) {
        sentence <- tolower(sentence)
      }
      found <- private$find(sentence = sentence, info = info)
      return(found)
    },
    #-----------------------------------------------------------------
    replace_key_words = function(sentence) {
      stopifnot(is.character(sentence) && !is.na(sentence) && sentence != "" && length(sentence) != 0)
      if(private$attrs$lower) {
        sentence <- tolower(sentence)
      }
      new_sentence <- private$replace(sentence = sentence)
      return(new_sentence)
    },
    #-----------------------------------------------------------------
    show_attrs = function() {
      return(private$attrs)
    }
    #-----------------------------------------------------------------
  ),
  private = list(
    #-----------------------------------------------------------------
    attrs = list(
      id = "_key_",
      chars = c(letters, LETTERS, 0:9, "_"),
      dict = list(),
      lower = TRUE,
      terms = 0L
    ),
    #-----------------------------------------------------------------
    set_attrs = function(id, lower, chars, dict) {
      private$attrs$id <- id
      private$attrs$lower <- lower
      private$attrs$chars <- chars
      private$attrs$dict <- dict
      return(TRUE)
    },
    #-----------------------------------------------------------------
    contain_key = function(key) {
      dict <- private$attrs$dict
      key <- strsplit(key, split = "", fixed = TRUE)[[1]]
      counter <- 0
      for(letter in key) {
        counter <- counter + 1
        dict <- dict[[letter]]
        if(is.null(dict)) {
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
      for(letter in key) {
        counter <- counter + 1
        dict <- dict[[letter]]
        if(is.null(dict)) {
          break
        }
      }
      if(!is.null(dict[[private$attrs$id]]) && counter == length(key)) {
        return(dict[[private$attrs$id]])
      }
      return(NA_character_)
    },
    #-----------------------------------------------------------------
    set_key_word = function(key, word) {
      status <- FALSE
      dict <- private$attrs$dict
      key <- strsplit(key, split = "", fixed = TRUE)[[1]]
      counter <- 0
      for(letter in key) {
        counter <- counter + 1
        dict <- dict[[letter]]
        if(is.null(dict)) {
          break
        }
      }
      if(is.null(private$attrs$dict[[key[1:counter]]][[private$attrs$id]])) {
        status <- TRUE
        private$attrs$terms <- private$attrs$terms + 1
      }
      while(counter <= length(key)) {
        if(counter == length(key)) {
          private$attrs$dict[[key[1:counter]]][private$attrs$id] <- list(word)
        } else {
          private$attrs$dict[[key[1:counter]]] <- list()
        }
        counter <- counter + 1
      }
      return(status)
    },
    #-----------------------------------------------------------------
    find = function(sentence, info) {
      words_found <- list()
      dict <- private$attrs$dict
      if(!length(dict)) {
        warning("No key-words in the dictionary. Set key-words first")
        return(words_found)
      }
      start_pos <- 1
      end_pos <- 1
      reset_dict <- FALSE
      idx <- 1
      sentence <- strsplit(sentence, split = "", fixed = TRUE)[[1]]
      len <- length(sentence)
      counter <- 1
      while(idx <= len) {
        char <- sentence[idx]
        if(!char %in% private$attrs$chars) {
          if(!is.null(dict[[private$attrs$id]]) || !is.null(dict[[char]])) {
            sequence <- NULL
            longest_sequence <- NULL
            longer <- FALSE
            inner_char <- "<eof>"
            if(!is.null(dict[[private$attrs$id]])) {
              sequence <- dict[[private$attrs$id]]
              longest_sequence <- dict[[private$attrs$id]]
              end_pos <- idx
            }
            if(!is.null(dict[[char]])) {
              dict_cont <- dict[[char]]
              idy <- idx + 1
              while(idy <= len) {
                inner_char <- sentence[idy]
                if(!inner_char %in% private$attrs$chars && !is.null(dict_cont[[private$attrs$id]])) {
                  longest_sequence <- dict_cont[[private$attrs$id]]
                  end_pos <- idy
                  longer <- TRUE
                }
                if(!is.null(dict_cont[[inner_char]])) {
                  dict_cont <- dict_cont[[inner_char]]
                } else {
                  break
                }
                idy <- idy + 1
              }
              if(!inner_char %in% private$attrs$chars && !is.null(dict_cont[[private$attrs$id]])) {
                longest_sequence <- dict_cont[[private$attrs$id]]
                end_pos <- idy
                longer <- TRUE
              }
              if(longer) {
                idx <- end_pos
              }
            }
            dict <- private$attrs$dict
            if(!is.null(longest_sequence)) {
              if(info) {
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
        } else if(!is.null(dict[[char]])) {
          dict <- dict[[char]]
        } else {
          dict <- private$attrs$dict
          reset_dict <- TRUE
          idy <- idx + 1
          while(idy <= len) {
            char <- sentence[idy]
            if(!char %in% private$attrs$chars) {
              break
            }
            idy <- idy + 1
          }
          idx <- idy
        }
        if(idx + 1 > len) {
          if(!is.null(dict[[private$attrs$id]])) {
            sequence <- dict[[private$attrs$id]]
            if(info) {
              words_found[[counter]] <- list(word = sequence, start = start_pos, end = idx)
            } else {
              words_found[[counter]] <- list(word = sequence)
            }
            counter <- counter + 1
          }
        }
        idx <- idx + 1
        if(reset_dict) {
          reset_dict <- FALSE
          start_pos <- idx
        }
      }
      return(words_found)
    },



    #-----------------------------------------------------------------
    replace = function(sentence) {
      words_found <- list()
      dict <- private$attrs$dict
      if(!length(dict)) {
        warning("No key-words in the dictionary. Set key-words first")
        return(words_found)
      }
      start_pos <- 1
      end_pos <- 1
      reset_dict <- FALSE
      idx <- 1
      sentence <- strsplit(sentence, split = "", fixed = TRUE)[[1]]
      len <- length(sentence)
      #counter <- 1
      while(idx <= len) {
        char <- sentence[idx]
        if(!char %in% private$attrs$chars) {
          if(!is.null(dict[[private$attrs$id]]) || !is.null(dict[[char]])) {
            sequence <- NULL
            longest_sequence <- NULL
            longer <- FALSE
            inner_char <- "<eof>"
            if(!is.null(dict[[private$attrs$id]])) {
              sequence <- dict[[private$attrs$id]]
              longest_sequence <- dict[[private$attrs$id]]
              end_pos <- idx
            }
            if(!is.null(dict[[char]])) {
              dict_cont <- dict[[char]]
              idy <- idx + 1
              while(idy <= len) {
                inner_char <- sentence[idy]
                if(!inner_char %in% private$attrs$chars && !is.null(dict_cont[[private$attrs$id]])) {
                  longest_sequence <- dict_cont[[private$attrs$id]]
                  end_pos <- idy
                  longer <- TRUE
                }
                if(!is.null(dict_cont[[inner_char]])) {
                  dict_cont <- dict_cont[[inner_char]]
                } else {
                  break
                }
                idy <- idy + 1
              }
              if(!inner_char %in% private$attrs$chars && !is.null(dict_cont[[private$attrs$id]])) {
                longest_sequence <- dict_cont[[private$attrs$id]]
                end_pos <- idy
                longer <- TRUE
              }
              if(longer) {
                idx <- end_pos
              }
            }
            dict <- private$attrs$dict
            if(!is.null(longest_sequence)) {
              sentence[start_pos:(idx - 1)] <- ""
              sentence[start_pos] <- longest_sequence
            }
            reset_dict <- TRUE
          } else {
            dict <- private$attrs$dict
            reset_dict <- TRUE
          }
        } else if(!is.null(dict[[char]])) {
          dict <- dict[[char]]
        } else {
          dict <- private$attrs$dict
          reset_dict <- TRUE
          idy <- idx + 1
          while(idy <= len) {
            char <- sentence[idy]
            if(!char %in% private$attrs$chars) {
              break
            }
            idy <- idy + 1
          }
          idx <- idy
        }
        if(idx + 1 > len) {
          if(!is.null(dict[[private$attrs$id]])) {
            sequence <- dict[[private$attrs$id]]
            sentence[start_pos:idx] <- ""
            sentence[start_pos] <- sequence
          }
        }
        idx <- idx + 1
        if(reset_dict) {
          reset_dict <- FALSE
          start_pos <- idx
        }
      }
      return(paste(sentence, collapse = ""))
    }



  ),
  cloneable = FALSE
)
