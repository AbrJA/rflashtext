#include <Rcpp.h>
#include <iostream>
#include "json.h"

using namespace Rcpp;

using json = nlohmann::json;
using json_pointer = nlohmann::json::json_pointer;

std::string separatePath(const std::string key) {
  if (!key.size()) {
    return "";
  }
  std::stringstream key_path;
  for (unsigned int i = 0; i < key.size(); i++) {
    key_path << "/" << key[i];
  }
  return key_path.str();
}

// [[Rcpp::export]]
SEXP loadTrie(std::string trieStr) {
  json trie = json::parse(trieStr);
  json *p_trie = new json(trie);
  Rcpp::XPtr<json> ptr(p_trie, true);
  return ptr;
}

// [[Rcpp::export]]
SEXP buildTrie(Rcpp::CharacterVector keys, Rcpp::CharacterVector values, std::string id) {
  json trie;
  std::string path, key;
  Rcpp::LogicalVector keys_na = Rcpp::is_na(keys);
  for(int i = 0; i < keys.size(); i++) {
    key = Rcpp::as<std::string>(keys[i]);
    if (!keys_na[i] && !key.empty()) {
      path = separatePath(key) + "/" + id;
      trie[json_pointer(path)] = values[i];
    }
  }
  json *p_trie = new json(trie);
  Rcpp::XPtr<json> ptr(p_trie, true);
  return ptr;
}

// [[Rcpp::export]]
int addKeysWords(SEXP ptr, Rcpp::CharacterVector keys, Rcpp::CharacterVector values, std::string id) {
  Rcpp::XPtr<json> trie(ptr);
  int counter = 0;
  std::string path, key;
  Rcpp::LogicalVector keys_na = Rcpp::is_na(keys);
  for(int i = 0; i < keys.size(); i++) {
    key = Rcpp::as<std::string>(keys[i]);
    if (!keys_na[i] && !key.empty()) {
      path = separatePath(key) + "/" + id;
      trie->operator[](json_pointer(path)) = values[i];
      counter += 1;
    }
  }
  return counter;
}

// [[Rcpp::export]]
std::string dumpTrie(SEXP ptr) {
  Rcpp::XPtr<json> trie(ptr);
  return trie->dump();
}

// [[Rcpp::export]]
Rcpp::LogicalVector containKeys(SEXP ptr, Rcpp::CharacterVector keys, std::string id) {
  Rcpp::XPtr<json> trie(ptr);
  Rcpp::LogicalVector is_in(keys.size());
  std::string path;
  for(int i = 0; i < keys.size(); i++) {
    path = separatePath(Rcpp::as<std::string>(keys[i])) + "/" + id;
    is_in[i] = trie->contains(json_pointer(path));
  }
  return is_in;
}

// [[Rcpp::export]]
Rcpp::StringVector getWords(SEXP ptr, Rcpp::CharacterVector keys, std::string id) {
  Rcpp::XPtr<json> trie(ptr);
  Rcpp::CharacterVector words(keys.size());
  std::string path;
  for(int i = 0; i < keys.size(); i++) {
    path = separatePath(Rcpp::as<std::string>(keys[i])) + "/" + id;
    if (!trie->contains(json_pointer(path))) {
      words[i] = NA_STRING;
      continue;
    }
    words[i] = trie->at(json_pointer(path)).get<std::string>();
  }
  return words;
}

Rcpp::List findKeysSingle(SEXP ptr, std::string sentence, std::string word_chars, std::string id, bool span_info) {
  Rcpp::XPtr<json> trie(ptr);
  int start_pos = 0, end_pos = 0, idx = 0, idy, len = sentence.length();
  bool reset_path = false, longer;
  std::string letter, inner_letter, sequence, longest_sequence, path, inner_path;
  Rcpp::CharacterVector words_found;
  Rcpp::IntegerVector start_index, end_index;
  while (idx < len) {
    letter = sentence[idx];
    if (word_chars.find(letter) == std::string::npos) {
      if (trie->contains(json_pointer(separatePath(path) + "/" + id)) || trie->contains(json_pointer(separatePath(path) + "/" + letter))) {
        sequence = "";
        longest_sequence = "";
        longer = false;
        inner_letter = "";
        if (trie->contains(json_pointer(separatePath(path) + "/" + id))) {
          sequence = trie->at(json_pointer(separatePath(path) + "/" + id)).get<std::string>();
          longest_sequence = sequence;
          end_pos = idx;
        }
        if (trie->contains(json_pointer(separatePath(path) + "/" + letter))) {
          inner_path = path + letter;
          idy = idx + 1;
          while (idy < len) {
            inner_letter = sentence[idy];
            if (word_chars.find(inner_letter) == std::string::npos && trie->contains(json_pointer(separatePath(inner_path) + "/" + id))) {
              longest_sequence = trie->at(json_pointer(separatePath(inner_path) + "/" + id)).get<std::string>();
              end_pos = idy;
              longer = true;
            }
            if (trie->contains(json_pointer(separatePath(inner_path) + "/" + inner_letter))) {
              inner_path += inner_letter;
            } else {
              break;
            }
            idy++;
          }
          if ((idy >= len || word_chars.find(inner_letter) == std::string::npos) && trie->contains(json_pointer(separatePath(inner_path) + "/" + id))) {
            longest_sequence = trie->at(json_pointer(separatePath(inner_path) + "/" + id)).get<std::string>();
            end_pos = idy;
            longer = true;
          }
          if (longer) {
            idx = end_pos;
          }
        }
        path = "";
        if (!longest_sequence.empty()) {
          words_found.push_back(longest_sequence);
          if (span_info) {
            start_index.push_back(start_pos);
            end_index.push_back(idx);
          }
        }
        reset_path = true;
      } else {
        path = "";
        reset_path = true;
      }
    } else if (trie->contains(json_pointer(separatePath(path + letter)))) {
      path += letter;
    } else {
      path = "";
      reset_path = true;
      idy = idx + 1;
      while (idy < len) {
        letter = sentence[idy];
        if (word_chars.find(letter) == std::string::npos) {
          break;
        }
        idy++;
      }
      idx = idy;
    }
    if (idx + 1 >= len) {
      if (trie->contains(json_pointer(separatePath(path) + "/" + id))) {
        sequence = trie->at(json_pointer(separatePath(path) + "/" + id)).get<std::string>();
        words_found.push_back(sequence);
        if (span_info) {
          start_index.push_back(start_pos);
          end_index.push_back(idx + 1);
        }
      }
    }
    idx++;
    if (reset_path) {
      reset_path = false;
      start_pos = idx;
    }
  }
  if (span_info) return Rcpp::List::create(_["word"] = words_found, _["start"] = 1 + start_index, _["end"] = end_index);
  return Rcpp::List::create(_["word"] = words_found);
}

std::string replaceKeysSingle(SEXP ptr, std::string sentence, std::string word_chars, std::string id) {
  Rcpp::XPtr<json> trie(ptr);
  int end_pos = 0, idx = 0, idy, len = sentence.length();
  bool longer;
  std::string letter, inner_letter, sequence, longest_sequence, path, inner_path, new_sentence, word, inner_word, white_space;
  while (idx < len) {
    letter = sentence[idx];
    if (word_chars.find(letter) == std::string::npos) {
      word += letter;
      white_space = letter;
      if (trie->contains(json_pointer(separatePath(path) + "/" + id)) || trie->contains(json_pointer(separatePath(path) + "/" + letter))) {
        sequence = "";
        longest_sequence = "";
        longer = false;
        inner_letter = "";
        if (trie->contains(json_pointer(separatePath(path) + "/" + id))) {
          sequence = trie->at(json_pointer(separatePath(path) + "/" + id)).get<std::string>();
          longest_sequence = sequence;
          end_pos = idx;
        }
        if (trie->contains(json_pointer(separatePath(path) + "/" + letter))) {
          inner_path = path + letter;
          inner_word = word;
          idy = idx + 1;
          while (idy < len) {
            inner_letter = sentence[idy];
            if (word_chars.find(inner_letter) == std::string::npos && trie->contains(json_pointer(separatePath(inner_path) + "/" + id))) {
              inner_word += inner_letter;
              white_space = inner_letter;
              longest_sequence = trie->at(json_pointer(separatePath(inner_path) + "/" + id)).get<std::string>();
              end_pos = idy;
              longer = true;
            }
            if (trie->contains(json_pointer(separatePath(inner_path) + "/" + inner_letter))) {
              inner_word += inner_letter;
              inner_path += inner_letter;
            } else {
              break;
            }
            idy++;
          }
          if ((idy >= len || word_chars.find(inner_letter) == std::string::npos) && trie->contains(json_pointer(separatePath(inner_path) + "/" + id))) {
            white_space = "";
            longest_sequence = trie->at(json_pointer(separatePath(inner_path) + "/" + id)).get<std::string>();
            end_pos = idy;
            longer = true;
          }
          if (longer) {
            word = inner_word;
            idx = end_pos;
          }
        }
        path = "";
        if (!longest_sequence.empty()) {
          new_sentence.append(longest_sequence + white_space);
          word = "";
          white_space = "";
        } else {
          new_sentence.append(word);
          word = "";
          white_space = "";
        }
      } else {
        new_sentence.append(word);
        word = "";
        white_space = "";
        path = "";
      }
    } else if (trie->contains(json_pointer(separatePath(path + letter)))) {
      word += letter;
      path += letter;
    } else {
      word += letter;
      path = "";
      idy = idx + 1;
      while (idy < len) {
        letter = sentence[idy];
        word += letter;
        if (word_chars.find(letter) == std::string::npos) {
          break;
        }
        idy++;
      }
      idx = idy;
      new_sentence.append(word);
      word = "";
      white_space = "";
    }
    if (idx + 1 >= len) {
      if (trie->contains(json_pointer(separatePath(path) + "/" + id))) {
        sequence = trie->at(json_pointer(separatePath(path) + "/" + id)).get<std::string>();
        new_sentence.append(sequence);
      } else {
        new_sentence.append(word);
      }
    }
    idx++;
  }
  return new_sentence;
}

// [[Rcpp::export]]
Rcpp::List findKeys(SEXP ptr, Rcpp::CharacterVector sentences, std::string word_chars, std::string id, bool span_info) {
  Rcpp::List keys_found;
  for(int i = 0; i < sentences.size(); i++) {
    keys_found.push_back(findKeysSingle(ptr, Rcpp::as<std::string>(sentences[i]), word_chars, id, span_info));
  }
  return keys_found;
}

// [[Rcpp::export]]
Rcpp::CharacterVector replaceKeys(SEXP ptr, Rcpp::CharacterVector sentences, std::string word_chars, std::string id) {
  Rcpp::CharacterVector new_sentences;
  Rcpp::LogicalVector sentences_na = Rcpp::is_na(sentences);
  for(int i = 0; i < sentences.size(); i++) {
    if (sentences_na[i]) {
      new_sentences.push_back(NA_STRING);
    } else {
      new_sentences.push_back(replaceKeysSingle(ptr, Rcpp::as<std::string>(sentences[i]), word_chars, id));
    }
  }
  return new_sentences;
}