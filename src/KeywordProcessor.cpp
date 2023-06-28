#include <Rcpp.h>
#include <iostream>
#include "json.h"

using namespace Rcpp;

using json = nlohmann::json;
using json_pointer = nlohmann::json::json_pointer;

std::string separatePath(const std::string& key) {
  if (key.empty()) {
    return "";
  }

  std::string key_path;
  key_path.reserve(2 * key.size() + 1);

  for (char c : key) {
    key_path.push_back('/');
    key_path.push_back(c);
  }

  return key_path;
}

// [[Rcpp::export]]
SEXP loadTrie(const std::string& trieStr) {
  Rcpp::XPtr<json> ptr(new json(json::parse(trieStr)), true);
  return ptr;
}

// [[Rcpp::export]]
SEXP buildTrie(const Rcpp::CharacterVector& keys, const Rcpp::CharacterVector& values, const std::string& id) {
  json trie;
  const Rcpp::LogicalVector keys_na = Rcpp::is_na(keys);
  const int size = keys.size();

  for (int i = 0; i < size; i++) {
    if (!keys_na[i]) {
      const std::string key = Rcpp::as<std::string>(keys[i]);
      if (!key.empty()) {
        const std::string path = separatePath(key) + "/" + id;
        trie[json_pointer(path)] = values[i];
      }
    }
  }

  Rcpp::XPtr<json> ptr(new json(trie), true);
  return ptr;
}

// [[Rcpp::export]]
int addKeysWords(SEXP ptr, const Rcpp::CharacterVector& keys, const Rcpp::CharacterVector& values, const std::string& id) {
  Rcpp::XPtr<json> trie(ptr);
  int counter = 0;
  const Rcpp::LogicalVector keys_na = Rcpp::is_na(keys);
  const int size = keys.size();

  for (int i = 0; i < size; ++i) {
    if (!keys_na[i]) {
      const std::string key = Rcpp::as<std::string>(keys[i]);
      if (!key.empty()) {
        const std::string path = separatePath(key) + "/" + id;
        (*trie)[json_pointer(path)] = values[i];
        ++counter;
      }
    }
  }

  return counter;
}

// [[Rcpp::export]]
std::string dumpTrie(SEXP ptr) {
  const Rcpp::XPtr<json> trie(ptr);
  return trie->dump();
}

// [[Rcpp::export]]
Rcpp::LogicalVector containKeys(SEXP ptr, const Rcpp::CharacterVector& keys, const std::string& id) {
  const Rcpp::XPtr<json> trie(ptr);
  const int size = keys.size();
  Rcpp::LogicalVector is_in(size);

  for (int i = 0; i < size; ++i) {
    const std::string key = Rcpp::as<std::string>(keys[i]);
    const std::string path = separatePath(key) + "/" + id;
    is_in[i] = trie->contains(json_pointer(path));
  }

  return is_in;
}

// [[Rcpp::export]]
Rcpp::StringVector getWords(SEXP ptr, const Rcpp::CharacterVector& keys, const std::string& id) {
  const Rcpp::XPtr<json> trie(ptr);
  const int size = keys.size();
  Rcpp::StringVector words(size);

  for (int i = 0; i < size; ++i) {
    const std::string key = Rcpp::as<std::string>(keys[i]);
    const std::string path = separatePath(key) + "/" + id;
    const json_pointer ptr_path(path);

    if (trie->contains(ptr_path)) {
      words[i] = trie->at(ptr_path).get<std::string>();
    } else {
      words[i] = NA_STRING;
    }
  }

  return words;
}

// [[Rcpp::export]]
Rcpp::List findKeys(SEXP ptr, const Rcpp::CharacterVector& sentences, const std::string& word_chars, const std::string& id, bool span_info) {
  const Rcpp::XPtr<json> trie(ptr);
  const int lens = sentences.length();
  Rcpp::List keys_found(lens);

  for(int i = 0; i < lens; i++) {
    const std::string sentence = Rcpp::as<std::string>(sentences[i]);
    const int len = sentence.length();
    int start_pos = 0, end_pos, idx = 0, idy;
    std::string path;
    std::vector<std::string> words_found;
    std::vector<int> start_index, end_index;

    while (idx < len) {
      const std::string letter = sentence.substr(idx, 1);
      std::string sequence;
      if (word_chars.find(letter) == std::string::npos) {
        bool longer = false;
        std::string longest_sequence;
        if (trie->contains(json_pointer(separatePath(path) + "/" + id))) {
          sequence = trie->at(json_pointer(separatePath(path) + "/" + id)).get<std::string>();
          longest_sequence = sequence;
          end_pos = idx;
        }
        if (trie->contains(json_pointer(separatePath(path) + "/" + letter))) {
          std::string inner_letter, inner_path = path + letter;
          idy = idx + 1;
          while (idy < len) {
            inner_letter = sentence.substr(idy, 1);
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
        path.clear();
        if (!longest_sequence.empty()) {
          words_found.push_back(longest_sequence);
          if (span_info) {
            start_index.push_back(start_pos + 1);
            end_index.push_back(idx);
          }
        }
      } else if (trie->contains(json_pointer(separatePath(path + letter)))) {
        path += letter;
      } else {
        path.clear();
        idy = idx + 1;
        while (idy < len && word_chars.find(sentence.substr(idy, 1)) != std::string::npos) {
          ++idy;
        }
        idx = idy;
      }
      if (idx + 1 >= len && trie->contains(json_pointer(separatePath(path) + "/" + id))) {
        sequence = trie->at(json_pointer(separatePath(path) + "/" + id)).get<std::string>();
        words_found.push_back(sequence);
        if (span_info) {
          start_index.push_back(start_pos + 1);
          end_index.push_back(idx + 1);
        }
      }
      idx++;
      if (path.empty()) {
        start_pos = idx;
      }
    }
    if (span_info) {
      keys_found[i] = Rcpp::List::create(Rcpp::Named("word") = words_found, Rcpp::Named("start") = start_index, Rcpp::Named("end") = end_index);
    } else {
      keys_found[i] = Rcpp::List::create(Rcpp::Named("word") = words_found);
    }
  }
  return keys_found;
}

// [[Rcpp::export]]
Rcpp::StringVector replaceKeys(SEXP ptr, const Rcpp::CharacterVector& sentences, const std::string& word_chars, const std::string& id) {
  const Rcpp::XPtr<json> trie(ptr);
  const int lens = sentences.length();
  const Rcpp::LogicalVector sentences_na = Rcpp::is_na(sentences);
  Rcpp::StringVector new_sentences(lens);

  for(int i = 0; i < lens; i++) {
    if (sentences_na[i]) {
      new_sentences[i] = NA_STRING;
    } else {
      std::string new_sentence;
      const std::string sentence = Rcpp::as<std::string>(sentences[i]);
      const int len = sentence.length();
      int end_pos, idx = 0, idy;
      std::string path, word;

      while (idx < len) {
        const std::string letter = sentence.substr(idx, 1);
        std::string sequence;
        std::string white_space;
        if (word_chars.find(letter) == std::string::npos) {
          bool longer = false;
          std::string longest_sequence;
          word += letter;
          white_space = letter;
          if (trie->contains(json_pointer(separatePath(path) + "/" + id))) {
            sequence = trie->at(json_pointer(separatePath(path) + "/" + id)).get<std::string>();
            longest_sequence = sequence;
            end_pos = idx;
          }
          if (trie->contains(json_pointer(separatePath(path) + "/" + letter))) {
            std::string inner_letter, inner_path = path + letter, inner_word = word;
            idy = idx + 1;
            while (idy < len) {
              inner_letter = sentence.substr(idy, 1);
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
              if (idy >= len) {
                white_space.clear();
              }
              longest_sequence = trie->at(json_pointer(separatePath(inner_path) + "/" + id)).get<std::string>();
              end_pos = idy;
              longer = true;
            }
            if (longer) {
              word = inner_word;
              idx = end_pos;
            }
          }
          path.clear();
          if (!longest_sequence.empty()) {
            new_sentence.append(longest_sequence + white_space);
            word.clear();
            white_space.clear();
          } else {
            new_sentence.append(word);
            word.clear();
            white_space.clear();
          }
        } else if (trie->contains(json_pointer(separatePath(path + letter)))) {
          word += letter;
          path += letter;
        } else {
          word += letter;
          path.clear();
          idy = idx + 1;
          while (idy < len) {
            word += sentence.substr(idy, 1);
            if (word_chars.find(sentence.substr(idy, 1)) == std::string::npos) {
              break;
            }
            idy++;
          }
          idx = idy;
          new_sentence.append(word);
          word.clear();
          white_space.clear();
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
      new_sentences[i] = new_sentence;
    }
  }
  return new_sentences;
}
