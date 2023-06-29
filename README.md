
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rflashtext

<!-- badges: start -->

[![R-CMD-check](https://github.com/AbrJA/rflashtext/workflows/R-CMD-check/badge.svg)](https://cran.r-project.org/web/checks/check_results_rflashtext.html)
[![Grand-total](https://cranlogs.r-pkg.org/badges/grand-total/rflashtext)](https://github.com/AbrJA/rflashtext/actions)
[![Per-month](https://cranlogs.r-pkg.org/badges/rflashtext)](https://CRAN.R-project.org/package=rflashtext)
<!-- badges: end -->

*rflashtext* **can be used to find and replace words in a given text
with only one pass over the document.**

It’s a R implementation of the [FlashText
algorithm](https://arxiv.org/abs/1711.00046) and it’s inspired on the
python library [flashtext](https://github.com/vi3k6i5/flashtext).

## Installation

You can install the released version of rflashtext from
[CRAN](https://CRAN.R-project.org/package=rflashtext) with:

``` r
install.packages("rflashtext")
```

And the development version from
[GitHub](https://github.com/AbrJA/rflashtext) with:

``` r
install.packages("devtools")
devtools::install_github("AbrJA/rflashtext")
```

## Example

This is a basic example which shows you how to use the API:

### New processor

``` r
library(rflashtext)

processor <- KeywordProcessor$new(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
processor$show_trie()
#> [1] "{\"L\":{\"A\":{\"_word_\":\"Los Angeles\"}},\"N\":{\"Y\":{\"_word_\":\"New York\"}}}"
```

### Add keys-words to processor

``` r
processor$add_keys_words(keys = c("TX", "CA"), words = c("Texas", "California"))
processor$show_trie()
#> [1] "{\"C\":{\"A\":{\"_word_\":\"California\"}},\"L\":{\"A\":{\"_word_\":\"Los Angeles\"}},\"N\":{\"Y\":{\"_word_\":\"New York\"}},\"T\":{\"X\":{\"_word_\":\"Texas\"}}}"
```

### Find keys in a sentence

``` r
words_found <- processor$find_keys(sentences = c("I live in LA and I like NY", "Have you been in TX?"))
words_found
#> [[1]]
#> [[1]]$word
#> [1] "Los Angeles" "New York"   
#> 
#> [[1]]$start
#> [1] 11 25
#> 
#> [[1]]$end
#> [1] 12 26
#> 
#> 
#> [[2]]
#> [[2]]$word
#> [1] "Texas"
#> 
#> [[2]]$start
#> [1] 18
#> 
#> [[2]]$end
#> [1] 19
data.table::rbindlist(words_found)
#>           word start end
#> 1: Los Angeles    11  12
#> 2:    New York    25  26
#> 3:       Texas    18  19
```

### Replace keys in a sentence

``` r
processor$replace_keys(sentences = c("I live in LA and I like NY", "Have you been in TX?"))
#> [1] "I live in Los Angeles and I like New York"
#> [2] "Have you been in Texas?"
```

To see more details about the performance of the algorithm, click
[here](https://github.com/AbrJA/rflashtext_benchmark).
