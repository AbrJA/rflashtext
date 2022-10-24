
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rflashtext

<!-- badges: start -->

[![R-CMD-check](https://github.com/AbrJA/rflashtext/workflows/R-CMD-check/badge.svg)](https://github.com/AbrJA/rflashtext/actions)
[![Grand-total](https://cranlogs.r-pkg.org/badges/grand-total/rflashtext)](https://cran.r-project.org/web/packages/rflashtext/index.html)
[![Per-month](https://cranlogs.r-pkg.org/badges/rflashtext)](https://cran.r-project.org/web/packages/rflashtext/index.html)
<!-- badges: end -->

*rflashtext* **can be used to find and replace words in a given text
with only one pass over the document.**

It’s a pure R implementation of the [FlashText
algorithm](https://arxiv.org/abs/1711.00046) and it’s inspired on the
python library [flashtext](https://github.com/vi3k6i5/flashtext).

## Installation

You can install the released version of rflashtext from
[CRAN](https://cran.r-project.org/web/packages/rflashtext/index.html)
with:

``` r
install.packages("rflashtext")
```

And the development version from
[GitHub](https://github.com/AbrJA/rflashtext) with:

``` r
# install.packages("devtools")
devtools::install_github("AbrJA/rflashtext")
```

## Example

This is a basic example which shows you how to use the API:

### New processor

``` r
library(rflashtext)

processor <- keyword_processor$new(ignore_case = FALSE, word_chars = c(letters, LETTERS))
processor$show_attrs(attrs = "dict_size")
#> [1] 0
```

### Add keys-words to processor

``` r
processor$add_keys_words(keys = c("NY", "LA"), words = c("New York", "Los Angeles"))
processor$show_attrs(attrs = c("dict", "dict_size"))
#> $dict
#> $dict$`_class_`
#> [1] "keyword_dictionary"
#> 
#> $dict$N
#> $dict$N$Y
#> $dict$N$Y$`_word_`
#> [1] "New York"
#> 
#> 
#> 
#> $dict$L
#> $dict$L$A
#> $dict$L$A$`_word_`
#> [1] "Los Angeles"
#> 
#> 
#> 
#> 
#> $dict_size
#> [1] 2
```

### Find keys in a sentence

``` r
words_found <- processor$find_keys(sentence = "I live in LA and I like NY")
words_found
#> [[1]]
#> [[1]]$word
#> [1] "Los Angeles"
#> 
#> [[1]]$start
#> [1] 11
#> 
#> [[1]]$end
#> [1] 13
#> 
#> 
#> [[2]]
#> [[2]]$word
#> [1] "New York"
#> 
#> [[2]]$start
#> [1] 25
#> 
#> [[2]]$end
#> [1] 26
do.call(rbind, words_found)
#>      word          start end
#> [1,] "Los Angeles" 11    13 
#> [2,] "New York"    25    26
```

### Replace keys in a sentence

``` r
processor$replace_keys(sentence = "I live in LA and I like NY")
#> [1] "I live in Los Angeles and I like New York"
```

To see more details about the performance of the algorithm, click
[here](https://github.com/AbrJA/rflashtext_benchmark.git).
