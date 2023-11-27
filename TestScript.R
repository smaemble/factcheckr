
# Source functions for text mining
source("common.R")

tokens <- tokenize_words("This is an example of how to tokenize text in R.")

testthat::test_that("The text has excatly 11 words", {
  testthat::expect_equal(length(tokens), 11)
})

testthat::test_that("The space must be removed so the text has excatly 1", {
  testthat::expect_equal(length(tokenize_words("")), 1)
})

text <- "This is a very long character vector. Why is it so long? I think lng. is short for long. I want to split this vector into senteces by using e.g. strssplit. Can someone help me? That would be nice?"
out <- tokenize_sentences(text)
print(out)
testthat::test_that("There must be 6 sentences", {
  testthat::expect_equal(length(out), 6)
  testthat::expect_equal(out[1], "This is a very long character vector.")
  testthat::expect_equal(out[2], "Why is it so long?")
  testthat::expect_equal(out[3], "I think lng. is short for long.")
  testthat::expect_equal(out[4], "I want to split this vector into senteces by using e.g. strssplit.")
  testthat::expect_equal(out[5], "Can someone help me?")
  testthat::expect_equal(out[6], "That would be nice?")
})


# =========================
library(tidytext)
library(tidyverse)
# Load the text corpus
#corpus <- read_text("corpus.txt")
corpus <- "The quick brown fox jumps over the lazy dog. The dog is not amused"
class(corpus)

# Apply the custom function to each document in the corpus
corpus <- corpus %>% mutate(text = map(text, remove_stop_words))

#===============================

#library(dplyr)

# Example data frames
#stop_wordsw <- data.frame(Word = c("the", "and", "is", "it"))
your_data <- data.frame(word = c("apple", "banana", "is", "orange"))

# Anti-join on the columns with corrected capitalization
result <- anti_join(your_data, stop_words, by = c("word" = "Word"))

# Display the result
print(result)

