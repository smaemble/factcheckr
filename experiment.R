

reviews <- read.csv("https://raw.github.com/VladAluas/Text_Analysis/master/Datasets/Text_review.csv", header = TRUE, sep = ",")
#> Rows: 433
#> Columns: 3
#> Delimiter: ","
#> chr [3]: Model, Segment, Text
#>
#> Use `spec()` to retrieve the guessed column specification
#> Pass a specification to the `col_types` argument to quiet this message

head(reviews)



# We want to display graphically a word frequency plot
# We will create a function that will store all the operations we will repeat several times

# Same dataset as before with an extra code line

#stop_words <- c(stop_words, "oneplus")
reviews_tidy <- reviews %>%
  unnest_tokens("Word", "Text") %>%
  anti_join(stop_words, by = c("Word" = "word")) %>% # anti_join just keeps the rows common to both data sets
  mutate(Word = str_replace(Word, "'s", "")) %>%
  anti_join(data.frame(word = c("oneplus")), by = c("Word" = "word")) %>%
  group_by(Model) %>%
  word_frequency(5) +
  facet_wrap(~ Model, scales = "free_y") # This is just to split the graph into multiple graphs for each model
#> Selecting by n
#>
review_tf_idf <- reviews_tidy %>%
  count(Model, Word, sort = TRUE) %>%
  bind_tf_idf(Word, Model, n)
review_tf_idf %>%
  arrange(desc(tf_idf))



