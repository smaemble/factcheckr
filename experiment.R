#rm(list=ls())

install.packages("tidytext")
install.packages("tidyverse")
library(tidytext)
library(tidyverse)
# We will need a URL from where to download the data
# In this case we will do it from my GitHub repository
# You can download the data using this link

# I prefer vroom to ingest csv, but you can use readr::read_csv() if you fancy it more

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

source("common.R")

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



review_tf_idf %>%
  # We need to sort the data in descending order so we can create the factors for each term
  arrange(desc(tf_idf)) %>%
  # We create the factors as we did previously
  mutate(Word = factor(Word, levels = rev(unique(Word)))) %>%
  # Select just the top 5 words for each model
  group_by(Model) %>%
  top_n(5) %>%
  ungroup() %>%
  # Our Plot
  ggplot(mapping = aes(x = Word, y = tf_idf, fill = Model)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  facet_wrap(~ Model, scales = "free_y")
#> Selecting by tf_idf
#> 

installed.packages("textdata")
library(textdata)


conclusion_bing %>%
  group_by(Model, sentiment) %>%
  count() %>%
  ungroup() %>%
  mutate(Model = reorder(Model, n)) %>%
  ggplot(mapping = aes(x = Model, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, y = "Negative vs positive sentiment / Model") +
  facet_wrap(~ sentiment, ncol = 2)



install.packages("wordcloud") 
library(wordcloud)
#> Loading required package: RColorBrewer
reviews_tidy %>%
  anti_join(data.frame(word = c("oneplus")), by = c("Word" = "word")) %>%
  count(Word) %>%
  with(wordcloud(Word, n, max.words = 50))


library(reshape2)
#> 
#> Attaching package: 'reshape2'
#> The following object is masked from 'package:tidyr':
#> 
#>     smiths
reviews_tidy %>%
  inner_join(get_sentiments("bing"), by = c("Word" = "word")) %>%
  count(Word, sentiment, sort = TRUE) %>%
  acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#202121", "#797C80"), max.words = 55)

# ==========

reviews_tidy %>% 
  inner_join(sentiment_type("bing"), by = c("Word" = "word")) %>% 
  count(Word, sentiment, sort=TRUE) %>%
  acast(Word ~ sentiment,value.var = "n" , fill = 0) %>%
  comparison.cloud(colors = c("red" , "dark green") , max.words = 50)



counting_words <- reviews_tidy %>% 
  inner_join(sentiment_type("bing"), by = c("Word" = "word")) %>% 
  count(Word,sentiment,sort=TRUE)

#head(counting_words)

counting_words %>% filter(n > 15) %>% mutate(n = ifelse(sentiment=="negative",-n,n)) %>%
  mutate(Word=reorder(Word,n)) %>%
  ggplot(aes(Word,n,fill=sentiment)) + geom_col() + coord_flip() + labs(y="Sentiment Score")



