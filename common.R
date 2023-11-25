
# Turn a text into tokens.
# text  - text to be tokenized
# words - an arrays of tokens to return
tokenize_words <- function(text) {
  # Remove non-alphanumeric characters
  text <- gsub("[^[:alnum:]]", " ", text)
  
  # Convert text to lowercase
  text <- tolower(text)
  
  # Split text into words
  words <- strsplit(text, " ")
  
  # Remove empty strings
  words <- sapply(words, function(x) x[!x == ""])
  
  # Return the tokenized text
  return(words)
}


# Turn any text into sentences.
# text  - text to be tokenized into sentences
# words - an arrays of tokens sentences
tokenize_sentences <- function(text) {
  # Convert the text to a character vector
  text <- as.character(text)
  
  # Split the text into individual sentences
  sentences <- strsplit(text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T)
  
  # Remove any empty sentences
  sentences <- sentences[!is.null(sentences)]
  
  # Return the tokenized sentences
  return(unlist(sentences))
}

tokenize <- function(text, what=c("word", "sentence"), remove_numbers = FALSE, remove_punct = FALSE, remove_symbols = FALSE, 
                     remove_separators = TRUE, remove_hyphens = FALSE, remove_url = FALSE) {
  
  return(c(0))
}

# This function will provide the most frequently use words in a text
# x - text to parse 
# top - indicated the limit to return after the operation is complete
# show_graph - is TRUE a Graph will be created as sidde effect
word_frequency <- function(x, top = 10, show_graph=FALSE){
  
  x %>%
    
  # We need a word count
  count(Word, sort = TRUE) %>%
  
  # We want to create a factor from the word column with the levels showing the most frequent words as top level
  # This is just for aestethic reasons, however, it helps make the point
  mutate(Word = factor(Word, levels = rev(unique(Word)))) %>% 
  # We use the "top" variable defined in the function so we can decide how many words we want to use 
  top_n(top) %>%
  
  # Could be useful if grouping variable is necessary
  ungroup() %>%
  
  # The graph itself
  if(showGraph) {
    ggplot(mapping = aes(x = Word, y = n)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = NULL)
  }
}




remove_stop_words <- function(text, additional=stopwords) {
  # Convert text to lowercase
  text <- tolower(text)
  
  # Split text into words
  words <- gsub("[^[:space:]]+", " ", text)
  
  # Remove stopwords
  words <- words[!words %in% stopwords]
  
  # Paste words back together into a string
  text <- paste(words, collapse = " ")
  
  return(text)
}



