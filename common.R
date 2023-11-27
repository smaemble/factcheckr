
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

tokenize_all <- function(text, what=c("word", "sentence"), remove_numbers = FALSE, remove_punct = FALSE, 
                remove_symbols = FALSE, remove_separators = TRUE, remove_hyphens = FALSE, remove_url = FALSE) {
  
  lex <- match.arg(lexicon)
  
  return(c(0))
}

# This function will provide the most frequently use words in a text
# x - text to parse 
# top - indicated the limit to return after the operation is complete
# show_graph - is TRUE a Graph will be created as sidde effect
word_frequency <- function(x, top = 10){
  
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
  ggplot(mapping = aes(x = Word, y = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL)

}




# Define a custom function to remove stop words
remove_stop_words <- function(text, additionalwords=NULL) {
  # Convert text to lowercase
  text <- tolower(text)
  
  # Split text into words
  words <- gsub("[^[:space:]]+", " ", text)
  
  # Create a set of stop words for faster lookup
  stop_words_set <- stop_words %>% pull(word) %>% unique()
  
  # Filter out stop words
  filtered_words <- words[!(words %in% stop_words_set)]
  
  # Paste filtered words back together into a string
  text <- paste(filtered_words, collapse = " ")
  
  return(text)
}


getSentiment <- function (lexicon = c("afinn", "bing", "loughran", "nrc"))  {
       data(list = "sentiments", package = "tidytext", envir = environment())
       lex <- match.arg(lexicon)
       if (lex == "afinn") {
           if (!requireNamespace("textdata", quietly = TRUE)) {
               stop("The textdata package is required to download the AFINN lexicon. \nInstall the textdata package to access this dataset.", 
                   call. = FALSE)
          }
           return(textdata::lexicon_afinn())
       }
      else if (lex == "nrc") {
         if (!requireNamespace("textdata", quietly = TRUE)) {
             stop("The textdata package is required to download the NRC word-emotion association lexicon. \nInstall the textdata package to access this dataset.", 
                 call. = FALSE)
         }
         return(textdata::lexicon_nrc())
     }
     else if (lex == "loughran") {
         if (!requireNamespace("textdata", quietly = TRUE)) {
             stop("The textdata package is required to download the Loughran-McDonald lexicon. \nInstall the textdata package to access this dataset.", 
                 call. = FALSE)
         }
         return(textdata::lexicon_loughran())
     }
     else if (lex == "bing") {
         return(sentiments)
     }
 }



