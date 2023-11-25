
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

#unlist(strsplit(string, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T))



stem_text <- function(text) {
  # Convert the text to lowercase
  text <- tolower(text)
  
  # Remove common prefixes
  prefixes <- c("un-", "re-", "pre-", "dis-", "mis-", "in-", "ex-")
  for (prefix in prefixes) {
    text <- gsub(prefix, "", text)
  }
  
  # Remove common suffixes
  suffixes <- c("-able", "-ible", "-ative", "-ive", "-ize", "-ize", "-ify", "-ify")
  for (suffix in suffixes) {
    text <- gsub(suffix, "", text)
  }
  
  # Remove common suffixes (ing, er, s)
  stemmed_text <- gsub("ing\\b", "", text)
  stemmed_text <- gsub("er\\b", "", stemmed_text)
  stemmed_text <- gsub("s\\b", "", stemmed_text)
  
  # Remove any remaining punctuation
  text <- gsub("[^[:alnum:]]", "", text)
  
  # Return the stemmed text
  return(text)
}


