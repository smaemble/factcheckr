
# Turn a text into tokens.
# text  - text to be tokenized
# words - an arrays of tokens to return
tokenizeWords <- function(text) {
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
tokenizeSentences <- function(text) {
  # Convert the text to a character vector
  text <- as.character(text)
  
  # Split the text into individual sentences
  sentences <- strsplit(text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T)
  
  # Remove any empty sentences
  sentences <- sentences[!is.null(sentences)]
  
  # Return the tokenized sentences
  return(unlist(sentences))
}

tokenizeCorpus <- function(corpus, what=c("word", "sentence"), remove_numbers = FALSE, remove_punct = FALSE, 
                remove_symbols = FALSE, remove_separators = TRUE, remove_hyphens = FALSE, remove_url = FALSE) {
  if(is.null(corpus)){
    stop("corpus cannot be NULL")
  }
  
  if(!is.null(what)){
    token_type <- match.arg(what)
    
    if (token_type == "word") {
      corpus <- tokenizeWords(corpus)
    }
    else if (token_type == "sentence") {
      corpus <- tokenizeSentences(corpus)
    }
  }
  
  # Remove numbers using gsub
  if(remove_numbers) {
    corpus <- gsub("\\d", "", corpus)
  }
  
  return(corpus)
}

# This function will provide the most frequently use words in a text
# x - text to parse 
# top - indicated the limit to return after the operation is complete
# show_graph - is TRUE a Graph will be created as sidde effect
frequencyGraph <- function(x, top = 10) {
  
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


genSentiment <- function (lexicon = c("afinn", "bing", "loughran", "nrc"))  {
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

# x - the corpus to clean
# title - Column title to be added into the corpus analysis
# Side effect, this clean function will also remove stop words
# This function is expected to return words
cleanTxt <- function(x, title){
  require(dplyr)
  require(stringr)
  require(tibble)
  
  x <- x %>%
    iconv(to = "UTF-8") %>%
    base::tolower() %>%
    paste0(collapse = " ") %>%
    stringr::str_squish()%>%
    # split the corpus to create words by calling our tokenize function
    tokenizeWords %>%
    # Convert object to one dimensional vector
    unlist() %>%
    tibble::tibble() %>%
    dplyr::select(Word = 1, everything()) %>%
    # Add a new column Subject
    dplyr::mutate(Subject = title) %>%
    # Remove all stop words
    dplyr::anti_join(stop_words, by = c("Word" = "word")) %>%
    # Remove any non words
    dplyr::mutate(Word = str_remove_all(Word, "\\W")) %>%
    # filtered empty space
    dplyr::filter(Word != "")
}


# list_of_dfs - List of data frames for each product reviews 
# The analysis can be done in as many as 4 products simultaneously
# Combine Word Emotion Association Lexicon
combineEmotion <- function(list_of_dfs){
  # Combine the arrayOfWords vector
  if(length(list_of_dfs) > 4){
    stop ("Currently supporting 4 products reviews simultaneously")
  }
  #rbind(arrayOfWords) %>%
  do.call(rbind, list_of_dfs) %>%
  # Groupby the subject column created during the cleaning phase  
  dplyr::group_by(Subject) %>%
  #provide a score for each words
  dplyr::mutate(words = n()) %>%
  dplyr::inner_join(genSentiment("nrc"), by = c("Word" = "word")) %>%
  # create the sentment column using the lexicon words to descripte emotion  
  dplyr::mutate(Subject = factor(Subject), sentiment = factor(sentiment))
}

# emotions - the annotation of different emotion in the subject
# Summarize the results of the Sentiment Analysis and calculate the percentages of the 
# prevalence of emotions across different subjects(Products) under analysis.
annotateSubjects <- function(emotions) {
  emotions %>%
  dplyr::group_by(Subject) %>%
  dplyr::group_by(Subject, sentiment) %>%
  dplyr::summarise(sentiment = unique(sentiment),
                   sentiment_freq = n(),
                   words = unique(words)) %>%
  # filter out any sentiment that has NA  
  dplyr::filter(is.na(sentiment) == F) %>%
    
  # create a new column name percentage that has each emotion frequency  
  dplyr::mutate(percentage = round(sentiment_freq/words*100, 1))
}

# visualize the results and show the scores for each core emotion by subjects(Products)
# subjects - The subjects to draw emotions from.
# display the emotions by subject and re-level sentiment so that the different core emotions are 
#              ordered from more negative (red) to more positive (blue)
emotionGraph <- function(subjects, emotions_by_subject = FALSE) {
  if(emotions_by_subject) {
    subjects %>%
    dplyr::filter(sentiment != "positive",
                  sentiment != "negative") %>%
      dplyr::mutate(sentiment = factor(sentiment, 
                                       levels = c("anger", "fear", "disgust", "sadness",
                                                  "surprise", "anticipation", "trust", "joy"))) %>%
      ggplot(aes(Subject, percentage, fill = sentiment)) +    
      geom_bar(stat="identity", position=position_dodge()) + 
      scale_fill_brewer(palette = "RdBu") +
      theme_bw() +
      theme(legend.position = "right") +
      coord_flip()
    
  } else {
    
    dplyr::filter(sentiment != "positive",
                  sentiment != "negative") %>%
    ggplot(aes(sentiment, percentage, fill = Subject)) +    
    geom_bar(stat="identity",   
             position=position_dodge()) + 
    scale_fill_manual(name = "", values=c("gray70", "orange", "red", "grey30")) +
    theme_bw() +
    theme(legend.position = "top")
  }
}

  




