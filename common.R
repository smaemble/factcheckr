


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


# list_of_dfs - List of data frames for each product reviews
# The analysis can be done in as many as 4 products simultaneously
# Combine Word Emotion Association Lexicon
combineSubjects <- function(list_of_dfs){
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
emotionPrevalenceBySubjects <- function(subjectsAnnotations) {
  subjectsAnnotations %>%
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

# Check words that have contributed to the emotionality of scores.
# In other words, we investigate, which words are more important for the emotion scores within each subject.
# For the sake of interpretability, we will remove several core emotion categories and also the polarity.
topWordsByEmotion <- function(subjects_annotation, top_words = 4){
  subjects_annotation %>%
    dplyr::filter(!is.na(sentiment),
                  sentiment != "anticipation",
                  sentiment != "surprise",
                  sentiment != "disgust",
                  sentiment != "negative",
                  sentiment != "sadness",
                  sentiment != "positive") %>%
    dplyr::mutate(sentiment = factor(sentiment, levels = c("anger", "fear",  "trust", "joy"))) %>%
    dplyr::group_by(Subject) %>%
    dplyr::count(Word, sentiment, sort = TRUE) %>%
    dplyr::group_by(Subject, sentiment) %>%
    dplyr::top_n(top_words) %>%
    dplyr::mutate(score = n/sum(n))
}


# An alternative approach to monitoring shifts in polarity across time involves
# computing rolling or moving averages. It is important to recognize, though, that while
# rolling averages are not the most effective means for monitoring temporal changes,
# they serve as a technique for smoothing out erratic time-series data. Nevertheless,
# they can be employed to enhance the examination of alterations identified through binning
#
# subjects_annotation - the subject annotation can be obtained by calling the combineSubjects() function
#
polarityChangesOverTime <- function(subjects_annotation) {
  subjects_annotation %>%
  dplyr::filter(is.na(sentiment) | sentiment == "negative" | sentiment == "positive") %>%
  dplyr::group_by(Subject) %>%
  dplyr::mutate(sentiment = as.character(sentiment),
                sentiment = case_when(is.na(sentiment) ~ "0", TRUE ~ sentiment),
                sentiment = case_when(sentiment == "0" ~ 0,
                                      sentiment == "positive" ~ 1,
                                      TRUE ~ -1),
                id = 1:n()) %>%
  dplyr::reframe(id = id, rmean=zoo::rollapply(sentiment, 100, mean, align='right', fill=NA)) %>%
  na.omit()
}





