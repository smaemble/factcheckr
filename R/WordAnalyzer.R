#' Combine subject using Word Emotion Association Lexicon
#'
#' @param list_of_dfs - List of data frames subject for each product under fact checking
#'
#' @return subjects combined
#' @export
#'
#' @examples
#' # list_of_dfs is mainly a list of neatlystart output results.
#' # default lex value are c("afinn", "bing", "loughran", "nrc")
#' output <- factcheckr::neatlystart("Texas A&M has the best Statistical Learning Program in the nation.", "Texas AM")
#'
#' output2 <- factcheckr::neatlystart("Mit is very expensive, student loans sucks", "MIT")
#'
#' annotations <- factcheckr::combinesubjects(list(output, output2), lex="nrc")
#'
#' # A tibble: 3 × 4
#' # Groups:   Subject [1]
#' # Word        Subject  words sentiment
#' # <chr>       <fct>    <int> <fct>
#' # statistical Texas AM     5 trust
#' # learning    Texas AM     5 positive
#' #  nation     Texas AM     5 trust
#'
#' annotations <- factcheckr::combinesubjects(list(output, output2), lex="bing")
#'
#' # Word      Subject words sentiment
#' # <chr>     <fct>   <int> <fct>
#' # expensive MIT         5 negative
#' # sucks     MIT         5 negative
combinesubjects <- function(list_of_dfs, lex = "nrc"){

  if(identical(list_of_dfs, NULL)) {
    stop ("list_of_dfs cannot be LULL")
  }

  base::do.call(rbind, list_of_dfs) %>%

  # Groupby the subject column created during the cleaning phase
  dplyr::group_by(Subject) %>%

  #provide a score for each words
  dplyr::mutate(words = n()) %>%
  dplyr::inner_join(.loadlexicon(lex), by = c("Word" = "word")) %>%

  # create the sentiment column using the lexicon words to descripte emotion
  dplyr::mutate(Subject = factor(Subject), sentiment = factor(sentiment))
}




.loadlexicon <- function (lexicon = c("afinn", "bing", "loughran", "nrc"))  {

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


#
#
# # emotions - the annotation of different emotion in the subject
# # Summarize the results of the Sentiment Analysis and calculate the percentages of the
# # prevalence of emotions across different subjects(Products) under analysis.
# emotionPrevalenceBySubjects <- function(subjectsAnnotations) {
#   subjectsAnnotations %>%
#   dplyr::group_by(Subject) %>%
#   dplyr::group_by(Subject, sentiment) %>%
#   dplyr::summarise(sentiment = unique(sentiment),
#                    sentiment_freq = n(),
#                    words = unique(words)) %>%
#   # filter out any sentiment that has NA
#   dplyr::filter(is.na(sentiment) == F) %>%
#
#   # create a new column name percentage that has each emotion frequency
#   dplyr::mutate(percentage = round(sentiment_freq/words*100, 1))
# }
#
#
# # Check words that have contributed to the emotionality of scores.
# # In other words, we investigate, which words are more important for the emotion scores within each subject.
# # For the sake of interpretability, we will remove several core emotion categories and also the polarity.
# topWordsByEmotion <- function(subjects_annotation, top_words = 4){
#   subjects_annotation %>%
#     dplyr::filter(!is.na(sentiment),
#                   sentiment != "anticipation",
#                   sentiment != "surprise",
#                   sentiment != "disgust",
#                   sentiment != "negative",
#                   sentiment != "sadness",
#                   sentiment != "positive") %>%
#     dplyr::mutate(sentiment = factor(sentiment, levels = c("anger", "fear",  "trust", "joy"))) %>%
#     dplyr::group_by(Subject) %>%
#     dplyr::count(Word, sentiment, sort = TRUE) %>%
#     dplyr::group_by(Subject, sentiment) %>%
#     dplyr::top_n(top_words) %>%
#     dplyr::mutate(score = n/sum(n))
# }
#
#
# # An alternative approach to monitoring shifts in polarity across time involves
# # computing rolling or moving averages. It is important to recognize, though, that while
# # rolling averages are not the most effective means for monitoring temporal changes,
# # they serve as a technique for smoothing out erratic time-series data. Nevertheless,
# # they can be employed to enhance the examination of alterations identified through binning
# #
# # subjects_annotation - the subject annotation can be obtained by calling the combineSubjects() function
# #
# polarityChangesOverTime <- function(subjects_annotation) {
#   subjects_annotation %>%
#   dplyr::filter(is.na(sentiment) | sentiment == "negative" | sentiment == "positive") %>%
#   dplyr::group_by(Subject) %>%
#   dplyr::mutate(sentiment = as.character(sentiment),
#                 sentiment = case_when(is.na(sentiment) ~ "0", TRUE ~ sentiment),
#                 sentiment = case_when(sentiment == "0" ~ 0,
#                                       sentiment == "positive" ~ 1,
#                                       TRUE ~ -1),
#                 id = 1:n()) %>%
#   dplyr::reframe(id = id, rmean=zoo::rollapply(sentiment, 100, mean, align='right', fill=NA)) %>%
#   na.omit()
# }





