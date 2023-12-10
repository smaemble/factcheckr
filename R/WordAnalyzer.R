#' Combine subject using Word Emotion Association Lexicon
#'
#' @param list_of_dfs - List of data frames subject for each product under fact checking
#' @param lex - the lexicon to use.
#' @return subjects combined
#' @export
#'
#' @examples
#' # list_of_dfs is mainly a list of neatlyStart output results.
#' # default lex value are c("afinn", "bing", "loughran", "nrc")
#'
#' output <- neatlyStart("Texas A&M has the best Statistical Learning Program in the nation.", "Texas AM")
#'
#' output2 <- neatlyStart("Mit is very expensive, student loans sucks", "MIT")
#'
#' annotations <- combineSubjects(list(output, output2), lex="nrc")
#'
#' # A tibble: 3 × 4
#' # Groups:   Subject [1]
#' # Word        Subject  words sentiment
#' # <chr>       <fct>    <int> <fct>
#' # statistical Texas AM     5 trust
#' # learning    Texas AM     5 positive
#' #  nation     Texas AM     5 trust
#'
#' annotations <- factcheckr::combineSubjects(list(output, output2), lex="bing")
#'
#' # Word      Subject words sentiment
#' # <chr>     <fct>   <int> <fct>
#' # expensive MIT         5 negative
#' # sucks     MIT         5 negative
combineSubjects <- function(list_of_dfs, lex = "nrc"){

  if(identical(list_of_dfs, NULL)) {
    stop ("list_of_dfs cannot be LULL")
  }

  base::do.call(rbind, list_of_dfs) %>%

  # Groupby the subject column created during the cleaning phase
  dplyr::group_by(Subject) %>%

  #provide a score for each words
  dplyr::mutate(words = n()) %>%
  dplyr::inner_join(loadlexicon(lex), by = c("Word" = "word")) %>%

  # create the sentiment column using the lexicon words to descripte emotion
  dplyr::mutate(Subject = factor(Subject), sentiment = factor(sentiment))
}


#' Load the correct lexicon
#'
#' @param lexicon -  lexicon type to find
#'
#' @return the lexicon
#' @export
#'
#' @examples
#'
#' loadlexicon(lexicon ="nrc")
#'
loadlexicon <- function (lexicon ="nrc")  {

     LEXICON_DEFAULT = c("bing", "loughran", "nrc")
     data(list = "sentiments", package = "tidytext", envir = environment())
     lex <- match.arg(lexicon, LEXICON_DEFAULT)

     if (lex == "nrc") {
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



#' Summarizes the results of the Sentiment Analysis and calculate the percentages of the
#' prevalence of emotions across different subjects(Products) under analysis.
#'
#' @param subjectsAnnotations - the annotation of different emotion by subject in this corpus
#'
#' @return emotion emotion frequency
#' @export
#'
#' @seealso \code{\link{combineSubjects}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#' @examples
#'
#'  out <- emotionFrequency(subjectsAnnotations)
#'
#' # Subject  sentiment sentiment_freq words percentage
#' # <fct>    <fct>              <int> <int>      <dbl>
#' #  Texas AM positive             1     5         20
#' #  Texas AM trust                2     5         40
emotionFrequency <- function(subjectsAnnotations) {

  if(identical(subjectsAnnotations, NULL)) {
    stop ("Annotation cannot be null")
  }
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


#' We currently examine the words that have influenced the emotionality scores.
#' In simpler terms, we explore which words carry the most significance for
#' the emotion scores within each subject. To enhance interpretability, we will exclude
#' specific core emotion categories as well as polarity.
#'
#' @param subjects_annotation - the subject annotation
#' @param min_top_words - the number of top words to return if any
#'
#' @return topwords associated with the lexicon
#'
#' @export
#' @seealso \code{\link{combineSubjects}}, \code{\link{emotionFrequency}}, \code{\link{topterms}},
#'          \code{\link{polarityChange}}
#' @examples
#'
#'
#'  out <- topterms (subjects_annotation)
#'
#' # Groups:   Subject, sentiment [1]
#' # Subject  Word        sentiment     n score
#' # <fct>    <chr>       <fct>     <int> <dbl>
#' # Texas AM nation      trust         1   0.5
#' # Texas AM statistical trust         1   0.5
topterms <- function(subjects_annotation, min_top_words = 4){

  if(identical(subjects_annotation, NULL)){
    stop("subjects_annotation cannot be NULL")
  }
  if(!is.numeric(min_top_words) || (min_top_words < 1)){
    stop("min_top_words must be a positive numeric")
  }
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
    dplyr::top_n(min_top_words) %>%
    dplyr::mutate(score = n/sum(n))
}


#' An alternative approach to monitoring shifts in polarity across time involves
#' computing rolling or moving averages. It is important to recognize, that while
#' rolling averages are not the most effective means for monitoring temporal changes,
#' they serve as a technique for smoothing out erratic time-series data.
#'
#'
#' @param subjects_annotation - subject annotation to find polarity changes over time
#'
#' @return the polarity changes over time.
#' @export
#'
#' @seealso \code{\link{combineSubjects}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#'          \code{\link{polarityChange}}
#'
#' @examples
#'
#'  out <- polarityChange (subjects_annotation = combineSubjects(list(subject1, subject2, ...), lex="nrc"))
#'
#' # Groups:   Subject, sentiment [1]
#' # Subject, id ,  rmean
#' # <fct>   <int>  <lgl>
polarityChange <- function(subjects_annotation) {

  if(identical(subjects_annotation, NULL)){
    stop("subjects_annotation cannot be NULL")
  }

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





