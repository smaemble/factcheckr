
#' ggplot3() function plot the words frequency,sentiment score, text emotion
#' using of the known lexicon, topterms, moving average which shows opinion changes
#' overtime, polarity and wordcloud. Display emotions by subject and re-level
#' sentiment so that the different core emotions are ordered from more negative
#' (red) to more positive (blue)
#'
#' @param text   - text to use to produce the graph, subject to draw emotion from
#' @param graphType - type of graph to plot: "frequency","sentiment", "emotion",
#'                    "topterms", "movingaverage","polarity", "wordcloud"
#'
#' @param top - indicated the limit to return after the operation is complete
#' @param embs - emotion by subject Boolean graph boolean flag
#' @param lexicon - lexicon to be used
#' @param maxWords - max number of words that could be used when drawing wordcloud
#' @param cutoffScore - Cut off score parameter useful for sentiment score plot
#'
#' @return plot the graph type passed to the function
#' @export
#'
#' @seealso \code{\link{combineSubjects}}, \code{\link{emotionFrequency}},
#'          \code{\link{topterms}}
#'
#' @examples
#'
#' output <- neatlyStart(corpus="Texas A&M has the best Statistical Learning Program in
#'                     the nation.", subject="Texas AM")
#' output2 <- neatlyStart(corpus="Mit is very expensive, student loans sucks",
#'                       subject="MIT")
#' nrcSubjects <- combineSubjects(list(output), lex="nrc")
#'
#' ggplot3(text=combineSubjects(list(output, output2), lex="nrc"))
#'
#' ggplot3(text=emotionFrequency(nrcSubjects), graphType="emotion", embs=TRUE)
#'
#' ggplot3(text=topterms(nrcSubjects), graphType="topterms")
#'
#' #ggplot3(text=polarityChange(nrcSubjects), graphType="movingaverage")
#'
#' ggplot3(text=emotionFrequency(nrcSubjects), graphType="polarity")
#'
#' ggplot3(text=neatlyStart(corpus=neatlyStart(corpus=ObamaVictorySpeech,
#' subject="Obama Speech"), subject="Comfort Suites"), graphType = "wordcloud",
#' lexicon="bing", maxWords = 50)
#'
#' ggplot3(text=neatlyStart(corpus=neatlyStart(corpus=ObamaVictorySpeech,
#' subject="Obama Speech"), subject="Comfort Suites"), graphType = "sentiment",
#' lexicon="bing", cutoffScore = 100)
#'
ggplot3 <- function(text=text, graphType = "frequency", top = 50, embs = FALSE,
                    lexicon="nrc", maxWords = 50, cutoffScore=100) {

   GRAPH_TYPE = c("frequency","sentiment", "emotion", "topterms", "movingaverage",
                  "polarity", "wordcloud")
   type <- match.arg(graphType, GRAPH_TYPE)

   print(paste("ggplot3 call, Selected Graph type:", type))

   if(type == "frequency") {
     frequencyPlot(x=text, top)

   } else if(type == "emotion") {
     emotionsPlot(text, embs)

   } else if(type == "topterms") {
     toptermplot(topwords=text)

   } else if(type == "movingaverage") {
     magplot(movingAverage=text)

   } else if(type == "polarity") {
     polarityPlot(subjects=text)

   } else if(type == "wordcloud") {
     wordcloudPlot(corpus=text, lexicon=lexicon, maxWords= maxWords)

   } else if(type == "sentiment") {
     sentimentPlot(x=text, lexicon=lexicon, cutoffScore=cutoffScore)
   }

}


#' Plot top Words frequency under the corpus
#'
#' @param x    - tibble of Word and subject columns created from neatlyStart function
#' @param top  - the number of word to show in the frequency plot
#'
#' @return the plot
#'
#'
#' @examples
#' corpus <- "Comfort Suites hotels in the Bahamas is dope. check it out"
#' frequencyPlot(x=neatlyStart(corpus=corpus, subject="Atlantis"), top = 100)
#'
frequencyPlot <- function(x, top = 10) {

    if(identical(x, NULL)) {
      stop(paste("x cannot be null", x))
    }

    if(top < 5) {
      stop(paste("top cannot be less than 5", top))
    }

    x %>%

    # We need a word count
    dplyr::count(Word, sort = TRUE) %>%

    # We want to create a factor from the word column with the levels showing the
    # most frequent words as top level
    # This is just for aestethic reasons, however, it helps make the point
    dplyr::mutate(Word = factor(Word, levels = rev(unique(Word)))) %>%
    # We use the "top" variable defined in the function so we can decide how many words we want to use
    dplyr::top_n(top) %>%

    # Could be useful if grouping variable is necessary
    dplyr::ungroup() %>%

    # The graph itself
    ggplot2::ggplot(mapping = ggplot2::aes(x = Word, y = n)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL)
}


#' Sentiment score visualization base on words score. It plots the scores along
#' the axis labeled with both positive as well as negative words.
#'
#' @param x             - A vector of words.
#' @param lexicon       - one of the lexicon (default is bing.)
#' @param cutoffScore   - cut off score to include in the plot
#'
#' @return              Sentiment Score plot
#'
#'
#' @examples
#'
#' sentimentPlot(x=neatlyStart(corpus=ObamaVictorySpeech, subject="Comfort Suites"))
#'
sentimentPlot <- function(x, lexicon="bing", cutoffScore = 100) {

    if(identical(x, NULL)) {
      stop(paste("x cannot be null", x))
    }

    data <- x %>%
      dplyr::inner_join(factcheckr::loadlexicon(lexicon), by=c("Word" = "word")) %>%
      dplyr::count(Word, sentiment, sort=TRUE)

    counting_words <- data %>%
      dplyr::filter(n > cutoffScore) %>%
      dplyr::mutate(n = ifelse(sentiment=="negative",-n,n)) %>%
      dplyr::mutate(Word=stats::reorder(Word,n))

    splot <- counting_words %>%
      ggplot2::ggplot(ggplot2::aes(Word, n, fill=sentiment)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(y="Sentiment Score")
    (splot)
}




#' visualize the results and show the scores for each core emotion by
#' subjects(Products)
#'
#' @param subjects - A tibble: with 5 columns (Subject,sentiment,sentiment_freq,
#'                    words, percentage)
#' @param embs - Boolean whether to show emotions by subject under study or not
#'
#' @return emotions plot
#'
#' @examples
#'
#' output <- neatlyStart(corpus=ObamaVictorySpeech, subject="MIT")
#'
#' nrcSubjects <- combineSubjects(list(output), lex="nrc")
#'
#' emotionsPlot(subjects=emotionFrequency(subjectsAnnotations=nrcSubjects),
#'  embs=TRUE)
#'
emotionsPlot <- function(subjects, embs = FALSE) {

  if(identical(subjects, NULL)){
    stop("text cannot be null")
  }

  if(embs) {
    subjects %>%
      dplyr::filter(sentiment != "positive",
                    sentiment != "negative") %>%
      dplyr::mutate(sentiment = factor(sentiment,
                                       levels = c("anger", "fear", "disgust", "sadness",
                                                  "surprise", "anticipation", "trust", "joy"))) %>%
      ggplot2::ggplot(ggplot2::aes(Subject, percentage, fill = sentiment)) +
      ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge()) +
      ggplot2::scale_fill_brewer(palette = "RdBu") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "right") +
      ggplot2::coord_flip()

  } else {
    subjects %>%
    dplyr::filter(sentiment != "positive",
                  sentiment != "negative") %>%
      ggplot2::ggplot(ggplot2::aes(sentiment, percentage, fill = Subject)) +
      ggplot2::geom_bar(stat="identity",
               position=ggplot2::position_dodge()) +
      ggplot2::scale_fill_manual(name = "", values=c("gray70", "orange", "red", "grey30")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top")
  }
}



#' Helper function to visualize the top n words for the core emotion categories.
#'
#'
#' @param topwords      - tibble of top words
#' @param show_legend  - boolean whether legend should be display in the plot or not
#'
#' @return tibble of top words by emotion
#'
#' @examples
#'
#' output <- neatlyStart(corpus=ObamaVictorySpeech, subject="Obama Speech")
#'
#' toptermplot(topwords=topterms(combineSubjects(list(output), lex="nrc")))
#'
toptermplot <- function(topwords, show_legend=FALSE) {

    if(identical(topwords, NULL)){
      stop("topwords cannot NULL")
    }
    topwords %>%
    dplyr::group_by(Subject) %>%
    slice_max(score, n = 20) %>%
    dplyr::arrange(desc(score)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(Word, score), y = score, fill = Word)) +
    ggplot2::facet_wrap(Subject~sentiment, ncol = 4, scales = "free_y") +
    ggplot2::geom_col(show.legend = show_legend) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Words")
}

#' The moving average plot of polarity change overtime
#' Showcase the rolling mean values to examine whether there are discernible
#' trends in how the polarity evolves throughout the subject under fact checking
#'
#' @param movingAverage - the polarity moving average
#'
#' @return the plot
#'
#'
#' @examples
#'
#' nsObamaSpeech <- neatlyStart(corpus=ObamaVictorySpeech, subject="Obama Speech")
#' nsTrumpSpeech <- neatlyStart(corpus=TrumpVictorySpeech, subject="Trump Speech")
#'
#' nrcSubjects <- combineSubjects(list(nsObamaSpeech, nsTrumpSpeech), lex="nrc")
#'
#' magplot(movingAverage=polarityChange(nrcSubjects))
#'
magplot <- function(movingAverage) {
    ggplot2::ggplot(movingAverage, ggplot2::aes(id, rmean)) +
    ggplot2::facet_wrap(vars(Subject), scales="free_x") +
    ggplot2::geom_smooth(se = F, col = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "polarity ratio (rolling mean, k = 100)",
         x = "index (word in the subject)")
}


#' Polarity visualization for each subject. This is the ratio of the number
#' of positive emotion words divided by the number of negative words
#'
#' @param subjects - subject under fact checking
#'
#' @return the plot
#'
#' @examples
#'
#' nsObamaSpeech <- neatlyStart(corpus=ObamaVictorySpeech, subject="Obama Speech")
#' nsTrumpSpeech <- neatlyStart(corpus=TrumpVictorySpeech, subject="Trump Speech")
#'
#' nrcSubjects <- combineSubjects(list(nsObamaSpeech, nsTrumpSpeech), lex="nrc")
#'
#' polarityPlot(subjects=emotionFrequency(subjectsAnnotations=nrcSubjects))
#'
polarityPlot <- function(subjects) {

  subjects %>%
    dplyr::filter(sentiment == "positive" | sentiment == "negative") %>%
    dplyr::select(-percentage, -words) %>%
    dplyr::mutate(sentiment_sum = sum(sentiment_freq),
                  positive = sentiment_sum-sentiment_freq) %>%
    dplyr::filter(sentiment != "positive") %>%
    dplyr::rename(negative = sentiment_freq) %>%
    dplyr::select(Subject, positive, negative) %>%
    dplyr::group_by(Subject) %>%
    dplyr::reframe(polarity = positive/negative) %>%
    ggplot2::ggplot(ggplot2::aes(stats::reorder(Subject, polarity, mean), polarity,
                                 fill = Subject)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(y = polarity-0.1, label = round(polarity, 2)),
              color = "white", size = 4) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Polarity\n(ration of positive to negative emitives)", x = "") +
    ggplot2::coord_cartesian(y= c(0,2)) +
    ggplot2::scale_y_continuous(breaks = seq(0,2,1),
                       labels = c("more negative", "neutral", "more positive")) +
    ggplot2::theme(legend.position = "none")
}



#' internal helper function to draw wordcloud
#'
#' @param corpus    - vector of words to plot wordcloud
#' @param lexicon   - lexicon to use. see documentation for different lexicon supported
#' @param maxWords  - max number of words to add to the word cloud
#'
#' @return the graph showing the wordcloud of the sentiment analysis
#'
#'
#' @examples
#'
#' wordcloudPlot(corpus=neatlyStart(corpus=ObamaVictorySpeech, subject="Comfort Suites"),
#' lexicon="nrc", maxWords = 50)
#'
wordcloudPlot <- function(corpus, lexicon="nrc", maxWords = 50) {

  if(identical(corpus, NULL)){
    stop("corpus cannot be NULL")
  }

  type <- match.arg(lexicon, c("bing", "loughran", "nrc"))

  corpus %>%
    dplyr::inner_join(factcheckr::loadlexicon(type), by = c("Word" = "word")) %>%
    dplyr::count(Word, sentiment, sort = TRUE) %>%
    reshape2::acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
    # colors = c("#202121", "#797C80")
    wordcloud::comparison.cloud(colors = c("red" , "dark green"),
                                scale=c(4, .4),
                                max.words = maxWords,
                                title.size=2.5)
}


