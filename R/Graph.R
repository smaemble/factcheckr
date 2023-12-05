
#' This function will provide the most frequently use words in a text plot.
#' Display emotions by subject and re-level sentiment so that the different core emotions are
#              ordered from more negative (red) to more positive (blue)
#'
#' @param text   - text to use to produce the graph, subject to draw emotion from
#' @param top - indicated the limit to return after the operation is complete
#' @param embs - emotion by subject Boolean graph boolean flag if true
#' @param graphType - the type of graph to plot
#'
#' @return will plot the graph type passed to the function
#' @export
#'
#' @seealso \code{\link{combinesubjects()}}, \code{\link{emotionFrequency()}}, \code{\link{topterms()}}
#' @examples
#'
#' annotations <- combinesubjects(list(output, output2), lex="nrc")
#'
#' emotions <- emotionFrequency(annotations)
#'
#' ggplot3(text=annotations)
#'
#' ggplot3(text=emotions, graphType="emotion", embs=TRUE)
#'
#' ggplot3(text=topwords, graphType="topterms")
#'
#' ggplot3(text=annotations, graphType="movingaverage")
#'
#' ggplot3(text=emotions, graphType="polarity")
#'
ggplot3 <- function(text, graphType = "frequency", top = 10, embs = FALSE) {
   GRAPH_TYPE = c("frequency", "emotion", "topterms", "movingaverage", "polarity", "wordcloud")
   type <- match.arg(graphType, GRAPH_TYPE)

   print(paste("ggplot3 call, Selected type:", type))

   if(type == "frequency") {
     .frequencyplot(x=text, top)
   } else if(type == "emotion") {
     .emotionplot(text, embs)
   } else if(type == "topterms") {
     .toptermplot(topwords=text)
   } else if(type == "movingaverage") {
     .magplot(movingAverage=text)
   }else if(type == "polarity") {
     .polarityplot(subjects=text)
   }else if(type == "wordcloud") {
     #.magplot(movingAverage=text)
   }

}


.frequencyplot <- function(x, top = 10) {
    if(identical(x, NULL)) {
      stop(paste("x cannot be null", x))
    }

    if(top < 5) {
      stop(paste("top cannot be less than 5", top))
    }

    x %>%

    # We need a word count
    count(Word, sort = TRUE) %>%

    # We want to create a factor from the word column with the levels showing the most frequent words as top level
    # This is just for aestethic reasons, however, it helps make the point
    dplyr::mutate(Word = factor(Word, levels = rev(unique(Word)))) %>%
    # We use the "top" variable defined in the function so we can decide how many words we want to use
    top_n(top) %>%

    # Could be useful if grouping variable is necessary
    ungroup() %>%

    # The graph itself
    ggplot2::ggplot(mapping = ggplot2::aes(x = Word, y = n)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL)
}



# visualize the results and show the scores for each core emotion by subjects(Products)
# subjects - The subjects to draw emotions from.
.emotionplot <- function(subjects, embs = FALSE) {

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



# Helper function to visualize the top n words for the core emotion categories.
# topwords - top words by emotion or top terms
.toptermplot <- function(topwords, show_legend=FALSE) {
    if(identical(topwords, NULL)){
      stop("topwords cannot NULL")
    }
    topwords %>%
    dplyr::group_by(Subject) %>%
    slice_max(score, n = 20) %>%
    dplyr::arrange(desc(score)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(Word, score), y = score, fill = Word)) +
    ggplot2::facet_wrap(Subject~sentiment, ncol = 4, scales = "free_y") +
    ggplot2::geom_col(show.legend = show_legend) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Words")
}

# The moving average plot of polarity change overtime
# movingAverage - the polarity moving average
.magplot <- function(movingAverage){
  ggplot2::ggplot(movingAverage, ggplot2::aes(id, rmean)) +
    ggplot2::facet_wrap(vars(Subject), scales="free_x") +
    ggplot2::geom_smooth(se = F, col = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "polarity ratio (rolling mean, k = 100)",
         x = "index (word in monograph)")
}

#polarity graph plot function
.polarityplot <- function(subjects) {
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
    ggplot2::ggplot(ggplot2::aes(reorder(Subject, polarity, mean), polarity, fill = Subject)) +
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



wcgplot <- function(text, lexicon = c("afinn", "bing", "loughran", "nrc"), maxWords = 50, sort_default=TRUE) {
  lex <- match.arg(lexicon)

  text %>%
    inner_join(genSentiment(lex), by = c("Word" = "word")) %>%
    count(Word, sentiment, sort = sort_default) %>%
    reshape2::acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
    # colors = c("#202121", "#797C80")
    comparison.cloud(colors = c("red" , "dark green"), max.words = maxWords)
}


