
#' This function will provide the most frequently use words in a text
#'
#' @param text   - text to use to produce the graph
#' @param top - indicated the limit to return after the operation is complete
#' @param emotions_by_subject - Boolean for dictating emotion graph plot
#' @param graphType - the type of graph to plot
#'
#' @return will plot the graph type passed to the function
#' @export
#'
#' @examples
#' ggplot3(nrcResult, top=10, "frequency")
ggplot3 <- function(text, top = 10, emotions_by_subject = FALSE,
    graphType = c("frequency", "emotion", "topterm", "movingaverage", "polarity", "wordcloud")) {

   type <- match.arg(graphType)

   if(type == "frequency") {
     .frequencyplot(text, top)
   }

}



.frequencyplot <- function(x, top = 10) {
    if(identical(x, NULL)) {
      stop("x cannot be null")
    }

    if(top < 5) {
      stop("top cannot be less than 5")
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
    ggplot2::ggplot(mapping = aes(x = Word, y = n)) +
    geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL)
}

#
#
# # visualize the results and show the scores for each core emotion by subjects(Products)
# # subjects - The subjects to draw emotions from.
# # display the emotions by subject and re-level sentiment so that the different core emotions are
# #              ordered from more negative (red) to more positive (blue)
# emotionGraph <- function(subjects, emotions_by_subject = FALSE) {
#   if(emotions_by_subject) {
#     subjects %>%
#       dplyr::filter(sentiment != "positive",
#                     sentiment != "negative") %>%
#       dplyr::mutate(sentiment = factor(sentiment,
#                                        levels = c("anger", "fear", "disgust", "sadness",
#                                                   "surprise", "anticipation", "trust", "joy"))) %>%
#       ggplot(aes(Subject, percentage, fill = sentiment)) +
#       geom_bar(stat="identity", position=position_dodge()) +
#       scale_fill_brewer(palette = "RdBu") +
#       theme_bw() +
#       theme(legend.position = "right") +
#       coord_flip()
#
#   } else {
#
#     dplyr::filter(sentiment != "positive",
#                   sentiment != "negative") %>%
#       ggplot(aes(sentiment, percentage, fill = Subject)) +
#       geom_bar(stat="identity",
#                position=position_dodge()) +
#       scale_fill_manual(name = "", values=c("gray70", "orange", "red", "grey30")) +
#       theme_bw() +
#       theme(legend.position = "top")
#   }
# }
#
#
#
# # Helper function to visualize the top n words for the core emotion categories.
# # topWordsByEmotion - top words by emotion
# # side effect, this function create graph
# # [ToDo] Fit LASSO on standardized data for a given lambda
# # Xtilde - centered and scaled X, n x p
# # Ytilde - centered Y, n x 1 (vector)
# # lamdba - tuning parameter
# # beta_start - p vector, an optional starting point for coordinate-descent algorithm
# # eps - precision level for convergence assessment, default 0.001
# emotionBySubjectGraph <- function(topWordsByEmotion) {
#   topWordsByEmotion %>%
#   dplyr::group_by(Subject) %>%
#   slice_max(score, n = 20) %>%
#   dplyr::arrange(desc(score)) %>%
#   dplyr::ungroup() %>%
#   ggplot(aes(x = reorder(Word, score), y = score, fill = Word)) +
#   facet_wrap(Subject~sentiment, ncol = 4, scales = "free_y") +
#   geom_col(show.legend = FALSE) +
#   coord_flip() +
#   labs(x = "Words")
# }
#
# # The moving average plot of polarity change overtime
# # movingAverage - the polarity moving average
#
# # [ToDo] Fit LASSO on standardized data for a given lambda
# # Xtilde - centered and scaled X, n x p
# # Ytilde - centered Y, n x 1 (vector)
# # lamdba - tuning parameter
# # beta_start - p vector, an optional starting point for coordinate-descent algorithm
# # eps - precision level for convergence assessment, default 0.001
# magplot <- function(movingAverage){
#   ggplot(movingAverage, aes(id, rmean)) +
#     facet_wrap(vars(Subject), scales="free_x") +
#     geom_smooth(se = F, col = "black") +
#     theme_bw() +
#     labs(y = "polarity ratio (rolling mean, k = 100)",
#          x = "index (word in monograph)")
# }
#
# #polarity graph plot function
# # [ToDo] Fit LASSO on standardized data for a given lambda
# # Xtilde - centered and scaled X, n x p
# # Ytilde - centered Y, n x 1 (vector)
# # lamdba - tuning parameter
# # beta_start - p vector, an optional starting point for coordinate-descent algorithm
# # eps - precision level for convergence assessment, default 0.001
# pcgplot <- function(subjects) {
#   subjects %>%
#     dplyr::filter(sentiment == "positive" | sentiment == "negative") %>%
#     dplyr::select(-percentage, -words) %>%
#     dplyr::mutate(sentiment_sum = sum(sentiment_freq),
#                   positive = sentiment_sum-sentiment_freq) %>%
#     dplyr::filter(sentiment != "positive") %>%
#     dplyr::rename(negative = sentiment_freq) %>%
#     dplyr::select(Subject, positive, negative) %>%
#     dplyr::group_by(Subject) %>%
#     dplyr::summarise(polarity = positive/negative) %>%
#     ggplot(aes(reorder(Subject, polarity, mean), polarity, fill = Subject)) +
#     geom_bar(stat = "identity") +
#     geom_text(aes(y = polarity-0.1, label = round(polarity, 2)),
#               color = "white", size = 4) +
#     theme_bw() +
#     labs(y = "Polarity\n(ration of positive to negative emitives)", x = "") +
#     coord_cartesian(y= c(0,2)) +
#     scale_y_continuous(breaks = seq(0,2,1),
#                        labels = c("more negative", "neutral", "more positive")) +
#     theme(legend.position = "none")
# }
#
#
# # [ToDo] Fit LASSO on standardized data for a given lambda
# # Xtilde - centered and scaled X, n x p
# # Ytilde - centered Y, n x 1 (vector)
# # lamdba - tuning parameter
# # beta_start - p vector, an optional starting point for coordinate-descent algorithm
# # eps - precision level for convergence assessment, default 0.001
# wcgplot <- function(text, lexicon = c("afinn", "bing", "loughran", "nrc"), maxWords = 50, sort_default=TRUE) {
#   lex <- match.arg(lexicon)
#
#   text %>%
#     inner_join(genSentiment(lex), by = c("Word" = "word")) %>%
#     count(Word, sentiment, sort = sort_default) %>%
#     reshape2::acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
#     # colors = c("#202121", "#797C80")
#     comparison.cloud(colors = c("red" , "dark green"), max.words = maxWords)
# }


