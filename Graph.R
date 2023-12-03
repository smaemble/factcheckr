# Helper function to visualize the top n words for the core emotion categories.
# topWordsByEmotion - top words by emotion
# side effect, this function create graph
# [ToDo] Fit LASSO on standardized data for a given lambda
# Xtilde - centered and scaled X, n x p
# Ytilde - centered Y, n x 1 (vector)
# lamdba - tuning parameter
# beta_start - p vector, an optional starting point for coordinate-descent algorithm
# eps - precision level for convergence assessment, default 0.001
emotionBySubjectGraph <- function(topWordsByEmotion) {
  topWordsByEmotion %>%
  dplyr::group_by(Subject) %>%
  slice_max(score, n = 20) %>%
  dplyr::arrange(desc(score)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = reorder(Word, score), y = score, fill = Word)) +
  facet_wrap(Subject~sentiment, ncol = 4, scales = "free_y") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Words")
}

# The moving average plot of polarity change overtime
# movingAverage - the polarity moving average

# [ToDo] Fit LASSO on standardized data for a given lambda
# Xtilde - centered and scaled X, n x p
# Ytilde - centered Y, n x 1 (vector)
# lamdba - tuning parameter
# beta_start - p vector, an optional starting point for coordinate-descent algorithm
# eps - precision level for convergence assessment, default 0.001
magplot <- function(movingAverage){
  ggplot(movingAverage, aes(id, rmean)) +
    facet_wrap(vars(Subject), scales="free_x") +
    geom_smooth(se = F, col = "black") +
    theme_bw() +
    labs(y = "polarity ratio (rolling mean, k = 100)",
         x = "index (word in monograph)")
}

#polarity graph plot function
# [ToDo] Fit LASSO on standardized data for a given lambda
# Xtilde - centered and scaled X, n x p
# Ytilde - centered Y, n x 1 (vector)
# lamdba - tuning parameter
# beta_start - p vector, an optional starting point for coordinate-descent algorithm
# eps - precision level for convergence assessment, default 0.001
pcgplot <- function(subjects) {
  subjects %>%
    dplyr::filter(sentiment == "positive" | sentiment == "negative") %>%
    dplyr::select(-percentage, -words) %>%
    dplyr::mutate(sentiment_sum = sum(sentiment_freq),
                  positive = sentiment_sum-sentiment_freq) %>%
    dplyr::filter(sentiment != "positive") %>%
    dplyr::rename(negative = sentiment_freq) %>%
    dplyr::select(Subject, positive, negative) %>%
    dplyr::group_by(Subject) %>%
    dplyr::summarise(polarity = positive/negative) %>%
    ggplot(aes(reorder(Subject, polarity, mean), polarity, fill = Subject)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = polarity-0.1, label = round(polarity, 2)),
              color = "white", size = 4) +
    theme_bw() +
    labs(y = "Polarity\n(ration of positive to negative emitives)", x = "") +
    coord_cartesian(y= c(0,2)) +
    scale_y_continuous(breaks = seq(0,2,1),
                       labels = c("more negative", "neutral", "more positive")) +
    theme(legend.position = "none")
}


# [ToDo] Fit LASSO on standardized data for a given lambda
# Xtilde - centered and scaled X, n x p
# Ytilde - centered Y, n x 1 (vector)
# lamdba - tuning parameter
# beta_start - p vector, an optional starting point for coordinate-descent algorithm
# eps - precision level for convergence assessment, default 0.001
wcgplot <- function(text, lexicon = c("afinn", "bing", "loughran", "nrc"), maxWords = 50, sort_default=TRUE) {
  lex <- match.arg(lexicon)

  text %>%
    inner_join(genSentiment(lex), by = c("Word" = "word")) %>%
    count(Word, sentiment, sort = sort_default) %>%
    reshape2::acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
    # colors = c("#202121", "#797C80")
    comparison.cloud(colors = c("red" , "dark green"), max.words = maxWords)
}


