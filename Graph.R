

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


# Helper function to visualize the top n words for the core emotion categories.
# topWordsByEmotion - top words by emotion
# side effect, this function create graph
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
magplot <- function(movingAverage){
  ggplot(movingAverage, aes(id, rmean)) +    
    facet_wrap(vars(Subject), scales="free_x") +
    geom_smooth(se = F, col = "black") + 
    theme_bw() +
    labs(y = "polarity ratio (rolling mean, k = 100)",
         x = "index (word in monograph)")
}


