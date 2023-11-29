

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