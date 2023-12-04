# # ============= combinesubjects() Test ====================
# output <- neatlystart("Texas A&M has the best Statistical Learning Program in the nation.", "Texas AM")
# output2 <- neatlystart("Mit is very expensive, student loans sucks", "MIT")
#
#
# nrcResult <- combinesubjects(list(output, output2), lex="nrc")
# bingResult <- combinesubjects(list(output, output2), lex="bing")
#
# #first_level <- as.character(levels(nrcResult$sentiment[3])[2])
#
# testthat::test_that("corpus annotation with nrc lexicon", {
#   testthat::expect_equal(nrow(nrcResult), 3)
#   testthat::expect_equal(ncol(nrcResult), 4)
#
#   testthat::expect_equal(nrcResult$Word[1], "statistical")
#   #This is a level
#   sentiment <- levels(nrcResult$sentiment[1])
#   testthat::expect_equal(as.character(sentiment[2]), "trust")
#
#
#   sentiment <- levels(nrcResult$sentiment[2])
#   testthat::expect_equal(nrcResult$Word[2], "learning")
#   testthat::expect_equal(as.character(sentiment[1]), "positive")
#
#
#   sentiment <- levels(nrcResult$sentiment[3])
#   testthat::expect_equal(nrcResult$Word[3], "nation")
#   testthat::expect_equal(as.character(sentiment[2]), "trust")
#
# })
#
# # ============= emotionFrequency() ====================
# subjects <- factcheckr::emotionFrequency(nrcResult)
#
# testthat::test_that("emotion Frequency analysis with nrc lexicon", {
#   testthat::expect_equal(nrow(subjects), 2)
#   testthat::expect_equal(ncol(subjects), 5)
#
#   testthat::expect_equal(subjects$percentage[1], 20)
#   testthat::expect_equal(subjects$percentage[2], 40)
#
#   sentiment <- as.character(levels(subjects$sentiment[1])[1])
#   testthat::expect_equal(sentiment, "positive")
#
#   (sentiment <- as.character(levels(subjects$sentiment[2])[2]))
#   testthat::expect_equal(sentiment, "trust")
#
# })
#
#
# # ============= topterms() ====================
# (topwords <- topterms(nrcResult))
#
# testthat::test_that("top terms with nrc lexicon", {
#   testthat::expect_equal(nrow(topwords), 2)
#   testthat::expect_equal(ncol(topwords), 5)
#
#   testthat::expect_equal(topwords$score[1], 0.5)
#   testthat::expect_equal(topwords$score[2], 0.5)
#
#   sentiment <- as.character(levels(topwords$sentiment[1])[3])
#   testthat::expect_equal(sentiment, "trust")
#
#   (sentiment <- as.character(levels(topwords$sentiment[2])[3]))
#   testthat::expect_equal(sentiment, "trust")
#
#   testthat::expect_error(topterms(nrcResult = NULL))
#   testthat::expect_error(topterms(nrcResult, min_top_words=0))
# })
#
#
# # ============= polaritychanges() ====================
# (emotionChanges <- polaritychanges(bingResult))
#
# testthat::test_that("there should be no emotion changes as the data is not enough", {
#   testthat::expect_equal(nrow(emotionChanges), 0)
#   testthat::expect_equal(ncol(emotionChanges), 3)
# })
