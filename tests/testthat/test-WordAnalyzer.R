# ============= combinesubjects() Test ====================
output <- neatlystart("Texas A&M has the best Statistical Learning Program in the nation.", "Texas AM")
output2 <- neatlystart("Mit is very expensive, student loans sucks", "MIT")


nrcResult <- combinesubjects(list(output, output2), lex="nrc")
bingResult <- combinesubjects(list(output, output2), lex="bing")

(nrcResult$sentiment[3])
first_level <- as.character(levels(nrcResult$sentiment[3])[2])
first_level

testthat::test_that("corpus annotation with nrc lexicon", {
  testthat::expect_equal(nrow(nrcResult), 3)
  testthat::expect_equal(ncol(nrcResult), 4)

  testthat::expect_equal(nrcResult$Word[1], "statistical")
  #This is a level
  sentiment <- levels(nrcResult$sentiment[1])
  testthat::expect_equal(as.character(sentiment[2]), "trust")


  sentiment <- levels(nrcResult$sentiment[2])
  testthat::expect_equal(nrcResult$Word[2], "learning")
  testthat::expect_equal(as.character(sentiment[1]), "positive")


  sentiment <- levels(nrcResult$sentiment[3])
  testthat::expect_equal(nrcResult$Word[3], "nation")
  testthat::expect_equal(as.character(sentiment[2]), "trust")

})
