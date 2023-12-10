# ============= ggplot3() Test ====================


output <- neatlyStart("Texas A&M has the best Statistical Learning Program in the nation.", "Texas AM")
output2 <- neatlyStart("Mit is very expensive, student loans sucks", "MIT")


nrcResult <- combineSubjects(list(output, output2), lex="nrc")
bingResult <- combineSubjects(list(output, output2), lex="bing")

#ggplot3(reviews, top=60, graphType="frequency")

testthat::test_that("top must not be less than 5", {
  testthat::expect_error(ggplot3(nrcResult, top=4, graphType="frequency"))
  testthat::expect_error(ggplot3(nrcResult=NULL))
})


emotionsEdu <- emotionFrequency(nrcResult)
ggplot3(nrcResult)
ggplot3(text = emotionsEdu, graphType ="emotion", embs = TRUE)
ggplot3(text = emotionsEdu, graphType ="emotion", embs = FALSE)

testthat::test_that("emotion plot", {
  testthat::expect_error(ggplot3(nrcResult=NULL, graphType ="emotion"))
  testthat::expect_error(ggplot3(nrcResult=emotions, graphType ="invalid"))
})

#
topwords <- topterms(nrcResult)
#topwords <- topterms(subjectsAnnotations)
ggplot3(text = topwords, graphType ="topterms")
testthat::test_that("emotion plot", {
  testthat::expect_error(ggplot3(text = NULL, graphType ="topterms"))
})

