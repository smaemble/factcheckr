# ============= ggplot3() Test ====================


output <- neatlystart("Texas A&M has the best Statistical Learning Program in the nation.", "Texas AM")
output2 <- neatlystart("Mit is very expensive, student loans sucks", "MIT")


nrcResult <- combinesubjects(list(output, output2), lex="nrc")
bingResult <- combinesubjects(list(output, output2), lex="bing")

testthat::test_that("top must not be less than 5", {
  testthat::expect_error(ggplot3(nrcResult, top=4, graphType="frequency"))
  testthat::expect_error(ggplot3(nrcResult=NULL))
})



#ggplot3(subjectsAnnotations)

# emotions <- emotionFrequency(subjectsAnnotations)
# ggplot3(subjectsAnnotations)
# ggplot3(text = emotions, graphType ="emotion", embs = TRUE)
# ggplot3(text = emotions, graphType ="emotion", embs = FALSE)
#graphType = c("frequency", "emotion", "topterm", "movingaverage", "polarity", "wordcloud")

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

#polarity <- polaritychanges(subjectsAnnotations)
#ggplot3(text = polarity, graphType ="movingaverage")

#emotions <- emotionFrequency(subjectsAnnotations)
#ggplot3(text=emotions, graphType="polarity")

#emotionsEdu <- emotionFrequency(bingResult)
#ggplot3(text=emotionsEdu, graphType="polarity")
