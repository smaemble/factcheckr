# ============= ggplot3() Test ====================


output <- neatlystart("Texas A&M has the best Statistical Learning Program in the nation.", "Texas AM")
output2 <- neatlystart("Mit is very expensive, student loans sucks", "MIT")


nrcResult <- combinesubjects(list(output, output2), lex="nrc")
bingResult <- combinesubjects(list(output, output2), lex="bing")

#ggplot3(nrcResult, top=10, "frequency")

testthat::test_that("top must not be less than 5", {
  testthat::expect_error(ggplot3(nrcResult, top=4, "frequency"))
  testthat::expect_error(ggplot3(nrcResult=NULL))
})
