
source("R/TextUtils.R")

#tokens <- strtokws("This is an example of how to tokenize text in R.")

testthat::test_that("The text has excatly 11 words", {
  testthat::expect_length(strtokwords("This is an example of how to tokenize text in R."), 11)
})

testthat::test_that("The space must be removed so the text has excatly 1", {
  testthat::expect_equal(length(strtokwords("")), 1)
})

text <- "This is a very long character vector. Why is it so long? I think lng. is short for long. I want to split this vector into senteces by using e.g. strssplit. Can someone help me? That would be nice?"
out <- strtokss(text)
testthat::test_that("There must be 6 sentences", {
  testthat::expect_equal(length(out), 6)
  testthat::expect_equal(out[1], "This is a very long character vector.")
  testthat::expect_equal(out[2], "Why is it so long?")
  testthat::expect_equal(out[3], "I think lng. is short for long.")
  testthat::expect_equal(out[4], "I want to split this vector into senteces by using e.g. strssplit.")
  testthat::expect_equal(out[5], "Can someone help me?")
  testthat::expect_equal(out[6], "That would be nice?")
})

# ============= removing empty spaces function text ====================
text <- "  This   is  String  "
#identical(trimTextl(text), "This   is  String  ")

testthat::test_that("remove left space", {
  testthat::expect_equal(trimTextl(text), "This   is  String  ")
})
testthat::test_that("remove right space", {
  testthat::expect_equal(trimTextr(text), "  This   is  String")
})

testthat::test_that("should remove space left, right, midle", {
  testthat::expect_equal(trimText(text), "This is String")
})

testthat::test_that("should remove space left", {
  testthat::expect_equal(trimText(text, left=TRUE), "This   is  String  ")
})

testthat::test_that("should remove space right", {
  testthat::expect_equal(trimText(text, right=TRUE), "  This   is  String")
})

testthat::test_that("should remove space right and left", {
  testthat::expect_equal(trimText(text, left=TRUE, right=TRUE), "This   is  String")
})

testthat::test_that("trimText should expect error", {
  testthat::expect_error(trimText(text, left=NULL), "left and right variables must be boolean")
})

testthat::test_that("trimText should expect error", {
  testthat::expect_error(trimText(NULL))
})





