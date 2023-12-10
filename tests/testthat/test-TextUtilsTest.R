# ============= strtokwords function tests ====================
testthat::test_that("The text has excatly 11 words", {
  testthat::expect_length(strtokwords("This is an example of how to tokenize text in R."), 11)
})

testthat::test_that("The space must be removed so the text has excatly 1", {
  testthat::expect_equal(length(strtokwords("")), 1)
})

# ============= strtoksentence function tests ====================
text <- "This is a very long character vector. Why is it so long? I think lng. is short for long. I want to split this vector into senteces by using e.g. strssplit. Can someone help me? That would be nice?"
out <- strtoksentence(text)
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
  testthat::expect_equal(trimText(text, left=T), "This   is  String  ")
})

testthat::test_that("should remove space right", {
  testthat::expect_equal(trimText(text, right=T), "  This   is  String")
})

testthat::test_that("should remove space right and left", {
  testthat::expect_equal(trimText(text, left=T, right=T), "This   is  String")
})

testthat::test_that("trimText should expect error", {
  testthat::expect_error(trimText(text, left=NULL), "left and right variables must be boolean")
})


testthat::test_that("trimText should expect error", {
  testthat::expect_error(trimText(text, left=NULL), "left and right variables must be boolean")
})

testthat::test_that("trimText should expect error", {
  testthat::expect_error(trimText(NULL))
})


# ============= neatlyStart() ====================
out <- neatlyStart("Texas A&M has the best     Statistical Learning Program in the nation.  ", "University")

testthat::test_that("stop words and extra spaces must be removed and non english words", {
  testthat::expect_equal(length(out$Word), 5)
  testthat::expect_equal(out$Word[1], "texas")
  testthat::expect_equal(out$Word[2], "statistical")
  testthat::expect_equal(out$Word[3], "learning")
  testthat::expect_equal(out$Word[4], "program")
  testthat::expect_equal(out$Word[5], "nation")
})


# ============= removeStopwords() ====================
out <- removeStopwords(out, data.frame(word = c("texas")))

testthat::test_that("words texas must be removed", {
  testthat::expect_equal(length(out$Word), 4)
  testthat::expect_equal(out$Word[1], "statistical")
  testthat::expect_equal(out$Word[2], "learning")
  testthat::expect_equal(out$Word[3], "program")
  testthat::expect_equal(out$Word[4], "nation")
  testthat::expect_error(removeStopwords(words_dictionary=NULL))
  testthat::expect_error(removeStopwords(words_dictionary, stopwords=NULL))
})
