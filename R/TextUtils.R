#' trimText function to clean text before analysis
#' This function takes a text as input and returns a new text with all extra spaces removed
#'
#' @param str - text to remove extra spaces left, middle and right
#' @param leftOnly - boolean flag if extra spaces should be removed from the left only
#' @param rightOnly - boolean flag if extra spaces should be removed from the right only
#'
#' @return the text already with extra spaces removed
#' @export
#'
#' @examples
#' trimText("  This   is  String  ")
#' # Output: "This is String"
#'
#'  trimText("  This   is  String  ", left=TRUE)
#' # Output: "This   is  String  "
#'
#' trimText("  This   is  String  ", right=TRUE)
#' # Output: "  This   is  String"
trimText <- function(str, left=FALSE, right=FALSE) {

  if(left & !right ) {
    str <- trimTextl(str)
    return (str)
  }
  else if(right & !left) {
    str <- trimTextr(str)
    return (str)

  } else if(left & right) {
    str <- trimTextl(str)
    str <- trimTextr(str)
    return (str)
  }
  else {
    str <- .trimTextlrm(str)
  }

  return(str)
}


# helper function to remove left, right and middle extra spacing
# str - text to remove extra space
.trimTextlrm <- function(str) {
  # Replace multiple spaces with a single space
  str <- gsub("\\s+", " ", str)

  # Remove leading and trailing spaces
  str <- gsub("^\\s+|\\s+$", "", str)

  return(str)
}

# helper function to remove left spacing
# str - text to remove extra space
#' Title
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
trimTextl <- function(str) {

  # Replace multiple spaces at the beginning with a single space
  str <- gsub("^\\s+", " ", str)

  # Remove the leading space
  str <- gsub("^ ", "", str)

  return(str)
}

# helper function to remove extra right spacing.
# str - text to remove extra space
#' Title
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
trimTextr <- function(str) {

  # Replace multiple spaces at the end with a single space
  str <- gsub("\\s+$", " ", str)

  # Remove the trailing space
  str <- gsub(" $", "", str)

  return(str)
}

# Turn a text into tokens.
# text  - text to be tokenized
# words - an arrays of tokens to return
#' Title
#'
#' @param text
#'
#' @return
#' @export
#'
#' @examples
tokws <- function(text) {

  # Remove non-alphanumeric characters
  text <- gsub("[^[:alnum:]]", " ", text)
  # Convert text to lowercase
  text <- tolower(text)
  # Split text into words
  words <- strsplit(text, " ")
  # Remove empty strings
  words <- sapply(words, function(x) x[!x == ""])
  # Return the tokenized text
  return(words)
}


# Extract sentences from text.
# text  - text to be tokenized into sentences
# an arrays of tokens sentences to return
#' Title
#'
#' @param text
#'
#' @return
#' @export
#'
#' @examples
tokss <- function(text) {

  # Convert the text to a character vector
  text <- as.character(text)

  # Split the text into individual sentences
  sentences <- strsplit(text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T)

  # Remove any empty sentences
  sentences <- sentences[!is.null(sentences)]

  # Return the tokenized sentences
  return(unlist(sentences))
}

