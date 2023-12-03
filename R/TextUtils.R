#' Clean text before analysis.
#' This function takes a text as input and returns a new text with all extra spaces removed
#'
#' @param str - text to remove extra spaces left, middle and right
#' @param left - boolean flag if extra spaces should be removed from the left only
#' @param right - boolean flag if extra spaces should be removed from the right only
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

  if(identical(str, NULL)){
    stop("str param cannot be NULL")
  }

  if (!is.logical(left) | !is.logical(right)) {
    stop("left and right variables must be boolean")
  }

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


#' helper function to remove left spacing in a text, necessary for data cleanup
#'
#' @param str - the string text to remove extra spacing
#'
#' @return the text with spacing removed on the left
#' @export
#'
#' @examples
#' trimTextl("  This   is  String  ")
#' # Output: "This   is  String  "
trimTextl <- function(str) {

  if(identical(str, NULL)){
    stop("str param cannot be NULL")
  }

  # Replace multiple spaces at the beginning with a single space
  str <- gsub("^\\s+", " ", str)

  # Remove the leading space
  str <- gsub("^ ", "", str)

  return(str)
}


#' helper function to remove extra right spacing.
#'
#' @param str  -  text to remove extra spacing
#'
#' @return  text with spacing removed on the left
#' @export
#'
#' @examples
#' trimTextr("  This   is  String  ")
#' # Output: "  This   is  String"
trimTextr <- function(str) {

  if(identical(str, NULL)){
    stop("str param cannot be NULL")
  }

  # Replace multiple spaces at the end with a single space
  str <- gsub("\\s+$", " ", str)

  # Remove the trailing space
  str <- gsub(" $", "", str)

  return(str)
}


#' Turn a text into words tokens
#'
#' @param text  -text to be tokenized
#'
#' @return  an arrays of words tokens
#' @export
#'
#' @examples
#' Output <- strtokwords("This is a very long character vector.")
#' # Output[1]: "This"
#' # Output[2]: "is"
#' # Output[3]: "a"
#' # Output[4]: "very"
#' # Output[5]: "long"
#' # Output[6]: "character"
#' # Output[7]: "vector"
strtokwords <- function(text) {

  if(identical(text, NULL)){
    stop("text param cannot be NULL")
  }
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


#' Extracts sentences from text if there is any.
#'
#' @param text - text to be tokenized into sentences
#'
#' @return an arrays of tokens sentences to return
#' @export
#'
#' @examples
#' Output <- strtokss("This is a very long character vector. Why is it so long? I think lng. is short for long")
#' # Output[1]: "This is a very long character vector"
#' # Output[2]: "Why is it so long?"
#' # Output[3]: "I think lng. is short for long"
strtokss <- function(text) {

  # Convert the text to a character vector
  text <- as.character(text)

  # Split the text into individual sentences
  sentences <- strsplit(text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T)

  # Remove any empty sentences
  sentences <- sentences[!is.null(sentences)]

  # Return the tokenized sentences
  return(unlist(sentences))
}

