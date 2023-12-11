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
#' @seealso \code{\link{trimTextl}}, \code{\link{trimTextr}}, \code{\link{strtokwords}}
#' @seealso \code{\link{strtoksentence}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#' @seealso \code{\link{neatlyStart}}, \code{\link{removeStopwords}}, \code{\link{combineSubjects}}
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
#' @seealso \code{\link{trimText}}, \code{\link{trimTextr}}, \code{\link{strtokwords}}
#' @seealso \code{\link{strtoksentence}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#' @seealso \code{\link{neatlyStart}}, \code{\link{removeStopwords}}, \code{\link{combineSubjects}}
#'
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
#' @seealso \code{\link{trimTextl}}, \code{\link{trimTextr}}, \code{\link{strtokwords}}
#' @seealso \code{\link{strtoksentence}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#' @seealso \code{\link{neatlyStart}}, \code{\link{removeStopwords}}, \code{\link{combineSubjects}}
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
#' @seealso \code{\link{trimTextl}}, \code{\link{trimTextr}}, \code{\link{strtokwords}}
#' @seealso \code{\link{strtoksentence}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#' @seealso \code{\link{neatlyStart}}, \code{\link{removeStopwords}}, \code{\link{combineSubjects}}
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
#' @seealso \code{\link{trimTextl}}, \code{\link{trimTextr}}, \code{\link{strtokwords}}
#' @seealso \code{\link{strtoksentence}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#' @seealso \code{\link{neatlyStart}}, \code{\link{removeStopwords}}, \code{\link{combineSubjects}}
#'
#' @examples
#' Output <- strtoksentence("This is a very long character vector. Why is it so long?
#'                   I think lng. is short for long")
#' # Output[1]: "This is a very long character vector"
#' # Output[2]: "Why is it so long?"
#' # Output[3]: "I think lng. is short for long"
strtoksentence <- function(text) {

  # Convert the text to a character vector
  text <- as.character(text)

  # Split the text into individual sentences
  sentences <- strsplit(text, "(?<=[[:punct:]])\\s(?=[A-Z])", perl=T)

  # Remove any empty sentences
  sentences <- sentences[!is.null(sentences)]

  # Return the tokenized sentences
  return(unlist(sentences))
}



#' clean and convert text to lowercase, then to words, remove any extra spaces
#' then add new column pass as parameter in the subject, remove all stops words and finally remove any
#' non English words.
#'
#' @param corpus  - Text corpus under fact checking to clean before starting the analysis
#' @param subject - subject domain to apply this study.
#'
#' @return  An Array(vector) of words in this corpus.
#' @export
#' @seealso \code{\link{trimTextl}}, \code{\link{trimTextr}}, \code{\link{strtokwords}}
#' @seealso \code{\link{strtoksentence}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#' @seealso \code{\link{neatlyStart}}, \code{\link{removeStopwords}}, \code{\link{combineSubjects}}
#'
#' @examples
#' Output <- neatlyStart(corpus="Texas A&M has the best Statistical Learning Program.",
#'                    subject="University")
#' # A tibble: 5 × 2
#' # Word          Subject
#' # <chr>         <chr>
#' # 1 texas       University
#' # 2 statistical University
#' # 3 learning    University
#' # 4 program     University
#' # 5 nation      University
#' #
#' # Clearly has, the, best, in are stopwords and have been removed
neatlyStart <- function(corpus="", subject="domain") {

  if (identical(corpus, NULL)){
    stop ("corpus cannot be null, specify the corpus under fact checking")
  }

  if (identical(subject, NULL)){
    stop ("subject cannot be null, specify the subject under fact checking")
  }
  corpus <- corpus %>%
  base::iconv(to = "UTF-8") %>%
  base::tolower() %>%
  base::paste0(collapse = " ") %>%

  #remove any extrace spaces in the corpus (stringr::str_squish())%>%
  trimText %>%

  # split the corpus to create words by calling our strtokwords function
  strtokwords %>%

  # Convert object to one dimensional vector
  base::unlist() %>%
  tibble::tibble() %>%
  dplyr::select(Word = 1, everything()) %>%

  # Add a new column Subject
  dplyr::mutate(Subject = subject) %>%

  # Remove all stop words
  dplyr::anti_join(tidytext::stop_words, by = c("Word" = "word")) %>%

  # Remove any non words
  dplyr::mutate(Word = stringr::str_remove_all(Word, "\\W")) %>%

  # filtered empty space
  dplyr::filter(Word != "")
}

#' Remove additional stopwords under fact checking
#'
#' @param words_dictionary -  dictionary of words array.
#' @param stopwords - The data frame of words to remove.
#'
#' @return dictionary of words with no stop words.
#' @export
#' @seealso \code{\link{trimTextl}}, \code{\link{trimTextr}}, \code{\link{strtokwords}}
#' @seealso \code{\link{strtoksentence}}, \code{\link{emotionFrequency}}, \code{\link{topterms}}
#' @seealso \code{\link{neatlyStart}}, \code{\link{removeStopwords}}, \code{\link{combineSubjects}}
#'
#' @examples
#' words <- neatlyStart(corpus="Texas A&M has the best Statistical Learning
#'             Program in the nation.", subject="University")
#' Output <- removeStopwords(words, data.frame(word = c("texas")))
#'
#' # A tibble: 5 × 2
#' # Word          Subject
#' # <chr>         <chr>
#' # 1 statistical University
#' # 2 learning    University
#' # 3 program     University
#' # 4 nation      University
#' #
#' # Texas has been removed from the texas as it could be words that means nothing in the study
removeStopwords <- function(words_dictionary, stopwords = data.frame(word = c())) {
  if(identical(words_dictionary, NULL)){
    stop("words_dictionary cannot be NULL")
  }

  if(!(is.data.frame(stopwords) && nrow(stopwords) > 0 && ncol(stopwords) > 0)) {
    stop("stopwords param must be a data frame of words to be removed")
  }

  # Remove addit stop words
  words_dictionary %>%
  dplyr::anti_join(stopwords, by = c("Word" = "word"))
}
