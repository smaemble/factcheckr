
#' Basic helper function to rapidly work with the example Hotel_Reviews
#' dataset shipped with this package. It collapses reviews into a single text
#'
#' @param dataset - the hotel review dataset
#' @param hotelName - the hotel name whose reviews should be extracted
#'
#' @return the text collapse into a single text
#' @export
#'
#' @examples
#'
#' createTestData(dataset=Hotel_Reviews, hotelName="Red Roof Inn Cedar Rapids")
#'
#'
createTestData <- function(dataset, hotelName="Red Roof Inn Cedar Rapids") {

  dataset <- dataset %>%
      dplyr::filter(name == hotelName) %>%
      dplyr::select(reviews.text)

  dataset <- paste(dataset, collapse = " ")

  return (dataset)
}
