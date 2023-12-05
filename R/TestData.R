
createTestData <- function(hotelName="Red Roof Inn Cedar Rapids"){
    reviews2 <- read.csv("~/Documents/r-workspace/stat600/factcheckr/data/Hotel_Reviews.csv", header = TRUE, sep = ",")
    print(colnames(reviews2))
    head(reviews2, 10)
    unique(reviews2$name)

    reviews2 <- reviews2 %>%
      dplyr::filter(name == hotelName) %>%
      dplyr::select(reviews.text)

    reviews2 <- paste(reviews2, collapse = " ")

    return (reviews2)
}
