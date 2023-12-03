
install.packages("dplyr")
library(dplyr)

createTestData <- function(){
  reviews2 <- read.csv("~/Documents/r-workspace/stat600/factcheckr/data/Hotel_Reviews.csv", header = TRUE, sep = ",")
  print(colnames(reviews2))
  head(reviews2, 10)
  unique(reviews2$name)

  reviews2 <- reviews2 %>%
    dplyr::filter(name == "Red Roof Inn Cedar Rapids") %>%
    dplyr::select(reviews.text)

  reviews2 <- paste(reviews2, collapse = " ")

   return (reviews2)
}
