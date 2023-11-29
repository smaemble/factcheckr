

createTestData <- function(){
  reviews2 <- read.csv("~/Documents/r-workspace/stat600/rpk-project/datasets/Hotel_Reviews.csv", header = TRUE, sep = ",")
  print(colnames(reviews2))
  head(reviews2, 10)
  unique(reviews2$name)
  
  review <- reviews2 %>% 
    dplyr::filter(name == "Red Roof Inn Cedar Rapids") %>% 
    dplyr::select(reviews.text)
  
  reviews <- paste(review, collapse = " ")
  
   return (reviews)
}
