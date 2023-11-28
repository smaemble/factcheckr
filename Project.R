rm(list=ls())
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")
install.packages("tibble")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("textdata")
#install.packages("Hmisc")
install.packages("sentimentr")
#install.packages("zoo")
#install.packages("flextable")
install.packages("wordcloud") 


# activate packages
#library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(tidytext)
library(tidyverse)
library(textdata)
#library(Hmisc)
library(reshape2)
library(sentimentr)

library(wordcloud)

#library(zoo)
#library(flextable)

source("common.R")

reviews1 <- read.csv("~/Documents/r-workspace/stat600/rpk-project/datasets/tripadvisor_hotel_reviews.csv", header = TRUE, sep = ",")

reviews2 <- read.csv("~/Documents/r-workspace/stat600/rpk-project/datasets/Hotel_Reviews.csv", header = TRUE, sep = ",")
print(colnames(reviews1))
unique(reviews2$name)

#head(reviews2)
#reviews1$Review

# collapse all rows of review column into a single text element
reviews <- paste(reviews1$Review, collapse = " ")

# Clean the data before processing
reviews <- cleanTxt(reviews, "Hotel")

emotions <- combineEmotion(list(reviews))

# Draw the frequency graph
frequencyGraph(emotions, 25)

products <- annotateSubjects(emotions)
head(products, 10)

emotionGraph(products, emotions_by_subject=TRUE)

# Check words that have contributed to the emotionality of scores
topWordsForEachEmotion(emotions)

wordCloudGraph(reviews, "bing")



