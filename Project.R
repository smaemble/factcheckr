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
install.packages("zoo")
#install.packages("flextable")
install.packages("wordcloud") 


# activate packages
library(dplyr)
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
library(zoo)

#library(flextable)

source("common.R")
source("common.R")

reviews1 <- read.csv("~/Documents/r-workspace/stat600/rpk-project/datasets/tripadvisor_hotel_reviews.csv", header = TRUE, sep = ",")

data.review2 <-createTestData()
source("TestData.R")

print(colnames(reviews1))
unique(reviews2$name)

#head(reviews2)
#reviews1$Review

# collapse all rows of review column into a single text element
reviews <- paste(reviews1$Review, collapse = " ")

# Clean the data before processing
reviews <- cleanTxt(reviews, "Hotel")
data.review2 <- cleanTxt(data.review2, "Super8")

subjectsAnnotations <- combineSubjects(list(reviews, data.review2))

# Draw the frequency graph
frequencyGraph(subjectsAnnotations, 25)

subjects <- emotionPrevalenceBySubjects(subjectsAnnotations)
head(subjects, 10)

emotionGraph(subjects, emotions_by_subject=TRUE)


source("common.R")
source("graph.R")
# Check words that have contributed to the emotionality of scores
wordsByEmotion <- topWordsByEmotion(subjectsAnnotations)
head(wordsByEmotion, 50)
#word cloud
wcgplot(reviews, "bing")
wcgplot(data.review2, "bing")

pcgplot(subjects)

head(subjects, 10)
emotionBySubjectGraph(wordsByEmotion)

# Calculate the polarity change overtime
movingAverage <- polarityChangesOverTime(subjectsAnnotations)

head(movingAverage, 10)
head(subjectsAnnotations, 10)

magplot(movingAverage)
