#rm(list=ls())
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("tibble")
# install.packages("tidytext")
# install.packages("tidyverse")
# install.packages("textdata")
# #install.packages("Hmisc")
# install.packages("sentimentr")
# install.packages("zoo")
# #install.packages("flextable")
# install.packages("wordcloud")


# # activate packages
# library(dplyr)
# library(stringr)
# library(tidyr)
# library(tibble)
# library(tidytext)
# library(tidyverse)
# library(textdata)
# #library(Hmisc)
# library(reshape2)
# library(sentimentr)
# library(wordcloud)
# library(zoo)

#library(flextable)

source("TestData.R")
source("Graph.R")

reviews1 <- read.csv("~/Documents/r-workspace/stat600/factcheckr/data/tripadvisor_hotel_reviews.csv", header = TRUE, sep = ",")

data.review2 <-createTestData()


print(colnames(reviews1))
unique(reviews2$name)

#head(reviews2)
#reviews1$Review

# collapse all rows of review column into a single text element
reviews <- paste(reviews1$Review, collapse = " ")

# Clean the data before processing, cleanCBAS
reviews <- factcheckr::neatlystart(reviews, "Hotel")
output <- factcheckr::neatlystart("Texas A&M has the best Statistical Learning Program in the nation.", "Texas AM")
output2 <- factcheckr::neatlystart("Mit is very expensive, student loans sucks", "MIT")

#out <- factcheckr::removeStopwords(output, data.frame(word = c("texas")))
#out

reviews2 <- factcheckr::neatlystart(reviews2, "Super8")



subjectsAnnotations <- factcheckr::combinesubjects(list(reviews, reviews2))



output
output2
schoolAnnotations <- factcheckr::combinesubjects(list(output, output2), lex="bing")
schoolAnnotations <- factcheckr::combinesubjects(list(output, output2))
schoolAnnotations

# Draw the frequency graph
frequencyGraph(subjectsAnnotations, 25)
frequencyGraph(schoolAnnotations, 25)

subjects <- factcheckr::emotionFrequency(subjectsAnnotations)
subjects <- factcheckr::emotionFrequency(schoolAnnotations)

head(subjects)

emotionGraph(subjects, emotions_by_subject=TRUE)



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
