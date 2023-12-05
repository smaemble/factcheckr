#install.packages("dplyr")
#library(dplyr)


reviews1 <- read.csv("~/Documents/r-workspace/stat600/factcheckr/data/tripadvisor_hotel_reviews.csv", header = TRUE, sep = ",")

reviews2 <-factcheckr::createTestData()


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

factcheckr::ggplot3(text = subjectsAnnotations, graphType ="emotion")



source("R/Graph.R")

ggplot3(schoolAnnotations, top=15, "frequency")



subjects <- factcheckr::emotionFrequency(subjectsAnnotations)
subjects <- factcheckr::emotionFrequency(schoolAnnotations)

head(subjects)

emotionGraph(subjects, emotions_by_subject=TRUE)



source("graph.R")
# Check words that have contributed to the emotionality of scores
topwords <- topterms(subjectsAnnotations)
head(topwords, 50)
#word cloud
wcgplot(reviews, "bing")
wcgplot(reviews2, "bing")

pcgplot(subjects)

head(subjects, 10)
emotionBySubjectGraph(topwords)

# Calculate the polarity change overtime
movingAverage <- polarity(subjectsAnnotations)

magplot(movingAverage)
