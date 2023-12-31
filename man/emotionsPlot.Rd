% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Graph.R
\name{emotionsPlot}
\alias{emotionsPlot}
\title{visualize the results and show the scores for each core emotion by
subjects(Products)}
\usage{
emotionsPlot(subjects, embs = FALSE)
}
\arguments{
\item{subjects}{- A tibble: with 5 columns (Subject,sentiment,sentiment_freq,
words, percentage)}

\item{embs}{- Boolean whether to show emotions by subject under study or not}
}
\value{
emotions plot
}
\description{
visualize the results and show the scores for each core emotion by
subjects(Products)
}
\examples{

output <- neatlyStart(corpus=ObamaVictorySpeech, subject="MIT")

nrcSubjects <- combineSubjects(list(output), lex="nrc")

emotionsPlot(subjects=emotionFrequency(subjectsAnnotations=nrcSubjects),
 embs=TRUE)

}
