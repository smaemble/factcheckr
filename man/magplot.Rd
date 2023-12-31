% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Graph.R
\name{magplot}
\alias{magplot}
\title{The moving average plot of polarity change overtime
Showcase the rolling mean values to examine whether there are discernible
trends in how the polarity evolves throughout the subject under fact checking}
\usage{
magplot(movingAverage)
}
\arguments{
\item{movingAverage}{- the polarity moving average}
}
\value{
the plot
}
\description{
The moving average plot of polarity change overtime
Showcase the rolling mean values to examine whether there are discernible
trends in how the polarity evolves throughout the subject under fact checking
}
\examples{

data(ObamaVictorySpeech)
data(TrumpVictorySpeech)
nsObamaSpeech <- neatlyStart(corpus=ObamaVictorySpeech, subject="Obama Speech")
nsTrumpSpeech <- neatlyStart(corpus=TrumpVictorySpeech, subject="Trump Speech")

nrcSubjects <- combineSubjects(list(nsObamaSpeech, nsTrumpSpeech), lex="nrc")

magplot(movingAverage=polarityChange(nrcSubjects))

}
