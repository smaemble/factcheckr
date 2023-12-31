% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WordAnalyzer.R
\name{topterms}
\alias{topterms}
\title{We currently examine the words that have influenced the emotionality scores.
In simpler terms, we explore which words carry the most significance for
the emotion scores within each subject. To enhance interpretability, we will exclude
specific core emotion categories as well as polarity.}
\usage{
topterms(subjectsAnnotations, min_top_words = 4)
}
\arguments{
\item{subjectsAnnotations}{- the subject annotation}

\item{min_top_words}{- the number of top words to return if any}
}
\value{
topwords associated with the lexicon
}
\description{
We currently examine the words that have influenced the emotionality scores.
In simpler terms, we explore which words carry the most significance for
the emotion scores within each subject. To enhance interpretability, we will exclude
specific core emotion categories as well as polarity.
}
\examples{

output <- neatlyStart("Texas A&M has the best Statistical Learning Program in
 the nation.", "Texas AM")

output2 <- neatlyStart("Mit is very expensive, student loans sucks", "MIT")

annotations <- combineSubjects(list(output, output2), lex="nrc")


 out <- topterms (subjectsAnnotations=combineSubjects(list(output, output2),
  lex="nrc"))

# Groups:   Subject, sentiment [1]
# Subject  Word        sentiment     n score
# <fct>    <chr>       <fct>     <int> <dbl>
# Texas AM nation      trust         1   0.5
# Texas AM statistical trust         1   0.5
}
\seealso{
\code{\link{combineSubjects}}, \code{\link{emotionFrequency}}, \code{\link{topterms}},
         \code{\link{polarityChange}}
}
