#library(textir)
#data(we8there)
#dim(we8thereCounts)
#dimnames(we8thereCounts)


install.packages("tidytext")
install.packages("tidyverse")
library(tidytext)
library(tidyverse)
# We will need a URL from where to download the data
# In this case we will do it from my GitHub repository
# You can download the data using this link

# I prefer vroom to ingest csv, but you can use readr::read_csv() if you fancy it more

reviews <- read.csv("https://raw.github.com/VladAluas/Text_Analysis/master/Datasets/Text_review.csv", header = TRUE, sep = ",")
#> Rows: 433
#> Columns: 3
#> Delimiter: ","
#> chr [3]: Model, Segment, Text
#> 
#> Use `spec()` to retrieve the guessed column specification
#> Pass a specification to the `col_types` argument to quiet this message

head(reviews)

#reviews %>%
#unnest_tokens("Word", "Text") 


reviews_tidy <- reviews %>%
unnest_tokens("Word", "Text") %>%
# We also want to prevent the analysis in showing 6t and 6t's as two separate words
mutate(Word = str_replace(Word, "'s", ""))
# We want to display graphically a word frequency plot
# We will create a function that will store all the operations we will repeat several times

source("common.R")
reviews_tidy %>%
word_frequency(15)
#> Selecting by n  
  