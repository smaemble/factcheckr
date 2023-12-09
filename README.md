factcheckr Package
================
Armel Oum Maemble

## factcheckr: Sentiment Analysis on any text data. 

This package provides utility functions for analyzing any text data including but not limited to functions for tokenizing text, topterms, word frequencies and sentiment analysis.

Sentiment Analysis involves discerning opinions expressed in various texts, categorizing them into different polarities such as positive, negative, or neutral. Also referred to as opinion mining and polarity detection, this process enables the identification of the underlying sentiment within documents, websites, social media feeds, political speeches, reviews and more. Sentiment Analysis is a form of classification, organizing data into distinct classes. These classes may take on a binary form, distinguishing between positive and negative sentiments, or they may encompass multiple categories, such as happy, sad, angry and so forth allowing consumers to make informed decisions before committing to a product.

## Installation

``` r
devtools::install_github("smaemble/factcheckr")
```

## Usage

``` r
library(factcheckr)
```

## Getting started.
This package comes with two ready to use datasets which can serve as a basis to get an easy start. In this example, we will show a contract between four different hotels. You can follow the same technique on any product, speech, etc....

``` r

ComfortSuites <- createTestData(dataset=Hotel_Reviews, hotelName="Comfort Suites")
Super8 <- createTestData(dataset=Hotel_Reviews, hotelName="Super 8")
Motel6 <- createTestData(dataset=Hotel_Reviews, hotelName="Motel 6")
GovernorHotel <- createTestData(dataset=Hotel_Reviews, hotelName="The Governor Hotel")

```
The hotel names can be found in the Hotel_Reviews dataset. This prep operation above just extracts reviews for four different hotels namely Comfort Suites, Motel 6 and Governor Hotel. There are 879 different hotels in the Hotel_Reviews dataset and you can follow instructions above to extract any hotels review needed. The `createTestData()` function only works with this test dataset and is not part of this package capability. 

## Data clean up: Package capability
We are going to use the `neatlystart()` function to clean up our text data. this function takes a corpus and subject then convert the corpus to lower case, then words, remove any extra spaces, remove stopwords as well as non English words and then create a new column called subject and return a tibble.

``` r
nsComfortSuites <- neatlystart(corpus=ComfortSuites, subject="Comfort Suites")
nsMotel6 <- neatlystart(corpus=Motel6, subject="Motel 6")
nsGovernorHotel <- neatlystart(corpus=GovernorHotel, subject="The Governor Hotel")
```

## Create a subject annotations for all subjects

 Use the `combinesubjects` function by passing a list of neatlystart output using 03 of the lexicon `nrc`, `bing` or `loughran`. This function help analysis all 03 subjects at once so we can draw a contrast.
 
```r
nrcSubjects <- factcheckr::combinesubjects(list(nsComfortSuites, nsMotel6, nsGovernorHotel), lex="nrc")

# A tibble: 4,119 × 4
# Groups:   Subject [3]
   Word    Subject        words sentiment   
   <chr>   <fct>          <int> <fct>       
 1 medical Comfort Suites  5533 anticipation
 2 medical Comfort Suites  5533 fear        
 3 medical Comfort Suites  5533 positive    
 4 medical Comfort Suites  5533 trust       
 5 center  Comfort Suites  5533 positive    
 6 center  Comfort Suites  5533 trust       
 7 clean   Comfort Suites  5533 joy         
 8 clean   Comfort Suites  5533 positive    
 9 clean   Comfort Suites  5533 trust       
10 decent  Comfort Suites  5533 positive
# ℹ 4,109 more rows
```

Using bing lexicon gives a comparable results

```r
bingSubjects <- factcheckr::combinesubjects(list(nsComfortSuites, nsMotel6, nsGovernorHotel), lex="bing")
# A tibble: 1,533 × 4
# Groups:   Subject [3]
   Word           Subject        words sentiment
   <chr>          <fct>          <int> <fct>    
 1 clean          Comfort Suites  5533 positive 
 2 decent         Comfort Suites  5533 positive 
 3 negative       Comfort Suites  5533 negative 
 4 pathetic       Comfort Suites  5533 negative 
 5 discriminatory Comfort Suites  5533 negative 
 6 free           Comfort Suites  5533 positive 
 7 accessible     Comfort Suites  5533 positive 
 8 issue          Comfort Suites  5533 negative 
 9 unreasonable   Comfort Suites  5533 negative 
10 remedy         Comfort Suites  5533 positive 
# ℹ 1,523 more rows

```
## Data Visualization

Use `ggplot3(text=text, graphType = "frequency", top = 50, embs = FALSE, lexicon="nrc", maxWords = 50, cutoffScore=100)` function to view and analyze 07 graph types namely. The `graphType` can be any of `c("frequency","sentiment", "emotion", "topterms", "movingaverage", "polarity", "wordcloud")`. The `lexicon` parameter can be `nrc`, `bing` and `loughran`. Words are classified based on the lexicon which contained known words. It is recommended to start with a lower cut off score or you may see an empty plot. Future releases will include suggestive cutoffScores.

## Viewing Sentiment Score
the text argument should be the output of the `neatlystart` function
``` r
ggplot3(text=nsComfortSuites, graphType = "sentiment", lexicon="bing", cutoffScore = 4)
```
Sentiment Score plot for the Comfort suites Hotel

![plot](./man/figures/comfort_sentiment.png)

## Word Frequency
Graph below shows the most frequently used words in Motel6 aggregated Reviews. 
``` r
ggplot3(text=nsMotel6, graphType = "frequency")
```
![plot](./man/figures/motel6_freq.png)

The word `motel` appears a lot and it does not tell us anything about this motel since we are reviewing the motel itself. So `motel` is a stopword and you can use factcheckr `removeStopwords()` function to remove one or many stopwords as follows. 

``` r
nsMotel6 <- removeStopwords(words_dictionary= nsMotel6, stopwords = data.frame(word = c("motel")))
```

## Emotion visualizations accross subjects

To summarize the results of the Sentiment Analysis, the percentages of the prevalence of emotions across these hotels is calculated using `nrc` lexicon because it has multiclass classification with words such as `anger`, `anticipation`, `disgust`, `fear`, `joy`, `sadness`, `surprise` and `trust` while `bing` lexicon classfies only as `positive` or `negative`.

Use `emotionFrequency()` passing `nrcSubjects` obtained above. Then pass that result to the `ggplot3()` to visualize the contrast amongst the subjects under fact checking. 

```r
nrcEmotions <- emotionFrequency(subjectsAnnotations=nrcSubjects)
ggplot3(text=nrcEmotions, graphType ="emotion")
```
![plot](./man/figures/emotion_freq.png)

## Moving Average Plot
The moving average plot tells us how opinions are changing overtime
``` r
ggplot3(text=nsGovernorHotel, graphType = "sentiment", lexicon="bing", cutoffScore = 1)
```

## WordCloud Visualization. 
For the ultimate visualization, we'll generate a word cloud that highlights the most frequently occurring positive and negative words. Specifically, we'll utilize the `ggplot3()` function to craft a single word cloud encompassing both negative and positive words, presenting a comprehensive view.
```r
ggplot3(text=nsGovernorHotel, graphType = "wordcloud", lexicon="bing", maxWords = 100)
```
The Governor Hotel Wordcloud which shows a big disappointment

![plot](./man/figures/gov_wc.png)

## factcheckr: Sentiment Analysis on any text data. 
