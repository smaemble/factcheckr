factcheckr Package
================
Armel Oum Maemble

## factcheckr: Sentiment Analysis on any text data. 

This package provides utility functions for analyzing any text data including but not limited to functions for tokenizing text, topterms, word frequencies and sentiment analysis.

Sentiment Analysis involves discerning opinions expressed in various texts, categorizing them into different polarities such as positive, negative, or neutral. Also referred to as opinion mining and polarity detection. This process enables the identification of the underlying sentiment within documents, websites, social media feeds, political speeches, reviews and more. Sentiment Analysis is a form of classification, organizing data into distinct classes. These classes may take on a binary form, distinguishing between positive and negative sentiments, or they may encompass multiple categories, such as happy, sad, angry and so forth allowing consumers to make informed decisions before committing to a product.

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

## Viewing Sentiment Score for specific products 

Use the `ggplot3()` function which takes 04 parameters. The `graphType ="sentiment"` and the `lexicon="bing"`. This package is currently using 03 type of lexicon: `nrc`, `bing` and `loughran`. Words are classified based on the lexicon which contained known words. It is recommended to start with a lower cut off score or you may see an empty plot. Future releases will include suggestive cutoffscores.

``` r
ggplot3(text=nsComfortSuites, graphType = "sentiment", lexicon="bing", cutoffScore = 4)
```
Sentiment Score plot for the Comfort suites Hotel

![plot](./man/figures/comfort_sentiment.png)

``` r
ggplot3(text=nsMotel6, graphType = "sentiment", lexicon="bing", cutoffScore = 1)
```

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
