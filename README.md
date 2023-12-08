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
This package comes with two ready to use datasets which can serve as a basic to get an easy start. In this example, we will show a contract between three different hotels reviews. You can follow the same technique on any product reviews, speech etc....

``` r

ComfortSuites <- createTestData(dataset=Hotel_Reviews, hotelName="Comfort Suites")
Super8 <- createTestData(dataset=Hotel_Reviews, hotelName="Super 8")
Motel6 <- createTestData(dataset=Hotel_Reviews, hotelName="Motel 6")
GovernorHotel <- createTestData(dataset=Hotel_Reviews, hotelName="The Governor Hotel")

```
The hotel names can be found in the Hotel_Reviews dataset. This prep operation above just extracts reviews for four different hotels namely Comfort Suites, Super 8, Motel 6 and Governor Hotel. There are 879 different hotels in the Hotel_Reviews dataset and you can follow instructions above to extract any hotels review needed. This does not show yet our package capability. The createTestData() function only works with this test dataset and is not part of this package capability. 

## Data clean up: Package capability starts here 
We are going to use the `neatlystart()` function to clean up our text data. this function takes a corpus and subject then convert the corpus to lower case, then words, remove any extra spaces, remove stopwords as well as non English words and then create a new column called subject and return a tibble.

``` r
nsComfortSuites <- neatlystart(corpus=ComfortSuites, subject="Comfort Suites")
nsSuper8 <- neatlystart(corpus=Super8, subject="Super 8")
nsMotel6 <- neatlystart(corpus=Motel6, subject="Motel 6")
nsGovernorHotel <- neatlystart(corpus=GovernorHotel, subject="The Governor Hotel")
```

## factcheckr: Sentiment Analysis on any text data. 



## factcheckr: Sentiment Analysis on any text data. 



## factcheckr: Sentiment Analysis on any text data. 
