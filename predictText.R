library(stringi)
library(dplyr)
library(tm)
library(RWeka)
library(gridExtra)
library(wordcloud)
library(ggplot2)

con1 <- file("Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r")
twit <- readLines(con1)
close(con1)

con2 <- file("Coursera-SwiftKey/final/en_US/en_US.news.txt", "r")
news <- readLines(con2)
close(con2)

con3 <- file("Coursera-SwiftKey/final/en_US/en_US.blogs.txt", "r")
blog <- readLines(con3)
close(con3)

set.seed(1234)
all <- c(twit, news, blog)
sampleall <- sample(all, length(all)*0.001)

sampleall <- iconv(sampleall, "latin1", "ASCII", sub = "")

corp <-  VCorpus(VectorSource(sampleall))
corp <- corp %>% 
    tm_map(PlainTextDocument) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace)

NGramFreq <- function(x, n) {
    
    NGT <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
    
    mat <- as.matrix(
        TermDocumentMatrix(x, control = list(tokenize = NGT)))
    freq <- rowSums(mat)
    
    wordfreq <- as.data.frame(cbind(names(freq), freq))
    colnames(wordfreq) <- c("nGram", "Frequency")
    wordfreq$Frequency <- as.numeric(as.character(wordfreq$Frequency))
    
    wordfreq <-wordfreq[order(wordfreq$Frequency, decreasing = TRUE), ]
    wordfreq <-wordfreq %>%
        mutate(CumProb = cumsum(wordfreq$Frequency/sum(wordfreq$Frequency)))
    
    return(wordfreq)
}

unifreq <- NGramFreq(corp, n = 1)
bifreq <- NGramFreq(corp, n = 2)
trifreq <- NGramFreq(corp, n = 3)

#tm_map for foreign words/dictionary comparison

## For Bigrams and Trigrams, split the words into separate columns
splitWords <- function(df, n) {
    tmp <- as.data.frame(matrix(unlist(strsplit(as.character(df$nGram), " ")),
                                ncol = n, byrow = TRUE))
    df$nGram <- NULL
    df <- cbind(tmp, df)
    return(df)
}

bifreq <- splitWords(bifreq, n = 2)
names(bifreq)[1:2] <- c("Word-1", "Word")

trifreq <- splitWords(trifreq, n = 3)
names(trifreq)[1:3] <- c("Word-2", "Word-1", "Word")

## Create Quadgram and split up
quadfreq <- NGramFreq(corp, n = 4)
quadfreq <- splitWords(quadfreq, n = 4)
names(quadfreq)[1:4] <- c("Word-3", "Word-2", "Word-1", "Word")

## Reduce the size of N-grams
bifreq <- bifreq[!(bifreq$Frequency == 1), ]
trifreq <- trifreq[!(trifreq$Frequency == 1), ]
quadfreq <- quadfreq[!(quadfreq$Frequency == 1), ]










