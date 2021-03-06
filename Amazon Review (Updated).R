## Amazon Review - "MSI GT63 TITAN-052 15.6" 120Hz 3ms G-Sync Extreme Gaming Laptop"

library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


# Amazon Reviews

aurl <- "https://www.amazon.com/MSI-GT63-TITAN-052-Extreme-i7-8750H/product-reviews/B07CSFW5Y1/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}

length(amazon_reviews)

setwd("C:/PRATIK/Data Science/Assignment/Completed/Text Mining/IMDB Analysis")

write.table(amazon_reviews,"MSIGTGN.txt",row.names = F)


MSIGTGN_Lap <- read.delim('MSIGTGN.TXT')
str(MSIGTGN_Lap)
View(MSIGTGN_Lap)


# Build Corpus and DTM/TDM

library(tm)
corpus <- MSIGTGN_Lap[-1,]
head(corpus)
class(corpus)

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)   # convert all to lowercase
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)   # To remove the Punctuations
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)   # To remove the Numbers
inspect(corpus[1:5])

corpus_clean<-tm_map(corpus,stripWhitespace)   #To remove the White Spaces
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('laptop','can'))   # To remove the laptop and can words
cleanset <- tm_map(cleanset, gsub,pattern = 'computer', replacement = 'machine')   # The barplot pulls both Computer and Machine as separate words. this should be counted as one as both holds the same synonym.
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])


#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]


# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))


# Word Cloud :

library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)


# lettercloud 
letterCloud(w,word = 'Am',frequency(5), size=1)


# Sentiment Analysis for tweets:

# Read File 

Amzn_reviews <- read.delim('MSIGTGN.TXT')
reviews <- as.character(Amzn_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
get_nrc_sentiment('Love')   # Love has one Joy and one positive 

get_nrc_sentiment('glaring')   # 1 Anger and 1 Negavite

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),ylab = 'Count',main= 'Sentiment scores for Amazon Reviews-MSI GT63 TITAN Gaming Laptop')
