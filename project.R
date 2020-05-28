f <- read.csv("Hotel_Reviews.csv")
summary(f)
library(corrplot)
M <- cor(f[c(2,4,8,9,11,12,13,15)])
M
corrplot(M, method = "circle")

library(NLP)
library(caret)
library(tidyverse)
library(tidytext)
library(plyr)
library(dplyr)
library(sentimentr)
library(SnowballC)
library(tm)
library(RColorBrewer)
library(ROAuth)
library(wordcloud)
library(corpus)
hotel.df <- f
str(hotel.df)
hotel.df.df <- hotel.df[1:1000, ]
hotel.df$reviews = paste(hotel.df$Negative_Review, hotel.df$Positive_Review)
set.seed(1207)
index <- 1:nrow(hotel.df)

training.index <- sample(index,trunc(length(index)*0.8))
training.df <- hotel.df$reviews[training.index]
validation.df <- hotel.df$reviews[-training.index]
library(syuzhet)
library(lubridate)
s <- get_nrc_sentiment(hotel.df$reviews)
barplot(colSums(s), las=2, col= rainbow(10), ylab= 'Count', main = 'sentiment analysis of amazon kindle')

library(corpus)
corpus<-Corpus(VectorSource(hotel.df$reviews))
#text cleaning
View(corpus)
#convert the text to lower case
corpus <- tm_map(corpus,content_transformer(tolower))
inspect(corpus[1:20])
#remove nuymbers
corpus<- tm_map(corpus,removeNumbers)
inspect(corpus[1:20])
#remove english common stopwords
corpus<- tm_map(corpus,removeWords,stopwords("english"))
inspect(corpus[1:20])
#remove punctuation
corpus<-tm_map(corpus,removePunctuation)
inspect(corpus[1:20])
#remove extra whitespaces
corpus<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:20])
#remove selected words from previous wordcloud search to get meani8ngful results
corpus<-tm_map(corpus,removeWords,c("this","the","was","and","you","but","her","this","that","she","with","book"))
#add words "books","plot","read","scene","also","without","think","someone","author","serial","reviews","word","man","years","things","world","scenes".
inspect(corpus[1:20])
#add stemming part


#create term document matrix(matrix which describes the frequency of the terms)
tdm<-TermDocumentMatrix(corpus)
tdm<-as.matrix(tdm)
tdm[1:10, 1:20]
v<-rowSums(tdm)
v
v<- subset(v, v>=100)#we set the limit here
v< -sort(rowSums(tdm), decreasing= TRUE)
v
library(syuzhet)
library(lubridate)
s <- get_nrc_sentiment(hotel.df$reviews)
s
barplot(colSums(s), las=2, col= rainbow(10), ylab= 'Count', main = 'sentiment analysis of amazon kindle')
corpus <- tm_map(corpus,stemDocument)