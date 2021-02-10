library(tm)
options(header=FALSE, stringsAsFactors = FALSE,FileEncoding="latin1")
setwd("C:/Users/dell/Documents/Product-Analysis")
text<-readLines("new.txt")
corpus<-Corpus(VectorSource(text))
inspect(corpus[34])
corpus<-tm_map(corpus, tolower)
corpus<-tm_map(corpus, removePunctuation)
corpus<-tm_map(corpus, removeNumbers)
cleanset<-tm_map(corpus, removeWords,stopwords("english"))
cleanset<-tm_map(cleanset, stripWhitespace)
dtm<-TermDocumentMatrix(cleanset,control=list(minwordLength=c(1,Inf)))
findFreqTerms(dtm,lowfreq=15)
termFrequency<-rowSums(as.matrix(dtm))
termFrequency<-subset(termFrequency,termFrequency>=10)
library(ggplot2)
barplot(termFrequency,las=2,col=rainbow(20))


cleanset<-tm_map(cleanset,removeWords,c("asus","use","will","still","can","one","really","phone","life","many","day","dont","get","like","make","even","just","without","mode","know","now","thing","well","phones","good","going","rog","oled","never"))
cleanset<-tm_map(cleanset,gsub,pattern="games",replacement="gaming")
cleanset<-tm_map(cleanset, stripWhitespace)
dtm<-TermDocumentMatrix(cleanset,control=list(minwordLength=c(1,Inf)))
findFreqTerms(dtm,lowfreq=15)
termFrequency<-rowSums(as.matrix(dtm))
termFrequency<-subset(termFrequency,termFrequency>=10)
library(ggplot2)
barplot(termFrequency,las=2,col=rainbow(20))



library(wordcloud)
library(wordcloud2)
m<- as.matrix(dtm)
wordFreq<- sort(rowSums(m),decreasing = TRUE)
set.seed(375)
grayLevels <-gray(wordFreq+10) / (max(wordFreq)+10)
wordcloud(words=names(wordFreq),freq=wordFreq,max.words=100, rot.per=0.2,scale=c(7,.1),min.freq=5,colors=brewer.pal(8,"Dark2"))
wordcloud(words=names(wordFreq),freq=wordFreq,max.words=100, rot.per=0.2,scale=c(7,.1),min.freq=5,colors=rainbow(20))






#sentiment analysis
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
filename<-glue("C:/Users/ANANT/Desktop/project",amazon,sep="")
filename<- trimws("new.txt")
fileText <- glue(read_file(filename))
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative


