library(tm)
library(wordcloud)
library(RWeka)
tokenize_bigrams <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
setwd('D:/Mike/Documents/Jean Marie Lab/yelpdata')
load('trainingset.RData')
load('testingset.RData')
restaurantreviews<-rbind(restaurantreviews,resttest)

onestar<-restaurantreviews[restaurantreviews$stars==1,'text']
twostar<-restaurantreviews[restaurantreviews$stars==2,'text']
threestar<-restaurantreviews[restaurantreviews$stars==3,'text']
fourstar<-restaurantreviews[restaurantreviews$stars==4,'text']
fivestar<-restaurantreviews[restaurantreviews$stars==5,'text']

rm(list=c('restaurantreviews','resttest'))

samplesize<-87213
intrain<-sample(length(onestar),samplesize)
onestar <- Corpus(VectorSource(onestar[intrain]))
onestar <- tm_map(onestar, stripWhitespace)
onestar <- tm_map(onestar, content_transformer(tolower))
onestar <- tm_map(onestar, removeWords, c(stopwords("english"),"i","stars","star","stars","star")) 
#onestar <- tm_map(onestar, stemDocument) 
onetdm <- TermDocumentMatrix(onestar, control = list(tokenize=tokenize_bigrams))
onetdm<-removeSparseTerms(onetdm,0.999)
onetdm<-as.matrix(rollup(onetdm,2,na.rm=TRUE,FUN=sum))
onem<-as.matrix(onetdm)
# get word counts in decreasing order
word_freqs <- sort(rowSums(onem), decreasing=TRUE) 
# create a data frame with words and their frequencies
onem <- data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud
wordcloud(onem$word, onem$freq, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#two stars
intrain<-sample(length(twostar),samplesize)
twostar <- Corpus(VectorSource(twostar[intrain]))
twostar <- tm_map(twostar, stripWhitespace)
twostar <- tm_map(twostar, content_transformer(tolower))
twostar <- tm_map(twostar, removeWords, c(stopwords("english"),"i","stars","star","stars","star")) 
#twostar <- tm_map(twostar, stemDocument) 
twotdm <- TermDocumentMatrix(twostar, control = list(tokenize=tokenize_bigrams))
twotdm<-removeSparseTerms(twotdm,0.999)
twotdm<-as.matrix(rollup(twotdm,2,na.rm=TRUE,FUN=sum))
twom<-as.matrix(twotdm)
# get word counts in decreasing order
word_freqs <- sort(rowSums(twom), decreasing=TRUE) 
# create a data frame with words and their frequencies
twom <- data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud
wordcloud(twom$word, twom$freq, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#three stars
intrain<-sample(length(threestar),samplesize)
threestar <- Corpus(VectorSource(threestar[intrain]))
threestar <- tm_map(threestar, stripWhitespace)
threestar <- tm_map(threestar, content_transformer(tolower))
threestar <- tm_map(threestar, removeWords, c(stopwords("english"),"i","stars","star","stars","star")) 
#threestar <- tm_map(threestar, stemDocument) 
threetdm <- TermDocumentMatrix(threestar, control = list(tokenize=tokenize_bigrams))
threetdm<-removeSparseTerms(threetdm,0.999)
threetdm<-as.matrix(rollup(threetdm,2,na.rm=TRUE,FUN=sum))
threem<-as.matrix(threetdm)
# get word counts in decreasing order
word_freqs <- sort(rowSums(threem), decreasing=TRUE) 
# create a data frame with words and their frequencies
threem <- data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud
wordcloud(threem$word, threem$freq, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#four stars
intrain<-sample(length(fourstar),samplesize)
fourstar <- Corpus(VectorSource(fourstar[intrain]))
fourstar <- tm_map(fourstar, stripWhitespace)
fourstar <- tm_map(fourstar, content_transformer(tolower))
fourstar <- tm_map(fourstar, removeWords, c(stopwords("english"),"i","stars","star")) 
#fourstar <- tm_map(fourstar, stemDocument) 
fourtdm <- TermDocumentMatrix(fourstar, control = list(tokenize=tokenize_bigrams))
fourtdm<-removeSparseTerms(fourtdm,0.999)
fourtdm<-as.matrix(rollup(fourtdm,2,na.rm=TRUE,FUN=sum))
fourm<-as.matrix(fourtdm)
# get word counts in decreasing order
word_freqs <- sort(rowSums(fourm), decreasing=TRUE) 
# create a data frame with words and their frequencies
fourm <- data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud
wordcloud(fourm$word, fourm$freq, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#five stars
intrain<-sample(length(fivestar),samplesize)
fivestar <- Corpus(VectorSource(fivestar[intrain]))
fivestar <- tm_map(fivestar, stripWhitespace)
fivestar <- tm_map(fivestar, content_transformer(tolower))
fivestar <- tm_map(fivestar, removeWords, c(stopwords("english"),"i","stars","star")) 
#fivestar <- tm_map(fivestar, stemDocument) 
fivetdm <- TermDocumentMatrix(fivestar, control = list(tokenize=tokenize_bigrams))
fivetdm<-removeSparseTerms(fivetdm,0.999)
fivetdm<-as.matrix(rollup(fivetdm,2,na.rm=TRUE,FUN=sum))
fivem<-as.matrix(fivetdm)
# get word counts in decreasing order
word_freqs <- sort(rowSums(fivem), decreasing=TRUE) 
# create a data frame with words and their frequencies
fivem <- data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud
wordcloud(fivem$word, fivem$freq, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

save(list=c('onem','twom','threem','fourm','fivem'),file='wordcloudbigrandomsample.RData')
