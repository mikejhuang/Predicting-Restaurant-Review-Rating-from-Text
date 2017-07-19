library(jsonlite)
business <- stream_in(file("yelp_academic_dataset_business.json"))
checkin <- stream_in(file("yelp_academic_dataset_checkin.json"))
review <- stream_in(file("yelp_academic_dataset_review.json"))
tip <- stream_in(file("yelp_academic_dataset_tip.json"))
user <- stream_in(file("yelp_academic_dataset_user.json"))


library(tm)
library(wordcloud)
library(RWeka)
reviewone<-review[review$stars==1,'text']
reviewtwo<-review[review$stars==2,'text']
reviewthree<-review[review$stars==3,'text']
reviewfour<-review[review$stars==4,'text']
reviewfive<-review[review$stars==5,'text']

test <- Corpus(VectorSource(reviewone))
test <- tm_map(test, stripWhitespace)
test <- tm_map(test, content_transformer(tolower))
test <- tm_map(test, removeWords, stopwords('english'))
test <- tm_map(test, stemDocument)
tokenize_bigrams <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) 
tdm <- TermDocumentMatrix(test, control = list(tokenize=tokenize_bigrams))
matrix <- DocumentTermMatrix(test,control=list(tokenize=tokenize_bigrams))

tdm2<-removeSparseTerms(tdm,0.99999)
tdm2<-rollup(tdm2,2,na.rm=TRUE,FUN=sum)
m = as.matrix(tdm2)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#wordcloud(dm, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))

install.packages(c("RTextTools","topicmodels"))
library(RTextTools)
library(topicmodels)

matrix<-DocumentTermMatrix(test)
data <- NYTimes[sample(1:3100,size=1000,replace=FALSE),]
matrix <- create_matrix(cbind(as.vector(data$Title),as.vector(data$Subject)), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
k <- length(unique(data$Topic.Code))
lda <- LDA(matrix, k)

ny<-Corpus(VectorSource(NYTimes$Subject))
ny <- tm_map(ny, removeWords, stopwords('english'))
wordcloud(ny, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))
