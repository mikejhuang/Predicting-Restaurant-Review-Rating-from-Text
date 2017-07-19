library(doMC)
registerDoMC(cores=6)

restaurants<-business[grep("Restaurants",business$categories),'business_id']

resttest<-review[grep(restaurants[10002],review$business_id),]
for(i in 100685:length(restaurants)){
  resttest<-rbind(restaurantreviews,review[grep(restaurants[i],review$business_id),])
  print(i)
}

library(tm)
library(wordcloud)
library(RWeka)
library(RTextTools)
library(e1071)
library(randomForest)
##find frequency of the 1000th most common word ***pair***
test <- Corpus(VectorSource(restaurantreviews$text[1:25000]))
test <- tm_map(test, stripWhitespace)
test <- tm_map(test, content_transformer(tolower))
test <- tm_map(test, removeWords, stopwords('english'))
test <- tm_map(test, stemDocument)
tokenize_bigrams <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) 
tdm <- TermDocumentMatrix(test, control = list(tokenize=tokenize_bigrams))
dtm <- DocumentTermMatrix(test, control = list(tokenize=tokenize_bigrams))
tdm2<-removeSparseTerms(tdm,0.98)
tdm2<-rollup(tdm,2,na.rm=TRUE,FUN=sum)
m = as.matrix(tdm2)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_frtraeqs)

##find frequency of the 1000th most common word
test <- Corpus(VectorSource(restaurantreviews$text))
test <- tm_map(test, stripWhitespace)
test <- tm_map(test, content_transformer(tolower))
test <- tm_map(test, removeWords, stopwords('english'))
test <- tm_map(test, stemDocument)
tdm <- TermDocumentMatrix(test)
tdm2<-removeSparseTerms(tdm,0.98)
tdm2<-rollup(tdm,2,na.rm=TRUE,FUN=sum)

#Load list of words and their correlations across stars
#tried corxy<1, 33.74% accuracy
#corxy<2,35.41 accuracy
#corxy<10, 45.48 accuracy
#nocorxy 47.9 accuracy
setwd('D:/Mike/Documents/Jean Marie Lab/yelpdata')
#load('evensample50000.RData')
load('trainingset.RData')
dataset<-restaurantreviews[1:200000,'stars']
rm(restaurantreviews)
load('wordCor.RData')
lowCor<-wordCor[wordCor$corxy>5,'word']
lowCor<-as.character(lowCor)
mlowCor<-m[,!(colnames(m) %in% lowCor)]
tdmcontainer<-create_container(mlowCor,as.factor(dataset$stars),trainSize=1:20000,testSize=20001:30000,virgin=FALSE)

#Drop words below word freq
summatrix<-apply(mlowCor,1,sum)
minWordCount<-9
mlowCorPruned<-mlowCor[unname(summatrix>minWordCount),]
starstest<-as.factor(dataset$stars[unname(summatrix>minWordCount)])

tdmcontainer<-create_container(mlowCorPruned,as.factor(starstest),trainSize=1:10000,testSize=10001:length(starstest),virgin=FALSE)
#Create SVM Classifier
system.time(svmmodel<-train_model(tdmcontainer,"SVM"))
system.time(rfmodel<-train_model(tdmcontainer,"RF",ntree=200))
system.time(modelr<-randomForest(x=mlowCorPruned[1:10000,],y=starstest[1:10000],xtest=mlowCorPruned[10001:length(starstest),],ytest=starstest[10001:length(starstest)]))
predsvm<-classify_model(tdmcontainer,rfmodel)

library(caret)
confusionMatrix(predsvm$FORESTS_LABEL,as.factor(dataset$stars[20001:30000]))
confusionMatrix(predsvm$SVM_LABEL,as.factor(starstest[10001:length(starstest)]))
confusionMatrix(modelr$test$predicted,starstest[10001:length(starstest)])
#compute frequency of actual categories
#actual = as.data.frame(table(starscommon[1:length(predvec)]))
actual = as.data.frame(table(dataset$stars[20001:30000]))
names(actual) = c("Actual","ActualFreq")

#build confusion matrix
#confusion = as.data.frame(table(predvec,starscommon[1:length(predvec)]))
confusion<- as.data.frame(table(predsvm$FORESTS_LABEL,as.factor(dataset$stars[20001:30000])))
names(confusion) = c("Predicted","Actual","Freq")

#calculate percentage of test cases based on actual frequency
confusion = merge(confusion, actual, by=c('Actual'))
confusion$Percent = confusion$Freq/confusion$ActualFreq*100

#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1)+labs(x="Actual",y="Predicted")
tile = tile + 
  geom_text(aes(x=Actual, y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black")+scale_fill_gradient(low="grey",high="red")

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Actual, y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

#render
tile

