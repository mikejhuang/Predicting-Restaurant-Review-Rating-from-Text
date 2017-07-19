library(foreach)
library(doSNOW)
library(snow)
cl <- makeCluster(c("localhost","localhost","localhost","localhost","localhost","localhost"), type = "SOCK")
clusterApply(cl, 1:6, get("+"), 3)
registerDoSNOW(cl)

restaurants<-business[grep("Restaurants",business$categories),'business_id']

resttest<-review[grep(restaurants[10002],review$business_id),]
for(i in 100685:length(restaurants)){
  resttest<-rbind(restaurantreviews,review[grep(restaurants[i],review$business_id),])
  print(i)
}

library(caret)
model<-train(Species~.,data=iris,method='rf')
library(tm)
library(wordcloud)
library(RWeka)

#Create evenly sampled training set
setwd('D:/Mike/Documents/Jean Marie Lab/yelpdata')
load('trainingset.RData')
#load('testingset.RData')
#restaurantreviews<-resttest
#rm(resttest)
onestar<-restaurantreviews[restaurantreviews$stars==1,'text']
twostar<-restaurantreviews[restaurantreviews$stars==2,'text']
threestar<-restaurantreviews[restaurantreviews$stars==3,'text']
fourstar<-restaurantreviews[restaurantreviews$stars==4,'text']
fivestar<-restaurantreviews[restaurantreviews$stars==5,'text']


set.seed(1233)
samplesize<-40000
intrain<-sample(length(onestar),samplesize)
dataset<-c(onestar[intrain],twostar[intrain],threestar[intrain],fourstar[intrain],fivestar[intrain])
dataseta<-data.frame(dataset,c(rep(1,samplesize),rep(2,samplesize),rep(3,samplesize),rep(4,samplesize),rep(5,samplesize)))
names(dataseta)<-c('text','stars')
intrain<-sample(nrow(dataseta),samplesize*5)
dataset<-dataseta[intrain,]
rm(dataseta)
rm(list=c('onestar','twostar','threestar','fourstar','fivestar'))




test <- Corpus(VectorSource(dataset$text))
test <- tm_map(test, stripWhitespace)
test <- tm_map(test, content_transformer(tolower))
test <- tm_map(test, removeWords, c(stopwords("english"),"i")) 
#test <- tm_map(test, stemDocument)
tokenize_bigrams <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)) 
system.time(tdm <- DocumentTermMatrix(test, control = list(tokenize=tokenize_bigrams)))
system.time(tdm2<-removeSparseTerms(tdm,0.9992))
#tdm2<-rollup(tdm,2,na.rm=TRUE,FUN=sum)
system.time(m <- as.matrix(tdm2))
# get word counts in decreasing order
word_freqs = sort(colSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)




#create word matrix for naive bayes with minDocFreq=1000freq
matrix = create_matrix(restaurantreviews$text, minDocFreq=200, language = "english", removeStopwords = TRUE, 
                       removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)

#Create naive bayes classifier to classify review rating based on text
classifier = naiveBayes(training$text, as.factor(training$rating))

library(RTextTools)
container<-create_container(tdm,as.factor(dataset$stars),trainSize=1:30000,testSize=30001:50000,virgin=FALSE)

system.time(svmmodel<-train_model(container,"SVM"))
predicted<-classify_model(container,svmmodel)
confusion<-confusionMatrix(predicted$SVM_LABEL,as.factor(dataset$stars[30001:50000]))
confusion<-table(predicted$SVM_LABEL,as.factor(dataset$stars[30001:50000]))
confusionm<-data.frame(confusion)
heatmap(confusion, Rowv='Actual', Colv='Predicted', col = heat.colors(64))
ggplot(data = data.frame(confusionm), aes(x=Var1, y=Var2, fill=Freq))+geom_tile()


#generate random data 
data = data.frame(sample(LETTERS[0:20], 100, replace=T),sample(LETTERS[0:20], 100, replace=T))
names(data) = c("Actual", "Predicted") 

#compute frequency of actual categories
actual = as.data.frame(table(dataset$stars[40001:50000]))
names(actual) = c("Actual","ActualFreq")

#build confusion matrix
confusion = as.data.frame(table(predicted$SVM_LABEL,as.factor(dataset$stars[40001:50000])))
names(confusion) = c("Actual","Predicted","Freq")

#calculate percentage of test cases based on actual frequency
confusion = merge(confusion, actual, by=c('Actual'))
confusion$Percent = confusion$Freq/confusion$ActualFreq*100

#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +geom_tile(aes(x=Predicted, y=Actual,fill=Percent),data=confusion, color="black",size=0.1)+labs(x="Actual",y="Predicted")
tile = tile + 
  geom_text(aes(x=Predicted,y=Actual, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black")+scale_fill_gradient(low="grey",high="red")

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Predicted,y=Actual),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

#render
tile


