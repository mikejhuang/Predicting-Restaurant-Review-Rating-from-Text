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


#create word matrix for naive bayes with minDocFreq=1000freq
set.seed(2352)
samplesize<-11000
minWordFreq<-20
minWordCount<-10
intrain<-sample(nrow(restaurantreviews),samplesize)
training<-restaurantreviews[intrain,]
testing<-restaurantreviews[-intrain,]
testing<-testing[1:samplesize,]

#Create evenly sampled training set
onestar<-restaurantreviews[restaurantreviews$stars==1,'text']
twostar<-restaurantreviews[restaurantreviews$stars==2,'text']
threestar<-restaurantreviews[restaurantreviews$stars==3,'text']
fourstar<-restaurantreviews[restaurantreviews$stars==4,'text']
fivestar<-restaurantreviews[restaurantreviews$stars==5,'text']

set.seed(1233)
samplesize<-10000
intrain<-sample(length(onestar),samplesize)
dataset<-c(onestar[intrain],twostar[intrain],threestar[intrain],fourstar[intrain],fivestar[intrain])
dataseta<-data.frame(dataset,c(rep(1,10000),rep(2,10000),rep(3,10000),rep(4,10000),rep(5,10000)))
names(dataseta)<-c('text','stars')
intrain<-sample(nrow(dataseta),50000)
dataset<-dataseta[intrain,]

trainingmatrix = create_matrix(dataset$text[1:10000], language = "english", removeStopwords = TRUE, removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)
trainingmatrix <- as.matrix(trainingmatrix)


#Drop words below word freq
trainingmatrix3<-trainingmatrix
trainingmatrix<-trainingmatrix[10001:20000,]
wordfreq<-apply(trainingmatrix,2,sum)
trainingmatrix<-trainingmatrix[,unname(wordfreq>minWordFreq)]
summatrix<-apply(trainingmatrix,1,sum)
trainingmatrix2<-trainingmatrix[unname(summatrix>minWordCount),]
starscommon<-dataset$stars[1:10000]
starscommon<-starscommon[unname(summatrix>minWordCount)]

trainsize<-30000
testsize<-trainsize+1
#Create naive bayes classifier to classify review rating based on text
#classifier<-naiveBayes(trainingmatrix3[1:40000,],as.factor(dataset$stars[1:40000]),laplace=1)
classifier<-naiveBayes(m[1:trainsize,],as.factor(starstest[1:trainsize]),laplace=1) #don't foget comma in index for matrix or prediction is same
#<-predict(classifier,trainingmatrix2,type='raw')
predictedrating<-predict(classifier,m[testsize:length(starstest),],type='raw') #don't foget comma in index for matrix or prediction is same
predvec<-rep(1,length(predictedrating[,1]))
for(i in 1:5){predvec[predictedrating[,i]>0.5]<-i}
library(caret)
starscommon<-starstest[testsize:length(starstest)]
confusionMatrix(predvec,starscommon)
confusion<-table(predvec,starscommon)


#compute frequency of actual categories
actual = as.data.frame(table(starscommon[1:length(predvec)]))
names(actual) = c("Actual","ActualFreq")

#build confusion matrix
confusion = as.data.frame(table(predvec,starscommon[1:length(predvec)]))
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

