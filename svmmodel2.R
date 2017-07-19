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

#Load list of words and their correlations across stars
#tried corxy<1, 33.74% accuracy
#corxy<2,35.41 accuracy
#corxy<10, 45.48 accuracy
#nocorxy 47.9 accuracy
setwd('D:/Mike/Documents/Jean Marie Lab/yelpdata')
load('datasetevensample200kpart2.RData')
load('trainingset.RData')
dataset<-restaurantreviews[1:200000,'stars']
dataset<-dataset$stars
rm(restaurantreviews)
load('wordCorBigRandomSample.RData')
lowCor<-wordCor[wordCor$corxy>5,'word']
lowCor<-as.character(lowCor)
mlowCor<-m[,!(colnames(m) %in% lowCor)]
rm(m)
#tdmcontainer<-create_container(mlowCor,as.factor(dataset$stars),trainSize=1:20000,testSize=20001:30000,virgin=FALSE)

#Drop words below word freq
summatrix<-apply(mlowCor,1,sum)
minWordCount<-5
mlowCorPruned<-mlowCor[unname(summatrix>minWordCount),]
#starstest<-as.factor(dataset[unname(summatrix>minWordCount)])
starstest3<-as.factor(starstest3[unname(summatrix>minWordCount)])
table(starstest3)

#evensampling
samplesize=min(table(starstest))
onestarm<-mlowCorPruned[starstest==1,]
twostarm<-mlowCorPruned[starstest==2,]
threestarm<-mlowCorPruned[starstest==3,]
fourstarm<-mlowCorPruned[starstest==4,]
fivestarm<-mlowCorPruned[starstest==5,]
onestarm<-onestarm[1:samplesize,]
twostarm<-twostarm[1:samplesize,]
threestarm<-threestarm[1:samplesize,]
fourstarm<-fourstarm[1:samplesize,]
fivestarm<-fivestarm[1:samplesize,]
starseven<-c(rep(1,samplesize),rep(2,samplesize),rep(3,samplesize),rep(4,samplesize),rep(5,samplesize))
mlowCorPrunedEvenSample<-rbind(onestarm,twostarm,threestarm,fourstarm,fivestarm)
intrain<-sample(samplesize*5,samplesize*5)
mlowCorPrunedEvenSample<-mlowCorPrunedEvenSample[intrain,]
starstest<-starseven[intrain]
rm(list=c('mlowCor','mlowCorPruned'))
rm(list=c('onestarm','twostarm','threestarm','fourstarm','fivestarm','starseven'))
sharewords<-colnames(mlowCorPrunedEvenSample) %in% colnames(mlowCorPrunedEvenSample2)
sharewords2<-colnames(mlowCorPrunedEvenSample2) %in% colnames(mlowCorPrunedEvenSample)
m<-rbind(mlowCorPrunedEvenSample[,sharewords],mlowCorPrunedEvenSample2[,sharewords2])
starstest<-as.factor(c(starstest,starstest2))
mWordRes<-rbind(mlowCorPrunedEvenSample[,!sharewords],matrix(0,nrow(mlowCorPrunedEvenSample2),ncol(mlowCorPrunedEvenSample[,!sharewords])))
mWordRes2<-rbind(matrix(0,nrow(mlowCorPrunedEvenSample),ncol(mlowCorPrunedEvenSample2[,!sharewords2])),mlowCorPrunedEvenSample2[,!sharewords2])
m<-cbind(m,mWordRes,mWordRes2)
intrain<-sample(nrow(m),nrow(m))
m<-m[intrain,]
starstest<-starstest[intrain]

#Weigh Word Matrix by wordCor
wordCor<-wordCor[wordCor$word %in% colnames(m),]
wordCor$word<-as.character(wordCor$word)
for(i in 1:nrow(wordCor)){m[,wordCor$word[i]]<-m[,wordCor$word[i]]/wordCor$corxy[i]}

trainsize=65000
testsize=trainsize+1
tdmcontainer<-create_container(m,starstest,trainSize=1:trainsize,testSize=testsize:length(starstest),virgin=FALSE)
#Create SVM Classifier
system.time(svmmodel<-train_model(tdmcontainer,"SVM"))
#system.time(rfmodel<-train_model(tdmcontainer,"RF",ntree=200))
#system.time(modelr<-randomForest(x=mlowCorPruned[1:1000,],y=starstest[1:1000],xtest=mlowCorPruned[1001:length(starstest),],ytest=starstest[1001:length(starstest)]))
predsvm<-classify_model(tdmcontainer,svmmodel)

library(caret)
#confusionMatrix(predsvm$FORESTS_LABEL,as.factor(dataset$stars[20001:30000]))
confusionMatrix(predsvm$SVM_LABEL,as.factor(starstest[testsize:length(starstest)]))

#compute frequency of actual categories
#actual = as.data.frame(table(starscommon[1:length(predvec)]))
actual = as.data.frame(table(starstest[testsize:length(starstest)]))
names(actual) = c("Actual","ActualFreq")

#build confusion matrix
#confusion = as.data.frame(table(predvec,starscommon[1:length(predvec)]))
confusion<- as.data.frame(table(predsvm$SVM_LABEL,as.factor(starstest[testsize:length(starstest)])))
names(confusion) = c("Predicted","Actual","Freq")

#calculate percentage of test cases based on actual frequency
confusion = merge(confusion, actual, by=c('Actual'))
confusion$Percent = confusion$Freq/confusion$ActualFreq*100
library(dplyr)
confusion<-mutate(confusion,var=abs(as.integer(Predicted)-as.integer(Actual))*Freq)
var<-sum(confusion$var)/sum(confusion$Freq)
#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
library(ggplot2)
tile <- ggplot() +geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1)+labs(x="Actual",y="Predicted")
tile = tile + 
  geom_text(aes(x=Actual, y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black")+scale_fill_gradient(low="grey",high="red")

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Actual, y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

#render
tile

