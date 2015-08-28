NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)
NewsTest$Popular =2

#categorical features must have same levels

NewsTrain$SectionName[NewsTrain$SectionName=="Style"]=""
NewsTrain$SectionName[NewsTrain$SectionName=="Sports"]=""
NewsTrain$SubsectionName[NewsTrain$SubsectionName=="Fashion & Style"]=""
NewsTrain$SubsectionName[NewsTrain$SubsectionName=="Politics"]=""
NewsTrain$NewsDesk[NewsTrain$NewsDesk=="National"]=""
NewsTrain$NewsDesk[NewsTrain$NewsDesk=="Sports"]=""

data=rbind(NewsTrain,NewsTest)
data=data[order(data$UniqueID, decreasing=FALSE),, drop=FALSE]

library(tm)

Words=function(text,sparsity){

corpus = Corpus(VectorSource(text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
#dtm = DocumentTermMatrix(corpusH,control=list(weighting=weightTfIdf))
dtmd = removeSparseTerms(dtm,sparsity)
dtmd = as.data.frame(as.matrix(dtmd))

dtmd$UniqueID=data$UniqueID
dtmd$Popular=data$Popular
dtmd
}

#WordsPower is a function that gives the proportion of the popular or non popular words in each Headline or Abstract
#ex.let w1 and w0 the occurences of words in Popular and non Popular Headlines
#wp=w1/(w0+w1) and wnp=w0/(w0+w1)
#then multiply those vectors with the whole matrix(words,Headlines) 

WordsPower=function(Words,Pop){

Words0=subset(Words,Words$Popular==0)
id0=Words0$UniqueID
Words0$Popular=NULL
Words0$UniqueID=NULL

Words1=subset(Words,Words$Popular==1)
id1=Words1$UniqueID
Words1$Popular=NULL
Words1$UniqueID=NULL

Words2=subset(Words,Words$Popular==2)
id2=Words2$UniqueID
Words2$Popular=NULL
Words2$UniqueID=NULL

wp0=colSums(as.matrix(Words0))
wp1=colSums(as.matrix(Words1))

if (Pop==0){
	wp=wp0/(wp1+wp0)
}
else{
	wp=wp1/(wp1+wp0)
}
	wpd=as.data.frame(wp)
	wpd[is.na(wpd)]=0
	wp=as.matrix(wpd)
	p0=(as.matrix(Words0))%*%wp
	p1=(as.matrix(Words1))%*%wp
	p2=(as.matrix(Words2))%*%wp

Words0$P=p0
Words1$P=p1
Words2$P=p2

Words0$UniqueID=id0
Words1$UniqueID=id1
Words2$UniqueID=id2

Words=rbind(Words0,Words1,Words2)
Words=Words[order(Words$UniqueID, decreasing=FALSE),, drop=FALSE]
Words$P
}

HeadlineWords=Words(data$Headline,0.9995)
AbstractWords=Words(data$Abstract,0.999)

HeadlinePower1=WordsPower(HeadlineWords,1)
HeadlinePower0=WordsPower(HeadlineWords,0)
AbstractPower1=WordsPower(AbstractWords,1)
AbstractPower0=WordsPower(AbstractWords,0)


#Feature Selection

data$HP=HeadlinePower1
data$HNP=HeadlinePower0
data$AP=AbstractPower1
data$ANP=AbstractPower0
data$HPNP=data$HP-data$HNP

data$SN = as.factor(data$SectionName)
data$SSN = as.factor(data$SubsectionName)
data$ND = as.factor(data$NewsDesk)
data$PubDate = strptime(data$PubDate, "%Y-%m-%d %H:%M:%S")
data$WD =as.factor(data$PubDate$wday)
data$H= as.factor(format(data$PubDate,"%H"))
data$WordCount= data$WordCount
data$Wlog=log(data$WordCount+1)

#Split/Cross-validation

train=subset(data,(data$Popular==0)|(data$Popular==1))
test=subset(data,data$Popular==2)

library(caTools)

set.seed(11)
spl=sample.split(train$Popular,0.75)
trainCV=subset(train,spl==TRUE)
testCV=subset(train,spl==FALSE)

#Models

library(randomForest)
library(gbm)
library(e1071)

#randomforests(originally used tfidf for HPNP feature)

modelRF1=randomForest(as.factor(Popular)~SN+SSN+ND+WD+WordCount+HP,data=train)
varImpPlot(modelRF)
predRF1=predict(modelRF1,newdata=test,type="prob")[,2]

modelRF2=randomForest(as.factor(Popular)~SN+SSN+ND+WD+WordCount+HPNP,data=train)
predRF2=predict(modelRF2,newdata=test,type="prob")[,2]

modelRF3=randomForest(as.factor(Popular)~SN+SSN+ND+WD+H+WordCount,data=train)
predRF3=predict(modelRF3,newdata=test,type="prob")[,2]

#gbm(cross-validation for shrinkage,interaction-depth and trees for each model)

modelGBM1=gbm(Popular~SN+SSN+ND+WD+HP+WordCount,data=train,shrinkage=0.01,interaction.depth=5,n.trees=3000,distribution="adaboost")
modelGBM
plot(modelGBM, i = "HP")
predGBM1=predict(modelGBM1,newdata=test,n.trees=1000,type="response")

modelGBM2=gbm(Popular~SN+SSN+ND+WD+HP+WordCount,data=train,shrinkage=0.01,interaction.depth=5,n.trees=3000,distribution="bernoulli")
predGBM2=predict(modelGBM2,newdata=test,n.trees=1000,type="response")

modelGBM3=gbm(Popular~SN+SSN+ND+WD+HPNP+WordCount,data=train,shrinkage=0.01,interaction.depth=5,n.trees=3000,distribution="adaboost")
predGBM3=predict(modelGBM3,newdata=test,n.trees=1000,type="response")

modelGBM4=gbm(Popular~SN+SSN+ND+WD+HPNP+WordCount,data=train,shrinkage=0.01,interaction.depth=5,n.trees=3000,distribution="bernoulli")
predGBM4=predict(modelGBM4,newdata=test,n.trees=1000,type="response")

modelGBM5=gbm(Popular~SN+SSN+ND+WD+H+WordCount,data=train,shrinkage=0.01,interaction.depth=5,n.trees=3000,distribution="adaboost")
predGBM5=predict(modelGBM5,newdata=test,n.trees=1000,type="response")

modelGBM6=gbm(Popular~SN+SSN+ND+WD+H+WordCount,data=train,shrinkage=0.01,interaction.depth=5,n.trees=3000,distribution="bernoulli")
predGBM6=predict(modelGBM6,newdata=test,n.trees=1000,type="response")

#for cross-validation(trees selection)

n.trees = seq(from = 100, to = 1000, by = 50)
predmat = predict(modelGBM, newdata = testCV, n.trees = n.trees,type="response")
berr = with(testCV, apply((predmat - Popular)^2, 2, mean))
plot(n.trees, berr, pch = 19, ylab = "Mean Squared Error", xlab = "# Trees", 
    main = "Boosting Test Error")

#glm

modelGLM1=glm(Popular~SN+SSN+ND+WD+H+WordCount,data=train,family=binomial)
predGLM1=predict(modelGLM1,newdata=test,type="response")

modelGLM2=glm(Popular~SN+SSN+ND+WD+HP+WordCount,data=train,family=binomial)
predGLM2=predict(modelGLM2,newdata=test,type="response")

#svm

model=svm(as.factor(Popular)~SN+SSN+ND+WordCount+WD+H++HP+HPNP,data=train,probability=TRUE)
predSVM=predict(model,probability=TRUE)


#vote3 is function for conflicts between 3 models(geometric mean)

vote3=function(f,g,l){

a000=0
a001=0
a010=0
a011=0
a100=0
a101=0
a110=0
a111=0
pred=0

for (i in test$UniqueID){
	i=i-6532
	if ((f[i]>=0.5)&(g[i]>=0.5)&(l[i]>=0.5)){
		pred[i]=mean(c(f[i],g[i],l[i]))
		a111=a111+1
		}
 	if ((f[i]<0.5)&(g[i]<0.5)&(l[i]<0.5)){
		pred[i]=mean(c(f[i],g[i],l[i]))
		a000=a000+1
		}
	if ((f[i]>=0.5)&(g[i]>=0.5)&(l[i]<0.5)){
		pred[i]=sqrt(f[i])*sqrt(g[i])
		a110=a110+1
		}
	if ((f[i]<0.5)&(g[i]>=0.5)&(l[i]>=0.5)){
		pred[i]=sqrt(g[i])*sqrt(l[i])
		a011=a011+1
		}
	if ((f[i]>=0.5)&(g[i]<0.5)&(l[i]>=0.5)){
		pred[i]=sqrt(f[i])*sqrt(l[i])
		a101=a101+1
		}
	if ((f[i]<0.5)&(g[i]<0.5)&(l[i]>=0.5)){
		pred[i]=sqrt(f[i])*sqrt(g[i])
		a001=a001+1
		}
	if ((f[i]<0.5)&(g[i]>=0.5)&(l[i]<0.5)){
		pred[i]=sqrt(f[i])*sqrt(l[i])
		a010=a010+1
		}
	if ((f[i]>=0.5)&(g[i]<0.5)&(l[i]<0.5)){
		pred[i]=sqrt(g[i])*sqrt(l[i])
		a100=a100+1
		}
}
pred
}

predsall1=vote3((predRF1+predRF2+predRF3)/3,(predGBM1+predGBM2+predGBM3+predGBM4+predGBM5+predGBM6)/6,predGLM1)
predsall2=vote3((predRF1+(predGBM1+predGBM2)/2)/2,(predRF2+(predGBM3+predGBM4)/2)/2,(predRF3+(predGBM5+predGBM6)/2+predGLM1)/3)
predsall=sqrt(predsall1)*sqrt(predsall2)

MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = predsall)
write.csv(MySubmission, "submissionA.csv", row.names=FALSE)

MySubmission = data.frame(UniqueID = test$UniqueID, Probability1 = predsall2)
write.csv(MySubmission, "submissionB.csv", row.names=FALSE)






