#1
library(MASS)
library(caTools)
set.seed(18)
Boston_idx = sample(1:nrow(Boston), nrow(Boston) / 2) 
# You don't know what we just did?
# open the documentation of the function sample by 
# writing ?sample in the R console.
# Note that this is one of the ways to split it randomly and it is not necessary the best.
Boston_train = Boston[Boston_idx,]
Boston_test  = Boston[-Boston_idx,]

#2 
require(rpart)
Boston_tree<-rpart(medv~.,data=Boston_train)
Boston_tree

#3
plot(Boston_tree)
text(Boston_tree, pretty = 0)
title(main = "Regression Tree")


#4
library(rpart.plot)
rpart.plot(Boston_tree)
prp(Boston_tree)


#5
#summary(Boston_tree)
printcp(Boston_tree)
plotcp(Boston_tree)
#RMSE function

RMSE = function(expected,truevalue){
  sqrt(mean((expected-truevalue)^2))
}
#calculates the square root of the mean of the square of the difference between expected and true values

#6
predictBostonTree<-predict(Boston_tree,newdata = Boston_test)
predictBostonTree
rmseTree<-RMSE(Boston_test$medv,predictBostonTree)
#We calculate the rmse on medv and the prediction

#7
linearmodel1<-lm(medv~.,data = Boston_train)
predLinearmodel1<-predict(linearmodel1,Boston_test)
summary(linearmodel1)
RMSE(Boston_test$medv,predLinearmodel1)
#the RMSE is better on the linear model than the previous one

library(ggplot2)
ggplot(data = Boston_test,aes(x=predictBostonTree,
                              y=medv))+geom_point(color='blue')+geom_abline(color='red')+labs(
                                title='Predicted vs Actual : Single Tree, Test Data',
                                x='Predicted',y='Actual')

ggplot(data=Boston_test,aes(x=predLinearmodel1,y=medv))+geom_point(color='blue')+geom_abline(
  color='red')+labs(title='Predicted vs Actual : Linear model, Test Data',x='Predicted',y='Actual')


#BAGGING
#8
require(randomForest)
bostonRf=randomForest(medv~.,data=Boston_train,mtry=dim(Boston_train)[2]-1)

#9
predictRf=predict(bostonRf,Boston_test)
RMSE(Boston_test$medv,predictRf)
#The RMSE of the model Bagging is much better than linear and single tree models

#RANDOM FORESTS
#10
BostonRfRF=randomForest(medv~.,data=Boston_train,mtry=(dim(Boston_train)[2]-1)/3)
predictRfRF=predict(BostonRfRF,Boston_test)
RMSE(Boston_test$medv,predictRfRF)

#We have the a better RMSE for RANDOM FOREST THAN BAGGING

#11
MatrixPredictors<-importance(BostonRfRF)
MatrixPredictors
classement<-apply(MatrixPredictors,2,sort,index.return=TRUE)
classement$IncNodePurity$x
#So we can see the best predictors for this model are 'rm', 'lstat' and 'crim'
# but for the best predictors during session 2 were lstat, rad,dis,nox,rm
#so we can find similarities

#12
varImpPlot(BostonRfRF)
#we can see a visualisation of the importance of each predictors 

#BOOSTING
#10
require(gbm)
Boston_boost = gbm(medv ~ ., data = Boston_train, distribution = "gaussian", 
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
predictBoost<-predict(Boston_boost,Boston_test)
RMSE(Boston_test$medv,predictBoost)
#THe boost model is even more a better model than bagging as we can see thanks to RMSE

#11
summary(Boston_boost)
#the important variables shown in the graph are the same as the last model used


#COMPARISON
#12

plot1<-ggplot(data = Boston_test,aes(x=predictBostonTree,
                              y=medv))+geom_point(color='blue')+geom_abline(
                                color='red')+labs(title='Predicted vs Actual : Single Tree,
                                                  Test Data',x='Predicted',y='Reality')

plot2<-ggplot(data=Boston_test,aes(x=predictRf,
                                   y=medv))+geom_point(color='blue')+geom_abline(
                                     color='red')+labs(title='Predicted vs Actual : Bagging on 
                                                       Test Data',x='Predicted',y='Reality')
plot3<-ggplot(data=Boston_test,aes(x=predictRfRF,
                                   y=medv))+geom_point(color='blue')+geom_abline(
                                     color='red')+labs(title='Predicted vs Actual : Random Forest on 
                                                       Test Data',x='Predicted',y='Reality')
plot4<-ggplot(data=Boston_test,aes(x=predictBoost,
                                   y=medv))+geom_point(color='blue')+geom_abline(
                                     color='red')+labs(title='Predicted vs Actual : Boosting on 
                                                       Test Data',x='Predicted',y='Reality')
require(gridExtra)
grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2)

model<-c("Regression Linear","Bagging","Random Forest","Boosting")



#THE DIFFERENCE BETWEEN REGRESSION AND CLASSIFICATION TREES
# FOR A REGRESSION TREE THE PREDICTED RESPONSE IS GIVEN BY THE MEAN OF THE TRAINING OBSERVATIONS
#FOR A CLASSIFICATION TREE THE PREDICTED RESPONSE IS GIVEN BY THE CLASS MOST OCCURING OF THE TRAIN SET
set.seed(18)
spam <- read.csv("spam.csv")
str(spam)
class(spam)

#split train and test set 


spam_idx = sample(1:nrow(spam), nrow(spam) / 2) 
spam_train = spam[spam_idx,]
spam_test  = spam[-spam_idx,]



spam$spam<-as.factor(spam$spam)




#Logistic regression
logisticModelSpam<- glm(spam~.,family ='binomial',data=spam_train )
predictionLogisticSpam<-predict(logisticModelSpam,spam_test,type='response')
pred_0_1<-ifelse(predictionLogisticSpam>=0.5,1,0)
accLogistic<-mean(spam_test$spam==pred_0_1)




require(rpart)
#classification tree
spamtree<-rpart(spam~.,data=spam_train,method='anova')

plot(spamtree)
text(spamtree,pretty=0)
title(main='Regression Tree Spam')

predictSpamTree<-predict(spamtree,newdata = spam_test)
predictTree0_1<-ifelse(predictSpamTree>=0.5,1,0)
accClassfication<-mean(spam_test$spam==predictTree0_1)

#Bagging 
require(randomForest)
spamRf=randomForest(spam~.,data=spam_train,mtry=dim(spam_train)[2]-1)
predictspamRf=predict(spamRf,spam_test)
#We should have TRUE or FALSE in this model but we have double
#So we shouldn't use "ifelse" but the result is not good 
predictspamRf0_1<-ifelse(predictspamRf>=0.5,1,0)
accuBagging<-mean(spam_test$spam==predictspamRf0_1)
#accuBagging<-mean(spam_test$spam==predictspamRf)
accuBagging


#RANDOM FORESTS

spamRfRF=randomForest(spam~.,data=spam_train,mtry=(dim(spam_train)[2]-1)/3)
predictspamRfRF=predict(spamRfRF,spam_test)
predictSpamRfRf0_1<-ifelse(predictspamRfRF>=0.5,1,0)
accRandomForest<-mean(spam_test$spam==predictSpamRfRf0_1)


#BOOSTING

require(gbm)
spam_boost = gbm(spam ~ ., data = spam_train, distribution = "gaussian", 
                 n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
predictSpamBoost<-predict(spam_boost,spam_test)
#Hesitation between distribution gaussian or bernouilli
predictSpamBoost0_1<-ifelse(predictSpamBoost>=0.5,1,0)
accuracyBoosting<-mean(spam_test$spam==predictSpamBoost0_1)
#THe boost model is even more a better model than bagging as we can see thanks to RMSE



#presentation of results
nom<-c("Logistic Reg","classificationTree","Bagging","Random Forest","Boosting")
acc<-c(accLogistic,accClassfication,accuBagging,accRandomForest,accuracyBoosting)

dfAcc<-data.frame(nom,acc)
dfAcc