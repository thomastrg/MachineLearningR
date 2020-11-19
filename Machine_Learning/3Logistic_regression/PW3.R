#1
#Le fichier se trouve danss mon working directory
social_Network_Ads <- read.csv("Social_Network_Ads.csv")




#2
summary(social_Network_Ads)
#The dataset has 400lines, and 5 columns
#5 cols: USER,GENDER,AGE,ESTIMATED SALARY,PURCHASED
cor(social_Network_Ads$EstimatedSalary,social_Network_Ads$Age)
#faible corrélation car proche de 0 (0.155)
str(social_Network_Ads)

library(ggplot2)
p<-ggplot(social_Network_Ads,aes(x=Age)) + 
  geom_histogram(aes(fill=..count..),bins=40) +
  scale_fill_gradient("Count",low='pink',high='red')
p
#we make an histogram in function of the age we can see that the estimated salary is better around 35-40 years old
ggplot(social_Network_Ads,aes(x=Age))+geom_histogram(aes(y=..density..), colour="black", fill="white",bins=60)+geom_density(alpha=.2, fill="#FF6666")
library(car)
library(carData)
require(carData)
require(car)
scatterplot(EstimatedSalary~Age,smooth=FALSE,boxplots=FALSE,data=social_Network_Ads,pch=21)
#it is not very accurate, there are a lot of scattered values so these 2 features are not correlated

#3

library(caTools) 
set.seed(123)
split = sample.split(social_Network_Ads$Purchased, SplitRatio = 0.75)
training_set = subset(social_Network_Ads, split == TRUE)
test_set = subset(social_Network_Ads, split == FALSE)



#4
#Input variables = Age & EstimatedSalary
#scaling consists in transforming the data in order to put it on comparable scales with other  variables
#To scale manually, you have to substract the variable by the mean and divide by the std (standard deviation) of the variable 
# = ((x-mean)/std(x))

training_set[,3:4]<-scale(training_set[,3:4],center=TRUE,scale=TRUE)

test_set[,3:4]<-scale(test_set[,3:4],center=TRUE,scale=TRUE)

test_set

#5
first_logistic_reg<-glm(Purchased~Age,family = "binomial",data=social_Network_Ads)
summary(first_logistic_reg)
#We can see age is a good predictor thanks to 3 stars
plot(social_Network_Ads$Age,social_Network_Ads$Purchased,xlim=c(0,80),xlab='age',ylab='Purchase probability')
x<-seq(0,80,l=200)
y<- exp(-(first_logistic_reg$coefficients[1]+first_logistic_reg$coefficients[2]*x))
y<- 1/(1+y)
lines(x,y,col=2,lwd=2)
# it shows a logic interpretation, that older people are more able to purchase than young people.



#6
#We choose the argument family to be binomial in order to do a logistic regression, because if we don't 
#it could be another type of linear model (gaussian,gamma,poisson) that's why we have to choose this argument
#furthermore it is not that one by deault that's why we have to mention it


#7
#The logistic distribution is :
#F(z)=e^z/(1+e^z)=1/(1+e^-z)
#p(x)=Beto0+Beta1*x
#We can find the coefficients in the summary


#8
summary(first_logistic_reg)
#As we can see on the summary the z value indicator is very good we have 3 stars
#which means it is very close to 0, so the significance of the age is very good
# for the purchase


#9
model_training=glm(Purchased~Age,family= "binomial", data=training_set)
summary(model_training)
#The AIC we have is 256.11

#10
ggplot(training_set,aes(x=Age,y=Purchased))+geom_point()+
  stat_smooth(method = 'glm',method.args = list(family='binomial'),se=FALSE) 


#11
second_logistic_model=glm(Purchased~Age+EstimatedSalary,family =  'binomial',data=training_set)
second_logistic_model

#12
summary(second_logistic_model)
#The model is signifcant beause we have 3 stars for the Pr(>z) which is very good near 0.
# for both features (Age and salary)

#13
# In fact yes we obtain a better model because the AIC is lower ( AIC =205 <256.11) and the lower it is the better it is

#14
predictionResproba=predict(second_logistic_model,test_set,type='response')
predictionResproba
#15
predictionRes=ifelse(predictionResproba<=0.50,0,1) #yHat
predictionRes
#16

confusion=table(predictionRes,test_set$Purchased)
confusion


#17
#SOURCE RECHERCHES : STHDA COURSE CLASSIFICATION METHODS ESSENTIALS
calculModel =function(confusionTab) {
  precision=confusionTab[2,2]/(confusionTab[2,2]+confusionTab[1,2]) 
  #precision = TruePositives/ (TruePositives+FalsePositives)
  sensitivity=confusionTab[2,2]/(confusionTab[2,2]+confusionTab[2,1])
  #Sensitivity = TruePositives/(TruePositives + FalseNegatives
  specificity=confusionTab[1,1]/(confusionTab[1,1]+confusionTab[2,1])
  #Specificity = TrueNegatives/(TrueNegatives + FalseNegatives)
  accuracy=(confusionTab[2,2]+confusionTab[1,1])/sum(confusionTab)
  #Accuracy=(TruePositives + TrueNegatives)/SampleSum
  data.frame(value = c(precision,sensitivity,specificity,accuracy))
}
df=data.frame(t(calculModel(confusion)))
names(df)=c('precision','sensitivity','specificity','accuracy')
df

#lets verify our answers

library(caret)
library(e1071)
library(pROC)
confusionMatrix(table(data=predictionRes,reference=test_set$Purchased))
#there is an inversion between true negatives and falses positives but values are right

#18

library(ROCR)

elements=prediction(predictionRes,test_set$Purchased)

AUC=performance(elements,measure ='auc')
AUC=AUC@y.values
RocrPerf=performance(elements,'tpr','fpr' )
plot(RocrPerf,main='ROC curve',colorize=T)



#19

#Wwe already did the one with age and estimated salary on the question 18
#Lets do the one with only age
PredictionRes2=predict(model_training,test_set,type='response')
elements2=prediction(PredictionRes2,test_set$Purchased)
AUC2=performance(elements2,measure='auc')
AUC2=AUC2@y.values
RocrPerf2=performance(elements2,'tpr','fpr')

plot(RocrPerf2,main='ROC curve',col='black',add=TRUE)


#The one with two features is more linear than the one with only age
#The model with one feature is most of the time higher than the one with 2 features
#the highest is the best according to the position where it is higher








