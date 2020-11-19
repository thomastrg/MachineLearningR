#Q.1
library(MASS)

#dim(Boston)
dim(Boston)

#Q2
training=Boston[1:400,]
#dim(train)
testing=Boston[-(1:400),]
#dim(test)
dim(testing)




#Q3
cor(Boston$age,Boston$medv)
#Le coefficient de corrélation est entre -1<r<0 et il est plus proche de 0
#que de -1 donc la relationlinéaire entre les variables est faible et de mauvaise qualité (-0.37)
#http://www.astro.ulg.ac.be/cours/magain/STAT/Stat_Main_Fr/Chapitre7.html

#Q4
model=lm(medv~age,data=training)
model
#medv = median house value
plot(training$age,training$medv,col='green',xlab="Age",ylab="MEDV= Median House Value",pch=20)
abline(model,col='red',lwd=3)
#Oldest houses have a Med House value lower than youngest houses which makes sense


#Q5
modelBis=lm(medv~log(lstat)+age,data=training)
modelBis
abline(modelBis)
#The linear regression line is decreasing it goes null very fast

#Q6
summary(modelBis)


#Q7
#The p value is equal to 2.2e-16 which is very low so predictors are good
# and the R squared is 67% which is good result because it is above 60%



#Q8
y_hat=predict(modelBis,data.frame(lstat=(testing$lstat),age=(testing$age)))
y=testing$medv
RMSE=mean((y-y_hat)^2)
RMSE
#RMSE is 23 and R squarred is 0.66 
#the p-value: < 2.2e-16 so it's significant because the p value is very low




#Q9
modelThird= lm(medv~.,data=training)
modelThird
summary(modelThird)

#Q10
modelFourth=lm(medv~.+log(lstat)-lstat,data=training)
modelFourth
summary(modelFourth)

#Q11
#In the question 9 the Adjusted R-squared:  0.7249 
#In the tenth, Adjusted R-squared:  0.7777 
#So yes the R improved so we can suppose that the result is better with log


#Q12
round(cor(training),digits=2)


#Q13
library(corrplot)
?corrplot.mixed
M<-cor(Boston)
corrplot.mixed(M,tl.col="Black")

#Q14
cor(Boston$rad,Boston$tax)
#The correlation between these two variables is 0.91 as stated in the corrplot mixed


#Q15
modelFifth=lm(medv~.+log(lstat)-lstat-tax,data=training)
summary(modelFifth)
summary(modelFourth)
#without tax, the R squarred is lower than the model with tax 
#Fourth Adjusted R-squared:  0.7777 -> Fifth 0.7706 
#whereas without tax, the Statistic is higher than the model with tax
#F-statistic Fourth: 108.4 -> Fifth : 112.7


#Q16
y_hatModelFifth=predict(modelFifth,testing)
y_hatModelFifth
RMSE=mean((testing$medv-y_hatModelFifth)^2)
RMSE


#ANOVA
#Q17
length(which(Boston$chas==1))
#there are 35 suburbs bounding the river

#Q18
boxplot(Boston$medv~Boston$chas)
#we can see that bounding the river the median prices are higher than 
#houses which dont bound the river
#CELA SERT A COMPARER VARIABLES QUALITATIF ET QUANTITATIF

#Q19
aggregate(training$medv,by=list(training$chas),FUN=mean)
#23.94 and 28.44
#u0 et u1 sont les moyennes des prix e nfonction de chas



#20
modelsixth=aov(medv~chas,data=training)
anova(modelsixth)
modelsixthBis= lm(medv~chas,data=training)
summary(modelsixthBis)
#We find the same result for the p value which is equal to 0.005 (0.5%) so lower than 1%
#We print summary modelSixth Bis in order to verify our result
# So there is a good relationship between median prices and chas

#QUALITATIVE PREDICTORS

#21
ano=aov(medv~chas+crim,data=training)
ano$coefficients
summary(ano)
#We can see that the chas'coefficient is 4.5 which is quite high and makes the medv rise 
#and the crim'coefficient is -0.4 whi makes the median price low

#22
#the chas'p value is 0.004(4%) so it is significant 
#but the crim'p value is much more lower so it has a biggest influence on medv

#23
modelseventh=lm(medv~lstat*age,data=training)
summary(modelseventh)
#the model is very significant because the p value is 2.2*10^-16


#24
modelFinal=lm(medv~.*.,data=training)
summary(modelFinal)
#this one tries every combination of features
#we can see the fiability thanks to the stars next to p value
#so rad:tax, rm:lstat,rm:black,chas:black give the best p values



#fiability is verified by p value
#Quality is verified by R squarred








