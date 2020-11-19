#1
#Le fichier se trouve danss mon working directory
social_Network_Ads <- read.csv("Social_Network_Ads.csv")
str(social_Network_Ads)
summary(social_Network_Ads)
boxplot(Age ~ Purchased, data=social_Network_Ads, col = "blue", main="Boxplot Age ~ Purchased")
#We can see that the majority of people who don't purchase are between 27and 38 with a mean of 34
#whereas majority of people who purchase are between 40 and 52 with a mean of 45
boxplot(EstimatedSalary ~ Purchased, data=social_Network_Ads,col = "red",
        main="Boxplot EstimatedSalary ~ Purchased")
#We can see that the majority of people who don't purchase have an estimated salary between 40k and 78k with a mean of 60k
#whereas majority of people who purchase have a salary between 40k and 120k with a mean of 80k 
#which means that they are more able to afford it.


aov(EstimatedSalary ~Purchased, data=social_Network_Ads)
summary(aov(EstimatedSalary ~Purchased, data=social_Network_Ads))
#the p value of the anova test is very good very close to 0 with 3 stars
#So the purchased feature is very important to the model and to the feature estimated Salary
summary(aov(Age ~Purchased, data=social_Network_Ads))
#the p value of the anova test is very good very close to 0 with 3 stars
#So this feature purchased feature is very important to the model and to the age



#We can see that women purchase more than men 
table(social_Network_Ads$Gender,social_Network_Ads$Purchased)
mosaicplot(~ Purchased + Gender, data=social_Network_Ads,
           main = "MosaicPlot of two categorical variables: Puchased & Gender",
           color = 2:3, las = 1)
#, the p value is high so we can't reject independance
#so it is dependant
chisq.test(social_Network_Ads$Purchased, social_Network_Ads$Gender)
social_Network_Ads = social_Network_Ads[3:5]
str(social_Network_Ads)



library(caTools)

social_Network_Ads


set.seed(123)
split = sample.split(social_Network_Ads$Purchased, SplitRatio = 0.75)
training_set = subset(social_Network_Ads, split == TRUE)
test_set = subset(social_Network_Ads, split == FALSE)



#[-3] means we don't take the 3rd column
training_set[-3] <- scale(training_set[-3]) #only first two columns
test_set[-3] <- scale(test_set[-3])




classifier.logreg <- glm(Purchased ~ Age + EstimatedSalary , family = binomial, data=training_set)
classifier.logreg
summary(classifier.logreg)


pred.glm = predict(classifier.logreg, newdata = test_set[,-3], type="response")

pred.glm_0_1 = ifelse(pred.glm >= 0.5, 1,0)

head(pred.glm)
head(pred.glm_0_1)

cm = table(test_set[,3], pred.glm_0_1)
cm

cm = table(pred.glm_0_1, test_set[,3])
cm

mosaicplot(cm,col=sample(1:8,2))
library(ROCR)
require(ROCR)
score <- prediction(pred.glm,test_set[,3]) # we use the predicted probabilities not the 0 or 1

performance(score,"auc") # y.values
plot(performance(score,"tpr","fpr"),col="green")
abline(0,1,lty=8)

#classifier.logreg is the model logistic regression (Purchased in function of age and estimated salary)




#2 plot the decision boundary with logistic reg
#intercept is the ordonnee a l'origine
#y =intercept+slope*x
#the line represents the separatio between 0 and 1 for purchased
coef(classifier.logreg)

slope<-coef(classifier.logreg)[2]/(-coef(classifier.logreg)[3])
slope
intercept<-coef(classifier.logreg)[1]/(-coef(classifier.logreg)[3])
intercept

plot(test_set$Age,test_set$EstimatedSalary,xlab='age',ylab='Estimated salary')
grid()
abline(intercept,slope)
#To verify our answer we can see that the intercept is good on th graph(0.85)
#And the slope is negative and almost equal to -2 (-1.88)


#3
bg=ifelse(pred.glm_0_1==1,'blue','red')
plot(test_set$Age,test_set$EstimatedSalary,xlab='age',ylab='Estimated salary',col=bg,pch=21,main='Decision Boundary Logistic Reg Salary/Age')
grid()
abline(intercept,slope,lwd=3)


#4
bg4=ifelse(test_set[3]==1,'blue','red')
plot(test_set$Age,test_set$EstimatedSalary,xlab='age',ylab='Estimated salary',col=bg4,pch=21,main='Decision Boundary Logistic Reg Salary/Age with real labels')
grid()
abline(intercept,slope,lwd=3)

cm
#We have the same results as the confusion matrix when we count on the plot false negatives
#Indeed we have a sum of 17 points which shouldn't be where they are

#LDA
#5
library(MASS)
classifier.lda <- lda(Purchased~Age+EstimatedSalary, data=training_set)
classifier.lda
#the goal is to model the distribution of each variable by a gaussian depending on what we want to predict

#6
#we get the oefficients of this probability law
#prior represents the distribution of classes
classifier.lda$prior
classifier.lda$means
#means  of variables linked to classes

#7
predictlLda <- predict(classifier.lda,newdata = test_set)
typeof(predictlLda)
str(predictlLda)
predictlLda

#8
matriceConf<- table(test_set[,3],predictlLda$class)
matriceConf

accuracyLDA=(matriceConf[2,2]+matriceConf[1,1])/sum(matriceConf)
accuracyLDA
accuracyLogistic=(cm[2,2]+cm[1,1])/sum(cm)
accuracyLogistic  
#We have the same accuracy for both confusion matrixes


#9
# create a grid corresponding to the scales of Age and EstimatedSalary
# and fill this grid with lot of points
X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
# Adapt the variable names
colnames(grid_set) = c('Age', 'EstimatedSalary')

# plot 'Estimated Salary' ~ 'Age'
plot(test_set[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

# color the plotted points with their real label (class)
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))

# Make predictions on the points of the grid, this will take some time
pred_grid = predict(classifier.lda, newdata = grid_set)$class

# Separate the predictions by a contour
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)


#LDA from scratch


#10
#10.1
training_set
class0=subset(training_set,training_set$Purchased==0)
dim(class0)
class1=subset(training_set,training_set$Purchased==1)
dim(class1)
#the nrows in training is 300 so it is well splited

class0

#10.2
pi0=dim(class0)[1]/dim(training_set)[1]
pi0
pi1=dim(class1)[1]/dim(training_set)[1]
pi1


#10.3
#let's take X1 :Age
#And X2 : Estimated Salary

mu0=c(mean(class0[,1]),mean(class0[,2]))
mu1=c(mean(class1[,1]),mean(class1[,2]))

classifier.lda
#we can verify our results thanks to classifier


#10.4
sigma_var= ((class0[1]-1)*sigma0+((class1)[1]-1)*sigma1)/(class0[1]+class1[1]-2)











