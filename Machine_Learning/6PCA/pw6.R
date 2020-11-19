#Load the iris data set 
#1.
irisdf<-read.csv('iris.data')
names(irisdf)

#Exploratory analysis
#2.
#compute 4 boxplots in the same figure
par(mfrow=c(2,2))
plot1<-boxplot(sepal_length~class,data=irisdf,col=c("green","yellow","orange"))
plot2<-boxplot(sepal_width~class,data=irisdf,col=c("green","yellow","orange"))
plot3<-boxplot(petal_length~class,data=irisdf,col=c("green","yellow","orange"))
plot4<-boxplot(petal_width~class,data=irisdf,col=c("green","yellow","orange"))
#The SETOSA has the lowest means for: sepal and petal length, petal width with small interquartile range
#but the highest mean sepal width with a large interquartile range
#The VERSICOLOR has the 2nd means for: sepal and petal length, petal width with small interquartile range
#but the lowest mean for sepal width with a large interquartile range
#The VIRGINCA has the highest means for: sepal and petal length, petal width with large interquartile range
#and the 2nd mean for sepal width with a medium interquartile range



#3.
# Let's use the ggplot2 library
# ggplot2 is the most advanced package for data visualization
# gg corresponds to The Grammar of Graphics.
library(ggplot2)
library("ggpubr")
#of course you must install it first if you don't have it already

# histogram of sepal_length
plot1gg<-ggplot(irisdf, aes(x=sepal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of sepal_width
plot2gg<-ggplot(irisdf, aes(x=sepal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_length
plot3gg<-ggplot(irisdf, aes(x=petal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_width
plot4gg<-ggplot(irisdf, aes(x=petal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
ggarrange(plot1gg,plot2gg,plot3gg,plot4gg,ncol=2,nrow=2)


#PCA using princomp()
#4.
pcairis=princomp(iris[,-5], cor=T) 
str(pcairis)
summary(pcairis) 
plot(pcairis)
#PC1 contains a lot of information
#PC1 and PC2 contain 96% of the information 
#PC3 and PC4 contains only 4% of the information 
#So we should only keep the 2 first variables

biplot(pcairis) 
#sepal width is more impotant for PC2 because it is closer to the vetical axis
#Sepal/petal lengh and Petal width are more important for PC1 beacause they are closer to

#Deeper PCA using factoextra package
#5.
library(factoextra)
#ScreePlot
fviz_eig(pcairis, addlabels = TRUE)
#we can see that we have alreadu 96% of information (73% for the first one and 23% for the second) on the 2 first components which is already high
#the last component is not that important thats why we could choose to not consider it

#Graph of individuals
fviz_pca_ind(pcairis, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)

#We choose to classify our individuals by color in function of the cos2
#All individuals which are similar are grouped on the graph
#we can see many clusters of individuals

#Graph of variables
fviz_pca_var(pcairis, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             )
#we can see that the variable petal length contributes the most to dimension 1 and
#sepal width contributes the most to dimensihon 2
#Petal length and petal width contribute a lot to dimension 1

fviz_pca_biplot(pcairis, repel = TRUE)
#There are too many indivduals so the graph is illisible
#individuals on the group on the left give a high value to the variable "Sepal Width"
#individuals on the group on the right give a high value to the 3 others variables

#contribution of varibles to components 1 & 2

# Contributions of variables to PC1
cont1<-fviz_contrib(pcairis, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
cont2<-fviz_contrib(pcairis, choice = "var", axes = 2, top = 10)
ggarrange(cont1,cont2,ncol=2,nrow=1)
#We can see that for the component 1, variables petal and sepal length and petal witdh contributes a lot
#for the component 2 sepal width contributes the most as we said earlier


#Step by step PCA
#6
#X represents data
#y represents the labels
X <- iris[,-5]
y <- iris[,5]

#7. standardizing
X_scaled<-scale(X,center=TRUE,scale=TRUE)

#8. Covariance Matrix
#2 diferent methods to get the cov matrix
covMat<-cov(X_scaled)
covMat
#calcul method
# %*% :do a matrix multiplication
M=colMeans(X_scaled)
covMatr<-(t(X_scaled-M)%*%(X_scaled-M))/(nrow(X_scaled)-1)
covMatr  

#9 Eigen values and EigenVectors
eigen(covMatr)
#We obtain Eigenvalues and eigen vectors


#Correlation Matrix
#10.
corrMat<-cor(X)
eigen(corrMat)
# the same method except the fact that we do with correlation mat
# we have same results

#11
corrMatScaled<-cor(X_scaled)
eigen(corrMatScaled)
#We have same results as previous 3 questions with same eigenvalues and eigen vectors
  

#Explained variance
#12
temp<-eigen(cor(X))$values
sommeVals<-sum(temp)
pc1<-c(temp[1]/sommeVals,sum(temp[1]/sommeVals))
pc2<-c(temp[2]/sommeVals,sum(temp[1:2]/sommeVals))
pc3<-c(temp[3]/sommeVals,sum(temp[1:3]/sommeVals))
pc4<-c(temp[4]/sommeVals,sum(temp[1:4]/sommeVals))
res<-data.frame(pc1,pc2,pc3,pc4)
#the first line each of the eigen vals
#the second line is the cumulative results of eigen vals


#13
res1<-data.frame(pc=c('pc1','pc2','pc3','pc4'),
                 val=c(res[1,1],res[1,2],res[1,3],res[1,4]))
res1
ggplot(data=res1,aes(x=pc,y=val))+geom_bar(stat='identity')
#we find the same distribution pc as in the beginning as 73% for the first, 23% for the second
      

#projection matrix
#14
projeMat<-eigen(cor(X))$vectors[,1:2]
#we take 2 first columns of eigenvectors as it was asked

#15
yres<-X_scaled%*%projeMat
#Y=X*A with A=projeMat

#16
dffinal<-data.frame(yres)
names(dffinal)<-c('pc1','pc2')
dffinal$class<-irisdf$class
#we put classes on final df
ggplot(data=dffinal,aes(x=pc1,y=pc2,color=class))+geom_point()
#We color in function of the class
#we can see 3 different classes which are well separated



# MTCARS PART

data(mtcars)
names(mtcars)
par(mfrow=c(2,2))
boxplot(mpg~carb,data=mtcars,col=c("green","yellow","orange"))
#we can see a linear relation between mpg and carb, indeed for a high MPG we have a low CARB
boxplot(cyl~carb,data=mtcars,col=c("green","yellow","orange"))
#Here for a high CARB we have a high CYL
boxplot(disp~carb,data=mtcars,col=c("green","yellow","orange"))
#also linear relation for a high carb we have a high DISP
boxplot(hp~carb,data=mtcars,col=c("green","yellow","orange"))
#also linear relation we have a high HP for a High CARB
ggplot(mtcars, aes(x=mpg, fill=mpg)) +
  geom_histogram(binwidth=.2, alpha=.5)
#The majority of cars are in middle value of MPG
pcaCars=princomp(mtcars[,],cor=T)
summary(pcaCars)
#the component 1 have 60% at the comp 4 we have a cmulative of 92%



biplot(pcaCars)
#we can see the individuals and the features which are very important for them
#for example for CAMARO the most important feature is HP
fviz_eig(pcaCars,addlabels = TRUE)
#we can see that the cp1 and cp2 and cp3 is much more important they gather 90% of information 
#whereas the 6 others gather 10%
fviz_pca_ind(pcaCars, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
)
fviz_pca_var(pcaCars, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

# for dimension 1 the most important features are MPG CYL and disp because they are close to the axis 1
#for dimension 2 the most important features are GSEC,GEAR becasue they are close to axis 2
fviz_pca_biplot(pcaCars, repel = TRUE)
cont3<-fviz_contrib(pcaCars, choice = "var", axes = 1, top = 10)
cont4<-fviz_contrib(pcaCars, choice = "var", axes = 2, top = 10)
ggarrange(cont3,cont4,ncol=2,nrow=1)
#the analysis for features and each dimension is correct for first dimension the most important is CYL disp and mpg as stated 
#And for dimension 2 the most important are SQEC, Gear

#PCA STEP BY STEP MT CARS
mtCarsScaled<-scale(mtcars,center=TRUE,scale=TRUE)
mtCarsScaled
covMatCars<-cov(mtCarsScaled)
covMatCars
#we can here see the correlation between all the features
