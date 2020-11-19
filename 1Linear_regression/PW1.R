df1 = 1
df2 = 5
res1=qf(0.95,df1,df2)
res2=qf(0.9,df1,df2)
res3=qf(0.99,df1,df2)
p=c(res1,res2,res3)

poisson= rpois(100,lambda=5)
poisson
plot(1:100,poisson,type='l')

x=seq(-4,4,l=100)
y=dt(x=x,df=1)
plot(x,y)

for(i in c(5,10,50,100))
{
  lines(x,dt(x=x,df=i))
}
plot(x,y,type='l')


load('EU.RData')
EU['Population2010']
names(EU)
myModel<-lm(formula=CamCom2011~Population2010,data=EU)
myModel['residuals']
myModel['coefficients']
summaryMyModel<-summary
summaryMyModel



