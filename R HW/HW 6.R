#Megan Stiles
#MES5AC
library(car)
library(glmnet)
library(leaps)
#Problem 9.7

#Part A
gasoline<- read.csv('data-table-B3.csv')
cor(gasoline[2:12])

#Based on the correlation matrix there appears to be a lot of correlation, many correlation values are close to the abs
#value of 1
#Part B
gasoline.lm<- lm(y~.,data = gasoline)

kappa(gasoline.lm) #114075, severe multicollinearity is indicated

vif(gasoline.lm) #multiple variables have VIFs over 5

#Problem 9.13

fuel<- read.csv('data-table-B18.csv')
cor(fuel[2:9])
#A few variables appear to be correlated

fuel.lm<- lm(y~.,data= fuel)
kappa(fuel.lm) #416182 severe multicollinearity is indicated

vif(fuel.lm) #variables x3, x4, x5, x6, x7, x8 all have VIF values over 5

#Problem 9.14

wine<- read.csv('data-table-B19.csv')
cor(wine[2:11]) #A few variables appear to be correlated

wine.lm<- lm(y~., data=wine)
kappa(wine.lm) # 3.7 e19 severe multicollinearity is indicated

vif(wine.lm)


#Problem 9.15

methanol<- read.csv('data-table-B20.csv')
cor(methanol[2:6]) #A few variables appear to be correlated

methanol.lm<- lm(y~., data = methanol)
kappa(methanol.lm) #71436 severe multicollinearity is indicated

vif(methanol.lm) #variables x2, x3 have VIF values greater than 5


#Problem 9.19

gasoline.m<- as.matrix(na.omit(gasoline))

#Estimate Parameters:
gasoline.ridge <- glmnet(gasoline.m[,2:12], gasoline.m[,1], alpha=0)
plot(gasoline.ridge,xvar="lambda",label=TRUE)

#Use log(lambda) = 5, lambda = exp(5)= 148.4132, Based on graph, this is the lambda where Betas start to converge

gasoline.ridge<- glmnet(gasoline.m[,2:12], gasoline.m[,1], alpha = 0, lambda =148.4132)
gasoline.ridge$beta

#Coefficients:
#x1  -0.0014912811
#x2  -0.0035206668
#x3  -0.0020192062
#x4   0.3071820730
#x5   0.2358954705
#x6  -0.0855754694
#x7   0.2088095295
#x8  -0.0069756129
#x9  -0.0268325814
#x10 -0.0001801139
#x11 -0.3121782901

#Part B
#Predict values to calculate RSS:
x<- as.matrix(na.omit(gasoline))
new.gasoline<- na.omit(gasoline) #remove missing values

predicted.y<- predict(gasoline.ridge, s= 148.4132, newx = x[,2:12])
RSS.ridge<- sum((predicted.y-new.gasoline$y)^2) #RSS using Ridge = 772.40

#Calculate RSS using OLS:
gasoline.lm<- lm(y~., data= new.gasoline)
RSS.ols<- sum((gasoline.lm$residuals)^2) #RSS = 187.200

#RSS increases by almost 600 due to Ridge REgression

#PArt C
#Calculate Original R^2
summary(gasoline.lm) # 0.8355

#Calculate R^2 with Ridge Regression:
mean.y<- mean(new.gasoline$y)
TSS<- sum((mean.y- new.gasoline$y)^2)
#R^2 = 1- RSS/TSS
1-(RSS.ridge/TSS) #0.3219, R^2 has significantly been reduced by the use of Ridge regression

#Problem 9.20

#k = p(sigma^2)/crossprod of Betas
#p = k+1 = 12
#sigma^2 = SSres/n-p = 187.200/(32-11) = 8.914
#crossprod of Betas = crossprod(gasoline.lm$coef) #348.0485
#k = (12*8.914)/348.0485 = 0.307

#Perform ridge regression with new lambda value
gasoline.ridge.2<- glmnet(gasoline.m[,2:12], gasoline.m[,1], alpha = 0, lambda =0.307)
gasoline.ridge.2$beta

#x1  -0.019883237
#x2  -0.008239652
#x3  -0.003127731
#x4   2.265631244
#x5   2.308681002
#x6  -0.071149241
#x7  -0.859392757
#x8   0.065423328
#x9  -0.253483608
#x10 -0.002078027
#x11  0.092076715

#These model parameters are slightly different than the model with the higher lambda value, the coefficients are higher

#Problem 10.1

football<- read.csv('data-table-B1.csv')

#Part A

football.null <- lm(y~1, data=football)
football.full <- lm(y~., data=football)

## Forward selection
step(football.null, scope=list(lower=football.null, upper=football.full), direction="forward")
#selects x8, x2, x7, x9

#Part B
## Backward selection
step(football.full, scope=list(lower=football.null, upper=football.full), direction="backward")
#selects x2, x7, x8, x9

#Part C
## Stepwise selection
step(football.null, scope=list(lower=football.null, upper=football.full), direction="both")
#selects x8, x2, x7, x9

#Part D
#All three methods selected the same model

#Problem 10.2

bestmod <- regsubsets(y~x1+ x2+ x4+ x7 +x8 +x9, data=football, nbest=10)
summary(bestmod)

summary(bestmod)$rss
summary(bestmod)$adjr2
summary(bestmod)$cp
summary(bestmod)$bic

#I recommend using variables x2, x7 x8, x9. This model produces an adj R^2 value of 0.7666 a Cp value of 4.04, an RSS of 65.0,
# and a BIC of -28.570

#Problem 10.14

wine<- read.csv('data-table-B11.csv')
wine$Region<- as.factor(wine$Region)
#PArt a
bestwine<- regsubsets(Quality~., data = wine, nbest =10)
summary(bestwine)
summary(bestwine)$cp
summary(bestwine)
#I would choose the model with a Cp value of 2.2436 which is the model with Flavor,Oakiness and Region

#PArt B
#Second best model in terms of Cp 2.4736 uses (Flavor, Region)

#First Model
one.lm<- lm(Quality~ Region+ Flavor +Oakiness, data = wine)
predicted.one<- predict(one.lm, interval = 'none')
residuals.one<- resid(one.lm)

plot(one.lm)
plot(predicted.one, residuals.one)

#Second Model
two.lm<- lm(Quality~  Flavor + Region, data = wine)
predicted.two<- predict(two.lm, interval = 'none')
residuals.two<- resid(two.lm)
plot(two.lm)
plot(predicted.two, residuals.two)
summary(two.lm)
summary(one.lm)

#There isn't really a practical basis to select one model over the other, the residual plots are fairly similiar

#Part C

#PRESS

resid.one<- resid(one.lm)
press.one<- sum((resid(one.lm)/(1 - lm.influence(one.lm)$hat))^2)
press.one #33.08173

resid.two<- resid(two.lm)
press.two<- sum((resid(two.lm)/(1 - lm.influence(two.lm)$hat))^2)
press.two #33.99346

#There is no noticeable difference in the PRESS Statistics for these two models

#Problem 10.15

wine.null <- lm(Quality~1, data=wine)
wine.full <- lm(Quality~., data=wine)

## Stepwise selection
step(wine.null, scope=list(lower=wine.null, upper=wine.full), direction="both")
#Selects Flavor, Oakiness, Region
#The model from problem 10.14 a was with the same model

#Problem 10.16

bestwine2<- regsubsets(Quality~.-Region, data = wine, nbest =10)
summary(bestwine2)
summary(bestwine2)$cp
#Best model with a Cp of 3.927790 is with Aroma, Flavor, and Oakiness

#First Model
one.lm<- lm(Quality~ Aroma + Flavor +Oakiness, data = wine)
predicted.one<- predict(one.lm, interval = 'none')
residuals.one<- resid(one.lm)

plot(one.lm)
plot(predicted.one, residuals.one)
anova(one.lm)
summary(one.lm)

#Second Model
two.lm<- lm(Quality~  Flavor + Region + Oakiness, data = wine)
predicted.two<- predict(two.lm, interval = 'none')
residuals.two<- resid(two.lm)
plot(two.lm)
plot(predicted.two, residuals.two)
anova(two.lm)
summary(two.lm)
#The model with Region does have a slightly higher adj R^2 value

#Part B
#Model from Problem 10.14:
one.lm<- lm(Quality~ Oakiness + Flavor + Region, data = wine)
predicted.one<- predict(one.lm, newdata = wine, interval = 'confidence')
predicted.one

#Model from Problem 10.16:
two.lm<- lm(Quality~ Aroma + Flavor +Oakiness, data = wine)
predicted.two<- predict(two.lm, newdata = wine, interval = 'confidence')
predicted.two

three.lm<-lm(Quality~  Flavor + Region + Oakiness, data = wine)
predicted.three<- predict(three.lm, newdata = wine, interval = 'confidence')
predicted.three

#Calculate range of confidence intervals
mean(predicted.one[,3] - predicted.one[,2]) # 1.275132
mean(predicted.two[,3]- predicted.two[,2]) #1.486928
mean(predicted.three[,3] - predicted.three[,2]) #1.275132

#The model with Oakiness, Flavor, and Region has a smaller range for the confidence interval so I would go with prefer this model.
