#Megan Stiles
#MES5AC
library(MPV)
library(lmtest)
library(fmsb)
#Problem 11.1

football<- read.csv('data-table-B1.csv')

football.lm<- lm(y~ x2 + x7 + x8, data= football)

#part A
#Calculate Residuals
residuals.football<-resid(football.lm)

#Calculate PRESS Statistic
press.football<- sum((residuals.football/(1 - lm.influence(football.lm)$hat))^2)
press.football #87.46123

#Calculate Sum of Squares
av<-anova(football.lm)
SST<-sum(av$`Sum Sq`) #326.96
#R^2 Prediction = 1- (PRESS/SST)
1-(press.football/SST) #0.7325

#The R^2 prediction is rather low which indicates this specific model may not be great at prediction.

#Part B

#Delete Half of the Observations

obs<- sample(1:nrow(football),as.integer(nrow(football) * 0.5))
football.half<- football[obs,]

#Refit model
football.half.lm<- lm(y~ x2+x7+x8, data = football.half)

football.half.lm$coefficients
#(Intercept)           x2           x7           x8 
#-1.568897091  0.003476951  0.204132149 -0.005200819

#Compare to Coefficients in full model:
football.lm$coefficients
#(Intercept)           x2           x7           x8 
#-1.808372059  0.003598070  0.193960210 -0.004815494 

#The coefficients have not change that much between the half and full data set.

#Calculate Prediction Error for removed data:

predicted.football.half<- predict(football.half.lm, newdata = football[-obs,], interval = 'none')
MSE<- mean((predicted.football.half-football$y[-obs])^2)
MSE #MSE is 2.556725

#Calculate PRESS Statistic
residuals.football<-resid(football.half.lm)
press.football<- sum((residuals.football/(1 - lm.influence(football.half.lm)$hat))^2)
press.football #59.06107

#Calculate Sum of Squares
av<-anova(football.half.lm)
SST<-sum(av$`Sum Sq`) 
#R^2 Prediction = 1- (PRESS/SST)
1-(press.football/SST) #0.7302268

#Part c

#Remove rows 8,7,17,26,9,11

obs<- c(8,7,17,26,9,11)
obs

#Fit New Linear Model
new.football<- football[-obs,]
new.football.lm<- lm(y~ x2+ x7 +x8, data= new.football)

#Predict on this linear model for removed teams
new.predicted<- predict(new.football.lm, newdata = football[obs,], interval = 'none')
MSE<- mean((new.predicted-football$y[obs])^2)
MSE #MSE is 2.078043


#Calculate PRESS Statistic
residuals.football<-resid(new.football.lm)
press.football<- sum((residuals.football/(1 - lm.influence(new.football.lm)$hat))^2)
press.football #81.43012

#Calculate Sum of Squares
av<-anova(new.football.lm)
SST<-sum(av$`Sum Sq`) 
#R^2 Prediction = 1- (PRESS/SST)
1-(press.football/SST) #0.7165407

#Problem 11.2

obs<- sample(1:nrow(football),as.integer(nrow(football) * 0.5))
estimation<- football[obs,]
prediction<- football[-obs,]
#Since the data was chosen at random, the two data sets are similiar

estimation.lm<- lm(y~., data= estimation)

#Perform Model Selection
s.null <- lm(y~1, data=estimation)
s.full <- lm(y~., data=estimation)

## Forward selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward") #Selects x2 + x8

## Backward selection
step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward") #Selects X3, x4, x5, x6, x7, x9

## Stepwise selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both") #Selects x8, x2

#Create model with x8, x2
estimation.lm<- lm(y~ x2+x8, data = estimation)

#Evalutate Model Performance
predicted<- predict(estimation.lm, newdata = prediction, interval= 'none')
MSE<- mean((predicted-football$y[-obs])^2)
MSE #MSE is 4.694794

#Calculate PRESS Statistic
residuals.football<-resid(estimation.lm)
press.football<- sum((residuals.football/(1 - lm.influence(estimation.lm)$hat))^2)
press.football #44.86437

#Calculate Sum of Squares
av<-anova(estimation.lm)
SST<-sum(av$`Sum Sq`) 
#R^2 Prediction = 1- (PRESS/SST)
1-(press.football/SST) #0.6915024

#This model is a poor model for prediction

#Problem 11.3

#Calculate Residuals
residuals.football<-resid(estimation.lm)

#Calculate PRESS Statistic
press.football<- sum((residuals.football/(1 - lm.influence(estimation.lm)$hat))^2)
press.football #44.86437

#Calculate Sum of Squares
av<-anova(estimation.lm)
SST<-sum(av$`Sum Sq`) 
#R^2 Prediction = 1- (PRESS/SST)
1-(press.football/SST) #0.6915024

#The model had an MSE of over 4, which was much  higher than the model used in 11.1, so this model is not the best model in terms
#of prediction

#Problem 11.11

#Calculate SE of Estimation SET
SE <- sqrt(diag(vcov(estimation.lm)))
SE
#(Intercept)          x2          x8 
#3.755019427 0.001144723 0.001309086 

#Calculate SE using all data:
SE<-sqrt(diag(vcov(football.lm)))
SE
# (Intercept)           x2           x7           x8 
#7.9008594002 0.0006949986 0.0882334488 0.0012769683 

#The Standard Error of the coefficients using all the data is smaller, which is expected because the model will have less variance with more data points

#Problem 11.12

#Develop Model with Prediction Data Set
#Perform Model Selection
s.null <- lm(y~1, data=prediction)
s.full <- lm(y~., data=prediction)

## Forward selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward") #Selects x7, x2, x9, x8

## Backward selection
step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward") #Selects x2,x3,x5,x7,x8,x9

## Stepwise selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both") #Selects x7,x2,x9,x8

#Develop Model with x7,x2,x9,x8

prediction.lm<- lm(y~x7+x2+x9+x8, data = prediction)

#Part A
prediction.lm$coefficients
#  (Intercept)            x7            x2            x9            x8 
#-11.402256848   0.406153858   0.003045072  -0.002800340  -0.002252884 

estimation.lm$coefficients
# (Intercept)           x2           x8 
#13.764781253  0.003807779 -0.007256225 
#The coefficients are significantly different than for the estimation set

#Part B

#Predict values for estimation set with prediciton set model
predicted<- predict(prediction.lm, newdata = estimation, interval= 'none')
MSE<- mean((predicted-football$y[obs])^2)
MSE #7.797269

#Other models had a significantly lower MSE, so this model is not an adequate model for prediction

#Problem 13.1

data("p13.1")
attach(p13.1)

#PArt A

#Fit Logistic Regression:

firing<-glm(y ~ x, data = p13.1, family="binomial")
firing$coefficients
#(Intercept)           x 
#6.0708839  -0.0177047 

#Part B

#Calculate Model Deviance:
deviance(firing) 

#LR Test
#Null model:
null<-glm(y~1, data= p13.1, family = 'binomial')
lrtest(null,firing) #P-value = 0.0001597, reject null hypothesis that null model is adequate.

#Part C
# Interpretation of B1: For every one knot increase in speed (x), the log of the odds ratio increases by B1( in this case -0.0177047)

#PArt D
p13.1$x2<- (p13.1$x)^2
firing.quadratic<- glm (y~ x + x2, data= p13.1, family ='binomial')

deviance(firing.quadratic) #20.36366

firing.quadratic$coefficients
# (Intercept)             x            x2 
#6.192726e+00 -1.846537e-02  1.100061e-06 

#LR TEST
lrtest(firing.quadratic,firing) #p-value = 0.9889, we fail to reject null hypothesis that the reduced model is appropriate


#Problem 13.2

data("p13.2")
attach(p13.2)

#Part A

#Fit Logistic Regression:
home<- glm(y~x, data= p13.2, family = 'binomial')

#Part B
deviance(home) #22.43492, 

#LR Test
#Null model:
null<- glm(y~1, data=p13.2, family = 'binomial')
lrtest(null,home) #P-value = 0.02406, reject null hypothesis that null model is adequate

#Part C

home$coefficients
#(Intercept)             x 
#-8.7395139021  0.0002009056 

#For every one dollar increase in income (x), the log of the odds ratio increases by B1( in this case 0.0002009056)

#PArt D
p13.2$x2<- p13.2$x^2
home.quad<- glm(y~x+ x2, data = p13.2, family = 'binomial')

lrtest(home.quad,home)
##p-value = 0.2924, we fail to reject null hypothesis that the reduced model is appropriate

#Problem 13.5
data("p13.5")
attach(p13.5)

#Part A

cars<- glm(y~x1 + x2, data= p13.5, family = 'binomial')

#PArt B
summary(cars)
deviance(cars) #21.08152

#LR Test
#Null model:
null<- glm(y~1, data = p13.5, family = 'binomial')
lrtest(null, cars) #p-value = 0.03607, reject null hypothesis that null model is adequate

#PArt c

cars$coefficients
#  (Intercept)            x1            x2 
#-7.047061e+00  7.381679e-05  9.878861e-01 

#For every one dollar increase in income (x1), the log of the odds ratio increases by B1( in this case 7.381679e-05).
#for every one unit increase in age of car (x2), the log of the odds ratio increases by B2( in this case 9.878861e-01)

#Part D

new<- data.frame(x1= 45,000, x2 = 5)
predict(cars, newdata = new, interval = 'none') #-2.104309
exp(-2.104409) #0.1219177
#P(x)/(1-P(x)) = 0.1219177
#P(x) = 0.108669

#Part E

cars.new<- glm(y~ x1+x2+ x1*x2, data= p13.5)
summary(cars.new) #This new model significantly decreases the residual deviance to 3.3039 indicating it could be 
#a better model

#Part F
SE <- sqrt(diag(vcov(cars)))
SE
# (Intercept)           x1           x2 
#4.674232e+00 6.371304e-05 5.273680e-01 

#PArt G

confint(cars, 'x1') #B1
#        2.5 %        97.5 % 
#-0.0000436154  0.0002184223 

confint(cars, 'x2') #B2
#    2.5 %    97.5 % 
#0.1544228 2.2872128 

#Problem 13.10 

pred_prob<- cars$fitted.values
dev_res<- residuals(cars, c='deviance')
qqnorm(dev_res)
plot(pred_prob, dev_res)

#The residuals are not randomally distributed making them unsatisfactory for this model

#Problem 13.25

challenger<- read.csv('data-prob-13-25.csv')

#Part A

challenger.glm<- glm(At.Least.One.O.ring.Failure ~ Temperature.at.Launch, data = challenger, family = 'binomial')

pred_prob<- challenger.glm$fitted.values
plot(challenger$Temperature.at.Launch, pred_prob)

#Part B
sum(challenger$At.Least.One.O.ring.Failure) #7 failures, 17 success
#Odds Ratio = (7/24)/(17/24) = 0.4118

#Part C
new_data<- data.frame(Temperature.at.Launch = 50)

predict(challenger.glm, newdata = new_data) #2.309324
exp(2.309324) #10.06762
10.06762/11.06762 # 0.9096463 percent chance of failure at 50 degrees

#Part D
new_data<- data.frame(Temperature.at.Launch = 75)
predict(challenger.glm, newdata = new_data) #-1.973688
exp(-1.973688) #0.1389435
0.1389435/1.1389435 #0.1219933 percent chance of failure at 75 degrees

#PArt E
new_data<- data.frame(Temperature.at.Launch = 31)
predict(challenger.glm, newdata = new_data) #5.564414
exp(5.564414) #260.9722
260.9722/261.9722 #0.9961828 percent chance of failure at 31 degrees.
#I would not recommend launch, even though this involves extrapolation because the risk of being wrong is too high

#Part F
residuals(challenger.glm, c= 'deviance')
#These residuals represent the difference in the predicited log-odds with the observed logg-odds

#Part G
challenger$x2<- challenger$Temperature.at.Launch^2
challenger.lm2<- glm(At.Least.One.O.ring.Failure ~ Temperature.at.Launch + x2, data = challenger, family = 'binomial')
lrtest(challenger.lm2,challenger.glm)
#P-value = 0.5064 and we fail to reject the null hypothesis that the model with the qreduced model is appropriate