#Megan Stiles
#MES5AC
library(readr)

#Problem 3.1
football<- read.csv("data-table-B1.csv")

#Part A

lm.football<- lm(y~x2 + x7+ x8,data = football)

#Part B
anova(lm.football)
summary(lm.football)
#F-Statistic = 29.44, p-value = 3.27e-08, reject Ho and at least one regressor must be significant

#Response: y
#          Df  Sum Sq Mean Sq F value    Pr(>F)    
# x2         1  76.193  76.193  26.172 3.100e-05 ***
# x7         1 139.501 139.501  47.918 3.698e-07 ***
# x8         1  41.400  41.400  14.221 0.0009378 ***
# Residuals 24  69.870   2.911                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Part C
summary(lm.football)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.808372   7.900859  -0.229 0.820899    
#x2           0.003598   0.000695   5.177 2.66e-05 ***
#x7           0.193960   0.088233   2.198 0.037815 *  
# x8          -0.004816   0.001277  -3.771 0.000938 ***

#t-statistics:
#x2 5.177
#x7 2.198
#x8 -3.771
#Based on the p-values, all three regressor variables are significant at the 0.05 threshold

#Part D

summary(lm.football) # R^2 = 0.7863, R^2 adj = 0.7596

#Part E
lm.football.test<- lm(y~x2 + x8, data= football)
anova(lm.football, lm.football.test)
#Based on this test, the p-value is 0.037, which is less than 0.05 and thus we can reject the null hypothesis that the coefficient
#of x7 = 0 and the variable is significant to regression.

#Problem 3.3

#Part A

confint(lm.football, "x7") # (0.01185532, 0.3760651)

#Part B
example<-data.frame (x2 = 2300, x7 = 56.0, x8 = 2100)
predict(lm.football,example, interval = 'confidence') #6.436203, 7.996645

#Problem 3.4

#Part A
lm2.football<- lm(y~x7 + x8, data = football)
summary(lm2.football)
#F- Statistic = 15.13, p-value = 4.935e-05, reject Ho and at least one regressor is significant

#Part B

#R^2 = 0.5477
# R^2 Adj = 0.5115
#These values are lower than the values when there is an additional regressor

#Part c
confint(lm2.football, 'x7') #-0.1971643, 0.293906

example.2<- data.frame(x7 = 56.0, x8 = 2100)
predict(lm2.football, example.2, interval = 'confidence') # 5.828643, 8.023842
#This interval is much larger than when there is an additional regressor variable

#Part D
#Omitting an important regressor from a model will make your model less accurate less confident, increase the size of your confidence intervals,
# and reducing your value of R^2.

#Problem 3.5

gasoline<- read.csv('data-table-B3.csv')

#Part a

lm.gasoline<- lm(y~ x1 + x6, data= gasoline)

#Part B
anova(lm.gasoline)
summary(lm.gasoline)
#F-statistic = 53.67, p-value = 1.79e-10, reject Ho, at least one regressor variable is significant
#Analysis of Variance Table

#Response: y
#         Df Sum Sq Mean Sq F value    Pr(>F)    
#x1         1 955.72  955.72 105.290 3.666e-11 ***
#x6         1  18.59   18.59   2.048    0.1631    
#Residuals 29 263.23    9.08                      
---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Part C
summary(lm.gasoline)
# R^2 = 0.7873
# R^2 adj = 0.7726
#This is very similar to the R^2 value found in problem 2.4

#Part d

confint(lm.gasoline, 'x1') # -0.06569892, -0.0405641

# Part e
summary(lm.gasoline)

#T-statistics:
#x1 -8.660
#x6 1.431
#We can conclude that x1 is significant while x6 is not.

#Part F
example.3.5<- data.frame(x1 = 275, x6 = 2)
predict(lm.gasoline, example.3.5, interval = 'confidence') # 18.87221, 21.50257

#Part G
predict(lm.gasoline, example.3.5, interval = 'prediction') # 13.8867, 26.48808

#Problem 3.6

#From problem 2.4 the CI when x1 = 275 was (19.58807, 21.80952). The predicition interval was #(14.34147, 27.05611)
# The CI is larger with 2 regressors, while the PI is slightly smaller with two regressors. This seems to confirm
#that x6 is not significant to the model.

#Problem 3.8

chemistry<- read.csv('data-table-B5.csv')

#Part A

lm.chemistry<- lm(y~ x6 + x7, data = chemistry)

#Part B
summary(lm.chemistry)
#F- statistic = 27.95, p-value = 5.391e-07, reject Ho, at least one regressor variable is significant 

#Multiple R-squared:  0.6996,	Adjusted R-squared:  0.6746 

  
#Part C
 #T-values:
#x6 = 6.742
#x7 = 2.247
#Both regressors contribute to the model

#Part D
confint(lm.chemistry, 'x6') #0.1285196, 0.02419204

confint(lm.chemistry, 'x7') # 0.1782076, 4.193298

#Part E

lm.chemistry.2<- lm(y~x6, data= chemistry)
summary(lm.chemistry.2) # p- value = 6.24e-07, R^2 = 0.6365, R^2 adj = 0.6219
#This model still shows that x6 is significant however the R^2 is reduced since we are removing
# a significant regressor variable.

#Part F

confint(lm.chemistry.2, 'x6') # 0.01335688, 0.02543261
#This CI is very close to the CI calculated with x7, which means that x7 only affects the response variable slightly.

#Part G
anova(lm.chemistry) #MSres = 98.5
anova(lm.chemistry.2) #MSres = 114.4

#When you remove x7 from the model, MSres slightly increases, which indicates that x7 does slightly contribute to the model.

#Problem 3.11

peanuts<- read.csv('data-table-B7.csv')

#Part a

lm.peanuts<- lm(y~ x1 + x2 + x3 + x4 + x5, data = peanuts)

#Part B
summary(lm.peanuts) 
#F-statistic = 29.86, p-value = 1.055e-05, reject Ho, at least one regressor variable is significant

#Part c
#P-Values
#x1 0.0925
#x2 0.000625
#x3 0.7629
#x4 1.000
#x5 6.4e-07
#Based on the p-values, we can conclude that x5 and x2 appear to contribute significantly to the model

#Part d
# with all regressors: Multiple R-squared:  0.9372,	Adjusted R-squared:  0.9058 
lm.peanuts.2<- lm(y~ x2 +x5, data = peanuts)
summary(lm.peanuts.2) #Multiple R-squared:  0.9149,	Adjusted R-squared:  0.9018 
#Because these R^2 values do not significantly decrease as regressor variables are dropped, 
#this confirms that x2 and x5 are the variables that contribute significantly to the model

#Part E

confint(lm.peanuts, 'x2') #0.1537804, 0.4105053
confint(lm.peanuts.2, 'x2') #0.1550559, 0.4092298
#The differences in CI are very minor, confirming that the other regressor variables do not contribute significantly.

#problem 3.16

expectancy<- read.csv('data-table-B16.csv')
expectancy$Country<-factor(expectancy$Country)
is.factor(expectancy$Country)

#Part A
lm.life.exp<- lm(LifeExp ~ People.per.TV + People.per.Dr, data = expectancy)
lm.exp.male<- lm(LifeExpMale ~ People.per.TV + People.per.Dr, data = expectancy)
lm.exp.female<- lm(LifeExpFemale ~ People.per.TV + People.per.Dr, data = expectancy)

#Part B
summary(lm.life.exp)
#F-statistic = 13.46, p-value = 4.623e-05, reject Ho, at least one regressor variable is significant

summary(lm.exp.male)
#F-statistic = 12.53, p-value = 7.863e-05, reject Ho, at least one regressor variable is significant

summary(lm.exp.female)
#F-statistic = 14.07, p-value = 3.279e-05, reject Ho, at least one regressor variable is significant


#Part C
summary(lm.life.exp)
#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   70.2362645  1.0925483  64.287   <2e-16 ***
# People.per.TV -0.0226074  0.0096005  -2.355   0.0243 *  
#People.per.Dr -0.0004470  0.0002016  -2.217   0.0332 * 

summary(lm.exp.male)

#Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   73.0919445  1.2505753  58.447   <2e-16 ***
# People.per.TV -0.0256825  0.0109891  -2.337   0.0253 *  
# People.per.Dr -0.0004785  0.0002308  -2.074   0.0455 *

summary(lm.exp.female)

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)   67.4297595  0.9569733  70.461   <2e-16 ***
#People.per.TV -0.0198637  0.0084091  -2.362   0.0239 *  
#People.per.Dr -0.0004086  0.0001766  -2.314   0.0267 * 

#We can conclude that People per Tv and People per Dr contribute to all three models

#Part D

#Total Multiple R-squared:  0.4347,	Adjusted R-squared:  0.4024 
#Male Multiple R-squared:  0.4173,	Adjusted R-squared:  0.384 
#Female Multiple R-squared:  0.4457,	Adjusted R-squared:  0.414 

#Part E

confint(lm.life.exp, 'People.per.Dr') # -0.0008563196, -3.77668e-05
confint(lm.exp.male, 'People.per.Dr') # -0.0009470177, -1.008023e-05
confint(lm.exp.female, 'People.per.Dr') # -0.0007670492, -5.007977e-05
