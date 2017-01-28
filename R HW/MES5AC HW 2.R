#Megan Stiles
#MES5AC

library(MPV)
library(readxl)

#Problem 2.20
fuel_consumption<- read.csv("Data-Table-B18.csv")
plot(y~x_5, data = fuel_consumption)
fuel.lm<- lm(y~x_5, data=fuel_consumption)

anova(fuel.lm) #p-value = 0.0159, thus we can reject Ho that Beta1 = 0 and the model is significant.
summary(fuel.lm)
#Analysis of Variance Table

#Response: y
#          Df Sum Sq Mean Sq F value  Pr(>F)  
#x_5        1 461.65  461.65  7.5143 0.01592 *
#Residuals 14 860.10   61.44                  
---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Call:
  #lm(formula = y ~ x_5, data = fuel_consumption)

#Residuals:
  #Min       1Q   Median       3Q      Max 
#-12.5651  -5.7681   0.1608   4.9303  11.7537 

#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 410.72319   18.92479  21.703 3.54e-12 ***
  #x_5          -0.26376    0.09622  -2.741   0.0159 *  
  ---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7.838 on 14 degrees of freedom
#Multiple R-squared:  0.3493,	Adjusted R-squared:  0.3028 
#F-statistic: 7.514 on 1 and 14 DF,  p-value: 0.01592
  
  
#Problem 2.21

wine<-read.csv("data-table-B19.csv")

plot(y~x_3, data = wine)

wine.lm<- lm(y~x_3, data = wine)

summary(wine.lm) #p-value = 0.034, thus we reject Ho that Beta1 = 0 and the model is significant.
#Residuals:
  #Min      1Q  Median      3Q     Max 
#-2.9282 -1.3457  0.1070  0.8373  3.4783 

#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 16.564030   0.620906  26.677   <2e-16 ***
  #x_3         -0.012762   0.005744  -2.222    0.034 *  
  ---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.668 on 30 degrees of freedom
#Multiple R-squared:  0.1413,	Adjusted R-squared:  0.1127 
#F-statistic: 4.936 on 1 and 30 DF,  p-value: 0.03399
  
anova(wine.lm)
#Analysis of Variance Table

#Response: y
#Df Sum Sq Mean Sq F value  Pr(>F)  
#x_3        1 13.734 13.7337  4.9363 0.03399 *
  #Residuals 30 83.466  2.7822                  
---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  

#Problem 2.22

oxidation<- read.csv("data-table-B20.csv")

plot(y~x_5, data = oxidation)

oxidation.lm<- lm(y~x_5, data = oxidation)

summary(oxidation.lm) # p-value = 0.6483, we do not reject Ho that Beta1 = 0 and the model is not significant.

#Residuals:
  #Min     1Q Median     3Q    Max 
#-30.29 -24.15 -16.76  29.42  63.20 

#Coefficients:
  #              Estimate Std. Error t value Pr(>|t|)
#(Intercept)    21.25      20.99   1.013    0.326
#x_5             7.80      16.78   0.465    0.648

#Residual standard error: 35.76 on 16 degrees of freedom
#Multiple R-squared:  0.01333,	Adjusted R-squared:  -0.04834 
#F-statistic: 0.2161 on 1 and 16 DF,  p-value: 0.6483

anova(oxidation.lm)
#Analysis of Variance Table

#Response: y
#Df  Sum Sq Mean Sq F value Pr(>F)
#x_5        1   276.4  276.35  0.2161 0.6483
#Residuals 16 20458.1 1278.63 


#Problem 2.30

data("p2.12")
attach(p2.12)
#Part A
cor(temp,usage) #correlation = 0.9999326

#Part B

cor.test(temp,usage) # source: http://www.personality-project.org/r/html/corr.test.html

#Pearson's product-moment correlation

#data:  temp and usage
#t = 272.25, df = 10, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.9997509 0.9999817
#sample estimates:
 #     cor 
#0.9999326 


#Part C
my.lm<- lm(temp~usage, data = p2.12)
summary(my.lm)
#r^2 = 0.9999
r<-sqrt(0.9999) # = r = 0.99995

z<- (atanh(r) - atanh(0.5))*((9)^(1/2))
#Since Z is approximately 14, which is greater than the Z value for a 95% confidence level, we reject Ho that the correlation = 0.5

#Part D

cor.test(temp,usage, conf.level = 0.99)
#99 percent confidence interval = 0.9996244, 0.9999879