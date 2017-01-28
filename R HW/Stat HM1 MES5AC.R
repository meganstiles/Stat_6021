#Megan Stiles MES5AC
library(MPV)

#Question 2.1
data(table.b1)
attach(table.b1)

#Part A
nfl.lm<- lm(y~x8, data= table.b1)

nfl.lm #B1 =-0.007025, B0 = 21.788251

#Part B

anova(nfl.lm) #p-value = 7.381e-06, because the p-value is < 0.05, we reject Ho because there is significant regression

#Analysis of Variance Table

#Response: y
#          Df Sum Sq Mean Sq F value    Pr(>F)    
#x8         1 178.09 178.092  31.103 7.381e-06 ***
#Residuals 26 148.87   5.726 

#Part C

confint(nfl.lm, level = 0.95) #(-0.009614347, -0.004435854)

#Part D

summary(nfl.lm)
#R^2 = 0.5447 = 54.47% of the total variability in y is explained by this model

#Part E

new_table<- data.frame(x8 = 2000)
predict(nfl.lm, new_table, interval = "confidence") #(6.765753, 8.710348)


#Problem 2.2
table2<- data.frame(x8=1800)
predict(nfl.lm, table2, interval = "none") #Point Estimate = 9.14307 games

#Prediction Interval
predict(nfl.lm,table2, interval = "prediction", level= 0.90) #(13.34975, 4.936392)


#Problem 2.4

data("table.b3")
attach(table.b3)

#Part A
lm.mpg<- lm(y~x1, data=table.b3)
lm.mpg # B1 = -0.04736 B0 = 33.72268

#Part b
anova(lm.mpg) #P-value = 3.743e-11, because the p value is < 0.05 we reject H0 and the regression is significant.

#Analysis of Variance Table

#Response: y
#          Df Sum Sq Mean Sq F value    Pr(>F)    
#x1         1 955.72  955.72  101.74 3.743e-11 ***
#Residuals 30 281.82    9.39    

#Part C
summary(lm.mpg)
#R^2 = 0.7723 = 77.23% percent of total mpg variability accounted for by the linear relationship with engine displacement

#Part D
new_displacement<- data.frame(x1= 275)
predict(lm.mpg, new_displacement, interval = "confidence") #(19.58807, 21.80952)

#Part E
predict(lm.mpg, new_displacement, interval = "none") #20.69879
predict(lm.mpg, new_displacement, interval = "prediction") #(14.34147, 27.05611)

#Part F:
#The prediction interval is wider than the confidence interval. This is because the confidence interval is trying to 
#estimate the mean of y given a value of x, whereas the prediction interval is trying to predict a future value of y, given a
#value of x. There is more variance in trying to predict a future value than just the mean so the prediction interval is wider.


#Problem 2.5

#Part a
lm.mpg2<- lm(y~x10, data= table.b3)
lm.mpg2 #B1 = -0.005752, B0 = 40.852431
#The linear models have a similar R^2 which could indicate colinearity between vehicle weight and engine displacement.
#The engine displacement model also has a smaller SSRes, as well as a slightly R^2 so this model is a slighly better fit.

#Part B
anova(lm.mpg2)

#Analysis of Variance Table

#Response: y
#          Df Sum Sq Mean Sq F value    Pr(>F)    
#x10        1 921.53  921.53  87.482 2.121e-10 ***
#Residuals 30 316.02   10.53 


#Part c
summary(lm.mpg2)
#R^2 = 0.7446 = 74.46 % of variability is explained by linear relationship between vehicle weight and MPG

#Problem 2.12

data(p2.12)
attach(p2.12)

#Part a

usage.lm<- lm(usage~temp, data = p2.12)
usage.lm #B1 = 9.208, B0 = -6.332

#Part b
anova(usage.lm) #P-value = 2.2e-16 which is <0.05, so we reject Ho and the regression model is significant.
#Analysis of Variance Table

#Response: usage
#          Df Sum Sq Mean Sq F value    Pr(>F)    
#temp       1 280590  280590   74123 < 2.2e-16 ***
#Residuals 10     38       4

#Part c
usage.lm
#slope = 9.208, meaning that for everyone 1 degree increase, usage increases by 9.2 units (1=1000 units), so approximately 9,200 units

#Part D
new_temp<- data.frame(temp = 58)
predict(usage.lm, new_temp, interval= "prediction", level = 0.99) #(521.2237, 534.2944)

# Problem 2.29

#Since the Standard Error of the slope = Squareroot(MSRes/Sxx), we want to either maximixe Sxx or minimize MSRes in order
#to keep the standard error low. Since Sxx = the sum of (xi - x-bar)^2, we would want to take sampples of x close to -1 and 1
#as this would increase Sxx and thus lower the standard error of the slope. Depending on the type of data, this may be impracticle
#since it may not be possible to sample this specific subset.
