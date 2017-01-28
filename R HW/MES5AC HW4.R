#Megan Stiles

#MES5AC

library(car)
library(MPV)
#Problem 4.2

football<- read.csv("data-table-B1.csv") 

lm.football<- lm(y~x2 + x7+ x8,data = football)

#Part A

qqnorm(rstudent(lm.football))
qqline(rstudent(lm.football))
#based on the plot, the distribution appears to be slightly light-tailed

#Part B

predicted.y<- predict(lm.football, interval = 'none')
residuals.y<- resid(lm.football)

plot(predicted.y, residuals.y)

#This plot indicates a satisfactory plot because we see no pattern in the plot and the points are centered around zero. The
#residuals appear to be uncorrelated and have constant variance.

#Part C

plot(football$x2,residuals.y)

#This plot also shows no distinct pattern and with the points centered around zero.

plot(football$x7, residuals.y)

#This plot appears to show a slight funnel, indicating that the variance is not constant.

plot(football$x8, residuals.y)

#This plot shows no distinct pattern and with the points centered around zero.

#These plots imply that x2 and x8 are correctly specified and that more consideration should be given to x7.

#Part D

avPlots(lm.football)

#Partial Residual plots can help you determine if a regressor variable needs a curvature effect. The partial residual
#plot of x7 also shows a slight funnel as it did in part C. The plots for the other variable appear consistent with a
#linear relationship. Plots of the residuals against the regressor variables can exhibit similar patterns to residual plots against 
# the predicted values. These plots can tell you if your residuals are desirable.

#Part E

# Find the studentized residuals
studentized<- rstandard(lm.football)
print(studentized)
#Studentized residuals can help in identifying potential outliers because it gives a more accurate estimate of the variance
#since high influence outliers tend to have a small variance even though ther are outliers because they pull the regression line
# towards the outlier point. None of the data appear to be outliers based on these residuals

# Find the R-student residuals
r_student<- rstudent(lm.football)
print(r_student)
#R-student residuals can also identify potential outliers because it uses a more sensitive estimation for the variance:
#the variance of the model with the ith data point removed. If S(i) differs greatly from MSE, then this point could
#be an outlier None of the data appear to be outliers based on these residuals

#Problem 4.4

#Part A

gasoline<- read.csv('data-table-B3.csv')

lm.gasoline<- lm(y~ x1 + x6, data= gasoline)

qqnorm(rstudent(lm.gasoline))
qqline(rstudent(lm.gasoline))

# The plot appears to follow a straight line and therefore the normality assumption should hold true.

#Part B
predicted.gas<- predict(lm.gasoline, interval = 'none')
residuals.gas<- resid(lm.gasoline)

plot(predicted.gas, residuals.gas)

#This plot appears to show a slight funneling of the residuals, indicating the variance may not be constant

#Part C

avPlots(lm.gasoline)

#The plot for x1 indicates a linear relationship however the plot for x6 does not. This makes sense from the p-values that
#were found in problem 3.5

#Part D

# Find the studentized residuals
studentized<- rstandard(lm.gasoline)
print(studentized)
#Studentized residuals can help in identifying potential outliers because it gives a more accurate estimate of the variance
#since high influence outliers tend to have a small variance even though ther are outliers because they pull the regression line
# towards the outlier point. None of the data appear to be outliers based on these residuals

# Find the R-student residuals
r_student<- rstudent(lm.gasoline)
print(r_student)

#R-student residuals can also identify potential outliers because it uses a more sensitive estimation for the variance:
#the variance of the model with the ith data point removed. If S(i) differs greatly from MSE, then this point could
#be an outlier. None of the data appear to be outliers based on these residuals

#problem 4.8

data(p2.12)
attach(p2.12)
usage.lm<- lm(usage~temp, data = p2.12)
#Part A
qqnorm(rstudent(usage.lm))
qqline(rstudent(usage.lm))

#The plot seems to have a slight curve 

#Part B

predicted.usage<- predict(usage.lm, interval = 'none')
residuals.usage<- resid(usage.lm)

plot(predicted.usage, residuals.usage)

#this plot shows a positive curved line between the residuals and the predicted values indicating possibly a non-linear function

#Part C

x<- seq(1,12,1)
plot(x, residuals.usage)

#this plot indicates a postive auto-correlation indicating that errors are related to the previous observation and thus that
#they are not random

#Problem 4.13

#Model #1
chemistry<- read.csv('data-table-B5.csv')
lm.chemistry<- lm(y~ x6 + x7, data = chemistry)

#Normal Probability Plot
qqnorm(rstudent(lm.chemistry)) #indicates distribution of residuals possibly have lighter tails
qqline(rstudent(lm.chemistry))

#Plot predicted values vs. residuals

predicted.chem<- predict(lm.chemistry, interval = 'none')
residuals.chem<- resid(lm.chemistry)

plot(predicted.chem, residuals.chem) #Indicates desirable distribution of residuals

#Calculate PRESS Statistic

PRESS(lm.chemistry) #3388.604

#Model #2
chemistry<- read.csv('data-table-B5.csv')
lm.chemistry.2<- lm(y~x6, data= chemistry)

#Normal Probability Plot
qqnorm(rstudent(lm.chemistry.2)) #indicates distribution of residuals possibly have lighter tails
qqline(rstudent(lm.chemistry.2))

#Plot predicted values vs. residuals

predicted.chem.2<- predict(lm.chemistry.2, interval = 'none')
residuals.chem.2<- resid(lm.chemistry.2)

plot(predicted.chem.2, residuals.chem.2) #Indicates Residuals have a possible funnel shape, which is undesirable

#Calculate PRESS Statistic

PRESS(lm.chemistry.2) #3692.881

#Analysis of both models indicate a slightly narrow distribution of the residuals. The first model has a slightly lower PRESS
# Statistic, and lower PRESS Statistics are more desirable.

#Problem 4.25

expectancy<- read.csv('data-table-B16.csv')
lm.life.exp<- lm(LifeExp ~ People.per.TV + People.per.Dr, data = expectancy)
lm.exp.male<- lm(LifeExpMale ~ People.per.TV + People.per.Dr, data = expectancy)
lm.exp.female<- lm(LifeExpFemale ~ People.per.TV + People.per.Dr, data = expectancy)

#Part A
qqnorm(rstudent(lm.life.exp)) #indicates distribution of residuals possibly have heavier tails
qqline(rstudent(lm.life.exp))

qqnorm(rstudent(lm.exp.male)) #indicates distribution of residuals possibly have heavier tails
qqline(rstudent(lm.exp.male))

qqnorm(rstudent(lm.exp.female)) #indicates distribution of residuals possibly have heavier tails
qqline(rstudent(lm.exp.female))

#These plots seem to indicate that all the models residuals follow a heavy-tailed distribution instead of a normaldistribution.

#Part B

predicted.life.exp<- predict(lm.life.exp, interval = 'none')
residuals.life.exp<- resid(lm.life.exp)

plot(predicted.life.exp, residuals.life.exp) #This plot shows residuals centered around zero with a possible few outliers

predicted.life.male<- predict(lm.exp.male, interval = 'none')
residuals.life.male<- resid(lm.exp.male)

plot(predicted.life.male, residuals.life.exp) #This plot shows residuals centered around zero with a possible few outliers

predicted.life.female<- predict(lm.exp.female, interval = 'none')
residuals.life.female<- resid(lm.exp.female)

plot(predicted.life.female, residuals.life.female) #This plot shows residuals centered around zero with a possible few outliers


#Problem 4.29

methanol<- read.csv('data-table-B20.csv')
lm.methanol<- lm(y~x_1 + x_2 + x_3 + x_4 + x_5, data = methanol)
summary(lm.methanol) #based on p-values, remove x_4 and rerun model:
lm.methanol.2<- lm(y~x_1 + x_2 + x_3 + x_5, data = methanol)
summary(lm.methanol.2) #based on p-values, remove x_5 and rerun model:
lm.methanol.3<- lm(y~ x_1 + x_2 + x_3, data = methanol)
summary(lm.methanol.3)

qqnorm(rstudent(lm.methanol.3)) #indicates distribution of residuals normal
qqline(rstudent(lm.methanol.3))

predicted.methanol<- predict(lm.methanol.3, interval = 'none')
residuals.methanol<- resid(lm.methanol.3)

plot(predicted.methanol, residuals.methanol) #residuals are uncorrelated and have mean approx 0

#The model with x_1, x_2, x_3 appears to be a suitable model

#Problem 5.2

data(p5.2)
attach(p5.2)

#Part A

plot(temp, vapor) #The plot is curved, indicating a non-linear relationship- a straight-line model would not be accurate

#Part B

my.lm<- lm(vapor~temp, data = p5.2)
summary(my.lm)

qqnorm(rstudent(my.lm)) #the plot is a curve, indicating a non-liner relationship
qqline(rstudent(my.lm))

predicted.mylm<- predict(my.lm, interval = 'none')
residuals.mylm<- resid(my.lm)

plot(predicted.mylm, residuals.mylm) #This plot shows a parabola, indicting a non-linear relationship

#Based on this analysis, I would conclude that a linear model is not satisfactory

#Part C

lm.transformed<- lm(log(vapor) ~(1/temp), data = p5.2)
summary(lm.transformed)

qqnorm(rstudent(lm.transformed)) #the plot is linear, indicating a normal distribution of residuals
qqline(rstudent(lm.transformed))

predicted.mylm.transformed<- predict(lm.transformed, interval = 'none')
residuals.mylm.transformed<- resid(lm.transformed)

plot(predicted.mylm.transformed, residuals.mylm.transformed)

#The residual plot is satisfactory. In conclusion, the data transformation makes a linear model appropiate

#Problem 5.5

data(p5.5)
attach(p5.5)

#Part A

defect.lm<- lm(defects~weeks, data = p5.5)
plot(weeks,defects) # plot of data is a curve
summary(defect.lm) #p-value for weeks is 2.35e-06

qqnorm(rstudent(defect.lm)) #the plot indicates a light-tailed residual distribution
qqline(rstudent(defect.lm))

predicted.defect<- predict(defect.lm, interval = 'none')
residuals.defect<- resid(defect.lm)

plot(predicted.defect, residuals.defect) #This plot shows a possible negative auto-correlation indicating the residuals are
#correlated rather than random

#Part- B
plot(weeks, log(defects))
defect.transformed<- lm(log(defects)~ weeks)
summary(defect.transformed)

qqnorm(rstudent(defect.transformed)) #the plot indicates a normal residual distribution
qqline(rstudent(defect.transformed))

predicted.defect.transformed<- predict(defect.transformed, interval = 'none')
residuals.defect.transformed<- resid(defect.transformed)

plot(predicted.defect.transformed, residuals.defect.transformed) #The plot indicates a satisfactory model

#Based on this analysis, the appropiate model is y~ log(x)

#Problem 5.7

methanol<- read.csv('data-table-B20.csv')
lm.methanol<- lm(y~x_1 + x_2 + x_3 + x_4 + x_5, data = methanol)
summary(lm.methanol) #based on p-values, remove x_4 and rerun model:
lm.methanol.2<- lm(y~x_1 + x_2 + x_3 + x_5, data = methanol)
summary(lm.methanol.2) #based on p-values, remove x_5 and rerun model:
lm.methanol.3<- lm(y~ x_1 + x_2 + x_3, data = methanol)
summary(lm.methanol.3)

qqnorm(rstudent(lm.methanol.3)) #indicates distribution of residuals normal
qqline(rstudent(lm.methanol.3))

predicted.methanol<- predict(lm.methanol.3, interval = 'none')
residuals.methanol<- resid(lm.methanol.3)

plot(predicted.methanol, residuals.methanol) #residuals are uncorrelated and have mean approx 0

#The model with x_1, x_2, x_3 appears to be a suitable model

#Transformation:

lm.methanol.transfored<- lm(y~ x_1+ log(x_2) + x_3, data = methanol)
summary(lm.methanol.transfored)

qqnorm(rstudent(lm.methanol.transfored)) #indicates distribution of residuals approx normal
qqline(rstudent(lm.methanol.transfored))

predicted.methanol<- predict(lm.methanol.transfored, interval = 'none')
residuals.methanol<- resid(lm.methanol.transfored)

plot(predicted.methanol, residuals.methanol) #indicated residuals are not correlated

#The transformation of y~ x_1 + log(x_2) + log(x_3) appears to be a more suitable model

#Problem 5.9

clathorate<- read.csv('data-table-B8.csv')
plot(clathorate)
lm.clathorate<- lm(y~ x1+ x2, data = clathorate)
summary(lm.clathorate)

qqnorm(rstudent(lm.clathorate)) #indicates distribution of residuals is heavy-tailed
qqline(rstudent(lm.clathorate))

predicted.clathorate<- predict(lm.clathorate, interval = 'none')
residuals.clathorate<- resid(lm.clathorate)

plot(predicted.clathorate, residuals.clathorate)

#Part B
lm.clathorate.tansformed<- lm(y~ x1 + log(x2), data = clathorate)
summary(lm.clathorate.tansformed)

qqnorm(rstudent(lm.clathorate.tansformed))
qqline(rstudent(lm.clathorate.tansformed))

predicted<- predict(lm.clathorate.tansformed, interval = 'none')
residuals.predicted<- resid(lm.clathorate.tansformed)

plot(predicted,residuals.predicted)

#Problem 5.10

pressure<- read.csv('data-table-B9.csv')
lm.pressure<- lm(y~x1+x2+x3+x4, data = pressure)
summary(lm.pressure)
#Remove x1 based on p-value
lm.pressure<- lm(y~x2+x3+x4, data = pressure)
summary(lm.pressure)
#Remove x4 based on p-value

lm.pressure<- lm(y~x2+x3, data = pressure)
summary(lm.pressure)

qqnorm(rstudent(lm.pressure))
qqline(rstudent(lm.pressure))

predicted<- predict(lm.pressure, interval='none')
residuals<- resid(lm.pressure)

plot(predicted, residuals)

#Part B

#Transformation

lm.pressure.transformed<- lm(log(y)~ x2 +x3, data = pressure)

qqnorm(rstudent(lm.pressure.transformed))
qqline(rstudent(lm.pressure.transformed))

predicted<- predict(lm.pressure.transformed, interval= 'none')
residuals<- resid(lm.pressure.transformed)

plot(predicted, residuals)
