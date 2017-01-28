
## Start with given x-values
x <- read.table("teamassign01data.txt")[,1]
x

## Generate corresponding y-values according to the model y ~ 25 + 4x + e, where e~N(0,var=12^2)
y <- 25 + 4*x + rnorm(100, mean=0, sd = 12)

write.table(y, file = "yvalues.txt")


## Plot the relationship
plot(x,y, pch=20, cex=0.3)

data.set<- cbind(x,y)
data.set<- as.data.frame(data.set)
colnames<- c("x_values", "y_values")
names(data.set)<- colnames

write.csv(data.set, "data.csv")
my.lm<- lm((y_values)~(x_values), data = data.set)
my.lm

#Coefficients = 26.481 (intercept) slope = 3.926
new_prediction<- data.frame(x_values =18)
predict(my.lm, new_prediction, interval = "none")# 99.70545

#Report MSRes
anova(my.lm) #MSRes = 192,528

#Part 2

#b if the linear relationship is strong, then the slope should not vary significantly between models, thus the variance of B1 
#should be close to zero


