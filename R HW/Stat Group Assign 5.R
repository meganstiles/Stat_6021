#Team 6: Megan Stiles, Tim Schroeder, James Rogol, Julina Zhang
library(leaps)
library(DAAG)
library(car)

training<- read.csv('teamassign05train.csv')
lm.training<- lm(y~x7, data=training)
plot(lm.training)

#Residual plot shows a funnel shape, implying that the variance is not constant and is potentially proportional to y

boxCox(lm.training)
#BoxCox analysis shows a lambda value = 0, which indicates a transformation of log(y).

lm.training.transformed<- lm(log(y)~., data= training)
plot(lm.training.transformed) #Normal Q-Q plot still appears to indicated a heavy-tailed distribution

#Perform Stepwise Model selection:

training.null<- lm(log(y)~1, data = training)
training.full<- lm(log(y)~., data = training)

step(training.null, scope=list(lower=training.null, upper=training.full), direction="forward")
#Selects x1, x2, x3, x4, x6, x7

step(training.null, scope=list(lower=training.null, upper=training.full), direction="both")
#Selects x1, x2, x3, x4, x6, x7

step(training.full, scope=list(lower=training.null, upper=training.full), direction="backward")
#Selects x1, x2, x3, x4, x6, x7

# From our Data Mining class (and the corresponding book - James, Witten, 
# Hastie, and Tibshirani. An Introduction to Statistical Learning. Springer, 
# 2013.), we have the following function to predict from regsubsets(), as well
# as the methods to cross-validate them.


predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Create a vector assigning each item to a fold:
set.seed(1)
k=10
folds <- sample(1:k, nrow(training), replace = TRUE)

# matrix to store the results 6 variables wide, 10 folds tall
cv.errors <- matrix(NA, k,6, dimnames = list(NULL, paste(1:6)))

# create a loop for CV!

for(j in 1:k){
  best.fit <- regsubsets(log(y)~.-x5, data = training[folds != j,], # e/t but j as training!
                         nvmax = 6)
  for (i in 1:6){
    pred <- predict(best.fit, training[folds == j,], id =i) # Predictions for j as test set, 1-19 times
    cv.errors[j,i] <- mean((log(training$y[folds==j])-pred)^2) # Testing MSE!
  }
}


mean.cv.errors <- apply(cv.errors,2, mean) # apply mean to the columns of errors for each column.
which.min(mean.cv.errors) # The "full" model with six variables has the lowest cross-validated MSE (all variables except x5)

#Perform Regression without variable x5:

lm.training.transformed5<- lm(log(y)~ x1+x2+x3+x4+x6+x7, data= training)
plot(lm.training.transformed5)
#Normal Q-Q plot still indicated heavy-tailed distribution

#Check Variables for collinearity:

vif(lm.training.transformed5)
#Remove variable x1, with a VIF value of 130.29

#Perform regression without x1 and x5:
lm.training.transformed5.1<- lm(log(y)~ x2+x3+x4+x6+x7, data=training)
plot(lm.training.transformed5.1)

#Check for collinearity
vif(lm.training.transformed5.1)

#Remove x3, with a VIF value of 31.31

#Perform regression without x1, x5, and x3:
lm.training.transformed5.1.3<- lm(log(y)~x2+x4+x6+x7, data=training)
plot(lm.training.transformed5.1.3)

#Check again for multicollinearity
vif(lm.training.transformed5.1.3) #No further multicollinearity indicted

#Check MSE of this model
predicted.y<-predict(lm.training.transformed5.1.3, newdata = training)
mse<- mean((exp(predicted.y)-(training$y))^2)
mse #MSE = 21.9, this seems high so now, we will try another method


#Perform Lasso 

# Create a vector of lambdas to test, from very large to very small.
grid <- 10^seq(10, -10, length = 100)

# Turn the X variables into a matrix for ridge and lasso regression
x<- model.matrix(log(y)~., data = training)

# Create a lasso model on the transformed Y
lasso.mod <- glmnet(x,log(training$y), alpha =1, lambda = grid, thresh = 1e-12)

# Set the seed for reproducable results
set.seed(1)

# 10-fold cross-validated lasso regression
lasso.out <- cv.glmnet(x, log(training$y), lambda = grid, alpha = 1)
plot(lasso.out)

# find the best lambda
bestlam <- lasso.out$lambda.min

# predict using the best lambda
lasso.predicts <- predict(lasso.mod, s = bestlam, newx = x)

#Take the Mean of the residuals (transforming the predictions back to match the
#existing y) for the MSE
mean((exp(lasso.predicts)-training$y)^2) # 14.00263


#Coefficients of the model (Everything but x3)
coef(lasso.out, id = which.min(lasso.out$lambda))

# Repeat the above for ridge regression
ridge.mod <- glmnet(x,log(training$y), alpha =0, lambda = grid, thresh = 1e-12)

set.seed(1)
ridge.out <- cv.glmnet(x, log(training$y), lambda = grid, alpha = 0)
plot(ridge.out)
bestlam2 <- ridge.out$lambda.min

ridge.predicts <- predict(ridge.mod, s = bestlam, newx = x)

mean((exp(ridge.predicts)-training$y)^2) # 14.00268

coef(ridge.out, id = which.min(ridge.out$lambda))

#Ridge and Lasso Regression produce similar MSE, we will go with the LASSO Regression because it eliminates x3 and 
# which gives us a simplier model

#Use Model to predict values of testing data:
lm.model.final<- lm(log(y)~x1+x2+x4+x5+x6+x7, data= training)
testdata<- read.csv('teamassign05test.csv')

#Use model to get predicted values
predicted.y<- predict(lm.model.final, newdata = testdata, interval="none")

#Transform predicted values

transformed.predicted<- exp(predicted.y)

#Write out predicted values
write.table(transformed.predicted, file = 'Team 6.csv', row.names = F, col.names = F, sep = ",")



