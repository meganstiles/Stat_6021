#Megan Stiles
#MES5AC

library(ForImp)
library(dplyr)
library(Amelia)
library(VIM)
library(mice)
###################
#Listwise Deletion#
###################

Data1<- read.csv('Homework08data01.csv')
Data2<- read.csv('Homework08data02.csv')
Data3<- read.csv('Homework08data03.csv')

col_names<- c('y', 'x1', 'x2')
names(Data1)<- col_names
names(Data2)<-col_names
names(Data3)<-col_names

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()


##Data 1 #####
i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample1<-sample_n(Data1, 500, replace=FALSE)
  
  #Perform listwise Deletion
  new_sample1<- ld(sample1)
  
  #Create Linear Model
  lm.new_sample1<- lm(y~ x1 + x2, data= new_sample1)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample1$coef[1]
  B1[i]<- lm.new_sample1$coef[2]
  B2[i]<- lm.new_sample1$coef[3]
 
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample1,new_sample1)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((new_sample1$y-predicted)^2)/nrow(new_sample1))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample1, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]

}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #476.9491
mean(Intercept) #29.05839
mean(B1) #5.69126
mean(B2) #3.744964

#Calculate the Variance of MSE and Coefficients
var(MSE) #768.5295
var(Intercept) #21.75007
var(B1) #0.04630634
var(B2) #0.1817036

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0.996
sum(Is_B1)/1000 # 0.994
sum(Is_B2)/1000 #0.994


#########################
#Listwise for Data#2 ####
#########################

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample2<-sample_n(Data2, 500, replace=FALSE)
  
  #Perform listwise Deletion
  new_sample2<- ld(sample2)
  
  #Create Linear Model
  lm.new_sample2<- lm(y~ x1 + x2, data= new_sample2)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample2$coef[1]
  B1[i]<- lm.new_sample2$coef[2]
  B2[i]<- lm.new_sample2$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample2,new_sample2)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((new_sample2$y-predicted)^2)/nrow(new_sample2))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample2, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #498.5916
mean(Intercept) #19.26391
mean(B1) #6.036477
mean(B2) #4.16062

#Calculate the Variance of MSE and Coefficients
var(MSE) #811.0889
var(Intercept) #35.60273
var(B1) #0.07513409
var(B2) #0.1922089

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0.866
sum(Is_B1)/1000 # 0.868
sum(Is_B2)/1000 #0.972

#####################
#Listwise for Data 3#
#####################

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample3<-sample_n(Data3, 500, replace=FALSE)
  
  #Perform listwise Deletion
  new_sample3<- ld(sample3)
  
  #Create Linear Model
  lm.new_sample3<- lm(y~ x1 + x2, data= new_sample3)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample3$coef[1]
  B1[i]<- lm.new_sample3$coef[2]
  B2[i]<- lm.new_sample3$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample3,new_sample3)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((new_sample3$y-predicted)^2)/nrow(new_sample3))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample3, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #429.5407
mean(Intercept) #50.6864
mean(B1) #5.173008
mean(B2) #2.994467

#Calculate the Variance of MSE and Coefficients
var(MSE) #681.3203
var(Intercept) #28.447
var(B1) #0.05557657
var(B2) #0.1503548

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0.074
sum(Is_B1)/1000 # 0.781
sum(Is_B2)/1000 #0.774

#########################
#Arithmetic Mean Imputation#
###########################


###########
## Data 1 #
###########
#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample1<-sample_n(Data1, 500, replace=FALSE)
  
  #Perform Arithmetic Mean Imputation
  sample1$y[is.na(sample1$y)] = mean(sample1$y, na.rm=TRUE)
  sample1$x1[is.na(sample1$x1)] = mean(sample1$x1, na.rm = TRUE)
  sample1$x2[is.na(sample1$x2)] = mean(sample1$x2, na.rm = TRUE)
  
  #Create Linear Model
  lm.new_sample1<- lm(y~ x1 + x2, data= sample1)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample1$coef[1]
  B1[i]<- lm.new_sample1$coef[2]
  B2[i]<- lm.new_sample1$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample1,sample1)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample1$y-predicted)^2)/nrow(sample1))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample1, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #501.5859
mean(Intercept) #54.28174
mean(B1) #4.593024
mean(B2) #2.853977

#Calculate the Variance of MSE and Coefficients
var(MSE) #571.9842
var(Intercept) #19.39208
var(B1) #0.04284479
var(B2) #0.134456

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0.002
sum(Is_B1)/1000 # 0.005
sum(Is_B2)/1000 #0.557

##########################
## Arithmetic Mean Data 2#
##########################

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample2<-sample_n(Data2, 500, replace=FALSE)
  
  #Perform Arithmetic Mean Imputation
  sample2$y[is.na(sample2$y)] = mean(sample2$y, na.rm=TRUE)
  sample2$x1[is.na(sample2$x1)] = mean(sample2$x1, na.rm = TRUE)
  sample2$x2[is.na(sample2$x2)] = mean(sample2$x2, na.rm = TRUE)
  
  #Create Linear Model
  lm.new_sample2<- lm(y~ x1 + x2, data= sample2)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample2$coef[1]
  B1[i]<- lm.new_sample2$coef[2]
  B2[i]<- lm.new_sample2$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample2,sample2)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample2$y-predicted)^2)/nrow(sample2))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample2, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #511.8981
mean(Intercept) #60.59879
mean(B1) #4.45177
mean(B2) #2.912285

#Calculate the Variance of MSE and Coefficients
var(MSE) #517.2478
var(Intercept) #20.46841
var(B1) #0.03993226
var(B2) #0.1358747

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0
sum(Is_B1)/1000 # 0.002
sum(Is_B2)/1000 #0.652

##########################
## Arithmetic Mean Data 3#
##########################

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample3<-sample_n(Data3, 500, replace=FALSE)
  
  #Perform Arithmetic Mean Imputation
  sample3$y[is.na(sample3$y)] = mean(sample3$y, na.rm=TRUE)
  sample3$x1[is.na(sample3$x1)] = mean(sample3$x1, na.rm = TRUE)
  sample3$x2[is.na(sample3$x2)] = mean(sample3$x2, na.rm = TRUE)
  
  #Create Linear Model
  lm.new_sample3<- lm(y~ x1 + x2, data= sample3)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample3$coef[1]
  B1[i]<- lm.new_sample3$coef[2]
  B2[i]<- lm.new_sample3$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample3,sample3)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample3$y-predicted)^2)/nrow(sample3))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample3, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #491.5417
mean(Intercept) #64.8689
mean(B1) #4.496901
mean(B2) #2.27828

#Calculate the Variance of MSE and Coefficients
var(MSE) #519.3287
var(Intercept) #18.65815
var(B1) #0.03686597
var(B2) #0.104637

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0
sum(Is_B1)/1000 # 0.001
sum(Is_B2)/1000 #0.071


#########################
## Regression Imputation#
#########################


#########################
### Data 1              #
#########################

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample1<-sample_n(Data1, 500, replace=FALSE)
  
  #Perform Regression Imputation
  sample1$x2<- as.factor(sample1$x2)
  sample1<- regressionImp(y~x1+x2, data = sample1)
  sample1<-regressionImp(x2~ x1+y, data = sample1)
  
  #Create Linear Model
  lm.new_sample1<- lm(y~ x1 + x2, data= sample1)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample1$coef[1]
  B1[i]<- lm.new_sample1$coef[2]
  B2[i]<- lm.new_sample1$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample1,sample1)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample1$y-predicted)^2)/nrow(sample1))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample1, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #362.703
mean(Intercept) #33.7937
mean(B1) #5.768892
mean(B2) #6.194994

#Calculate the Variance of MSE and Coefficients
var(MSE) #500.3383
var(Intercept) #39.863
var(B1) #0.04066632
var(B2) #52.06296

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0
sum(Is_B1)/1000 # 0.134
sum(Is_B2)/1000 #0.404


######################
## Data 2            #
######################

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample2<-sample_n(Data2, 500, replace=FALSE)
  
  #Perform Regression Imputation
  sample2$x2<- as.factor(sample2$x2)
  sample2<- regressionImp(y~x1+x2, data = sample2)
  sample2<-regressionImp(x2~ x1+y, data = sample2)
  
  #Create Linear Model
  lm.new_sample2<- lm(y~ x1 + x2, data= sample2)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample2$coef[1]
  B1[i]<- lm.new_sample2$coef[2]
  B2[i]<- lm.new_sample2$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample2,sample2)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample2$y-predicted)^2)/nrow(sample2))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample2, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #363.7628
mean(Intercept) #30.78819
mean(B1) #5.895901
mean(B2) #0.7109493

#Calculate the Variance of MSE and Coefficients
var(MSE) #447.4508
var(Intercept) #54.80398
var(B1) #0.05290501
var(B2) #60.84097

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0
sum(Is_B1)/1000 # 0.143
sum(Is_B2)/1000 #0.394

######################
## Data 3            #
######################

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample3<-sample_n(Data3, 500, replace=FALSE)
  
  #Perform Regression Imputation
  sample3$x2<- as.factor(sample3$x2)
  sample3<- regressionImp(y~x1+x2, data = sample3)
  sample3<-regressionImp(x2~ x1+y, data = sample3)
  
  #Create Linear Model
  lm.new_sample3<- lm(y~ x1 + x2, data= sample3)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample3$coef[1]
  B1[i]<- lm.new_sample3$coef[2]
  B2[i]<- lm.new_sample3$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample3,sample3)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample3$y-predicted)^2)/nrow(sample3))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample3, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #367.2205
mean(Intercept) #53.0367
mean(B1) #5.329927
mean(B2) #-1.92075

#Calculate the Variance of MSE and Coefficients
var(MSE) #537.8457
var(Intercept) #74.9332
var(B1) #0.04296086
var(B2) #82.82461

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0
sum(Is_B1)/1000 # 0.062
sum(Is_B2)/1000 #0.154

#####################################
## Hot Deck  Imputation#
#####################################

##########
## Data 1#
##########

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample1<-sample_n(Data1, 500, replace=FALSE)
  
  #Perform Hot-Deck Imputation
  sample1<- hotdeck(sample1)
  
  #Create Linear Model
  lm.new_sample1<- lm(y~ x1 + x2, data= sample1)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample1$coef[1]
  B1[i]<- lm.new_sample1$coef[2]
  B2[i]<- lm.new_sample1$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample1,sample1)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample1$y-predicted)^2)/nrow(sample1))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample1, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #727.2387
mean(Intercept) #57.01612
mean(B1) #4.612428
mean(B2) #2.28009

#Calculate the Variance of MSE and Coefficients
var(MSE) #3068.424
var(Intercept) #31.40006
var(B1) #0.07141504
var(B2) #0.2456573

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0.002
sum(Is_B1)/1000 # 0.056
sum(Is_B2)/1000 #0.206


#####################################
## Hot Deck  Imputation#
#####################################

##########
## Data 2#
##########

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample2<-sample_n(Data2, 500, replace=FALSE)
  
  #Perform Hot-deck Imputation
  sample2<- hotdeck(sample2)
  
  #Create Linear Model
  lm.new_sample2<- lm(y~ x1 + x2, data= sample2)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample2$coef[1]
  B1[i]<- lm.new_sample2$coef[2]
  B2[i]<- lm.new_sample2$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample2,sample2)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample2$y-predicted)^2)/nrow(sample2))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample2, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #740.3125
mean(Intercept) #63.41883
mean(B1) #4.467278
mean(B2) #2.33765

#Calculate the Variance of MSE and Coefficients
var(MSE) #3527.575
var(Intercept) #39.71891
var(B1) #0.07760037
var(B2) #0.2457063

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0
sum(Is_B1)/1000 # 0.027
sum(Is_B2)/1000 #0.225

##########
## Data 3#
##########

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample3<-sample_n(Data3, 500, replace=FALSE)
  
  #Perform Hot-Deck Imputation
  sample3<- hotdeck(sample3)
  
  #Create Linear Model
  lm.new_sample3<- lm(y~ x1 + x2, data= sample3)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample3$coef[1]
  B1[i]<- lm.new_sample3$coef[2]
  B2[i]<- lm.new_sample3$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample3,sample3)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample3$y-predicted)^2)/nrow(sample3))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample3, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #710.3896
mean(Intercept) #67.59726
mean(B1) #4.495615
mean(B2) #1.789471

#Calculate the Variance of MSE and Coefficients
var(MSE) #2992.134
var(Intercept) #32.15561
var(B1) #0.05953635
var(B2) #0.2221628

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0
sum(Is_B1)/1000 # 0.015
sum(Is_B2)/1000 #0.027


#######################
#### Indicator Method##
#######################

##########
## Data 1#
##########

#Create Empty Vectors that we will later use to store values
Intercept<- vector()
B1<- vector()
B2<- vector()
MSE<- vector()
Is_Intercept<- vector()
Is_B1<- vector()
Is_B2<- vector()
Lower_Intercept<- vector()
Upper_Intercept<-vector()
Upper_B1<-vector()
Lower_B1<- vector()
Upper_B2<-vector()
Lower_B2<-vector()

i=0
for(i in 1:1000) {
  #Take random sample of 500 values
  sample3<-sample_n(Data3, 500, replace=FALSE)
  
  #Perform Indicator Method Imputation
  sample3<- hotdeck(sample3)
  
  #Create Linear Model
  lm.new_sample3<- lm(y~ x1 + x2, data= sample3)
  
  #Append Values for Coefficients to emtpy vectors
  Intercept[i]<- lm.new_sample3$coef[1]
  B1[i]<- lm.new_sample3$coef[2]
  B2[i]<- lm.new_sample3$coef[3]
  
  #Predict new y values from linear model
  predicted<-predict(lm.new_sample3,sample3)
  
  #Calculate MSE for each model
  MSE[i]<- sum(((sample3$y-predicted)^2)/nrow(sample3))
  
  #Calculate Confidence Intervals for Parameters
  confindence_intervals<- confint(lm.new_sample3, level = 0.95)
  
  #Append Values to Vectors
  Lower_Intercept[i]<-confindence_intervals[1]
  Upper_Intercept[i]<- confindence_intervals[4]
  Lower_B1[i]<- confindence_intervals[2]
  Upper_B1[i]<-confindence_intervals[5]
  Lower_B2[i]<-confindence_intervals[3]
  Upper_B2[i]<-confindence_intervals[6]
  
}  

#Check to see if true value in interval
j=0
for(j in 1:1000) {
  if ((29.3<= Upper_Intercept[j]) & (29.3>=Lower_Intercept[j]))
    Is_Intercept[j]<-1
  else
    Is_Intercept[j]<- 0
  if ((5.6<= Upper_B1[j]) & (5.6>=Lower_B1[j]))
    Is_B1[j]<- 1
  else
    Is_B1[j]<-0
  if((3.8<= Upper_B2[j]) & (3.8>=Lower_B2[j]))
    Is_B2[j]<-1
  else
    Is_B2[j]<-0
}

#Calculate Mean of MSE and Coefficients
mean(MSE) #710.3896
mean(Intercept) #67.59726
mean(B1) #4.495615
mean(B2) #1.789471

#Calculate the Variance of MSE and Coefficients
var(MSE) #2992.134
var(Intercept) #32.15561
var(B1) #0.05953635
var(B2) #0.2221628

#Report Coverage of Parameters
sum(Is_Intercept)/1000 #0
sum(Is_B1)/1000 # 0.015
sum(Is_B2)/1000 #0.027