#Megan Stiles

#MES5AC
library(MASS)

#Problem 6.12
wine<- read.csv('data-table-B11.csv')

wine.lm<- lm(Quality~ Clarity +Aroma + Body+ Flavor+ Oakiness, data = wine)
summary(wine.lm)

cooks.distance(wine.lm)
#since none of the Di >1 for this model, we do not see influential points.

dfbetas(wine.lm)
#n = 38, 2/sqrt(38) = 0.324428
#since observations 2, 4, 9, 12, 20,23,24, 31, 37 have dfbeta values > 2/sqrt(n), they should be investigated for influence, however
#since none of the other measures indicate these points it is unlikely they are influential

dffits(wine.lm)
#cutoff = 2*sqrt(38/7) = 4.659
#Based on this measure, no points meet the cutoff

covratio(wine.lm)
#cutoff= 1 +- 3*7/38 = 1.55, 0.4736
#No points appear as influential based on this metric

#Problem 6.13
heat<- read.csv('data-table-B12.csv')

heat.lm<- lm(pitch~ temp+ soaktime + soakpct + difftime +diffpct, data = heat)

cooks.distance(heat.lm)
#since none of the Di >1 for this model, we do not see influential points.

dfbetas(heat.lm)
#n = 32, 2/sqrt(32) = 0.354
#Based on this measure, observations 31, 29 should be investigated for influence

dffits(heat.lm)
#cutoff = 2*sqrt(32/6) = 4.6188
#Based on this measure, no points meet the cutoff

covratio(heat.lm)
#cutoff = 1 +- 3*6/32 = 1.5625, 0.4375
#based on this measurement, observations 5, 31, and 32 should be investigated for influence.

#Problem 6.14

jet<- read.csv('data-table-B13.csv')

jet.lm<- lm(y~.,data = jet)

cooks.distance(jet.lm)
#Observation 20 have a Di >1 so should be studied for influence.

dfbetas(jet.lm)
#n = 40, 2/sqrt(40) = 0.316
#Based on this measurement, observation 20, 12 should be considered for influence

dffits(jet.lm)
#Cutoff = 2*sqrt(40/7) = 4.78914
#Based on this measurement, observation 20 appears to be influential

covratio(jet.lm)
#cutoff = 1 +- 3*7/40 = 1.525, 0.475
#observations 6, 9, 10, 15 should be investigated for influence

#Problem 6.15

data<- read.csv('data-table-B14.csv')

data.lm<- lm(y~ x1 + x2+ x3 + x4, data = data)

cooks.distance(data.lm)
#Observations 2, 4 have D1 >1 and should be investigated for influence

dfbetas(data.lm)
#n = 25, 2/sqrt(25) = 0.4
#Based on this measure, observations 8, 4, 2, 9 should be considered for influence

dffits(data.lm)
#Cutoff = 2*sqrt(25/5) = 4.427136
#Observations 2 should be investigated for influence based on this measure

covratio(data.lm)
#cutoff = 1 +- 3*4/25 = 1.48, 0.52
#Observation 10, 15, 19, 20, 25 are outside the desirable range

#Problem 8.5

gasoline<- read.csv('data-table-B3.csv')

gasoline.lm<- lm(y~ x10 + x11, data = gasoline)

#Part A
summary(gasoline.lm)
#p-value of x11 is > 0.05, so this variable is not significant and type of transmission does not significantly affect 
#gas mileage

#Part B
gasoline.lm2<- lm(y~ x10+x11 + x10*x11, data = gasoline)
summary(gasoline.lm2)
#x10, x11, and x10*x11 all appear to be significant
#An automatic transmission reduces gas milage overall, but a combination of a heavier vehicle and an automatice transmission
#will slightly increase gas mileage. Likely automatic transmissions have better gas mileage for heavier cars.

#Problem 8.6

football<- read.csv('data-table-B1.csv')
i = 0
for(i in 1:28) {
  if (football$x5[i] > 0){
    football$positive[i]<- 1
  } else {
    football$positive[i] <- 0
  }
}

i = 0
for (i in 1:28) {
  if (football$x5[i] == 0) {
    football$equal[i] <- 1
  } else {
    football$equal[i] <- 0
  }
}

i = 0
for (i in 1:28) {
  if (football$x5[i] < 0) {
    football$negative[i] <- 1
  } else{
    football$negative[i] <-0
  }
}

football.lm<- lm(y~ x8 + x7 +negative + positive + equal, data = football)
summary(football.lm)

# the p-values for negative and positive are greater than 0.05, so the turnover differential does not appear to 
#have a significant affect on game outcome