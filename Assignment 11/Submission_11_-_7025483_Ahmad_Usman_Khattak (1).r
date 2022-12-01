### Stats with R Exercise sheet 10

####################################################
# Week 11: Model Selection, Transformations, Power
####################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 23. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name:                Martin Banzer,  Ahmad Usman Khattak,  Tulika Nayak
## Matriculation number:2548984,        7025483,              7025773

###############################################################################
###############################################################################

# The following line of code clears your workspace.

rm(list = ls())


#########################################################################
### Exercise 1  Simplifying random effect structures
#########################################################################


library(lme4)
library(languageR)

##  Using the lexdec data set again, you want to fit the model that tests for effects of Frequency, the type of the 
##  previous Word and the native language of the participant:

m = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)

## a) Unfortunately, the maximal model given above gives a warning that indicates that the model is too complex for the data.
##  In order to get a model that converges without warnings, try to use backwards selection on the random effects. 
##  First exclude the random effect that is least contributing to the model fit and so on (this may require multiple 
##  steps and a large number of fitted models!). Use model comparison to decide which effects
##  can be excluded.
##  You may exclude random effects only, if they don't contribute significantly with alpha set to 0.1
summary(m)
m1 = lmer(RT ~ PrevType+  Frequency* NativeLanguage+(PrevType+Frequency|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)
m2 = lmer(RT ~ PrevType*Frequency + NativeLanguage+(PrevType+Frequency|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)
m3 = lmer(RT ~ PrevType+  Frequency + (PrevType+Frequency|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)


# Answer:
summary(m)
# AIC      BIC   logLik deviance df.resid 
# -986.5   -894.5    510.3  -1020.5     1642 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.4564 -0.6180 -0.1106  0.4563  6.3529 

summary(m1)
# AIC      BIC   logLik deviance df.resid 
# -989.6   -892.1    512.8  -1025.6     1641 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.5488 -0.6136 -0.1082  0.4511  6.3672

summary(m2)
# AIC      BIC   logLik deviance df.resid 
# -989.3   -891.9    512.7  -1025.3     1641 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.5059 -0.6128 -0.1142  0.4513  6.4008 

summary(m3)
# AIC      BIC   logLik deviance df.resid 
# -985.6   -899.0    508.8  -1017.6     1643 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.4499 -0.6165 -0.1132  0.4562  6.3498


## b) Comment on your result in a): were you able to produce a suitable model without convergence problems?

# Answer:
# The models still continue to give warnings. We haven't removed the random effect
# that is least contributing to the model fit. We weren't able to produce a suitable
# model without convergence problem.


## c) Another approach is to simplify the random effect structure by excluding correlations. 
##  Try out whether this would have solved the problem.

# Answer:
# We can apply PCA to the covariance matrix of the model to find if model is
# overfitting. Then we can fit a zero correlation parameter to identify random
# effects with zero, or very small variance.


#########################################################################
### Exercise 2  Simulations and power
#########################################################################

## In the following we provide you with code for simulations. The goal of the exercise is for you to try out
## the code and understand what it does.
## Please always execute the code at least 5 times for each subquestion, to see how stable the results are 
##  -- this is necessary because we are sampling the data randomly, so it could be that we sometimes get more or 
##  less "lucky" draws. 


n <- 200 # number of observations to be simulated
predA <- rnorm(n, 80, 20)
predB <- rnorm (n, 30, 30)
interact <- 0.02*(predA*predB) 
error <- rnorm (n, 0, 50)

resp <- 32 + 0.02*predA - 2.4*predB + interact + error

d <- data.frame(predA, predB, resp)

## a) Write down what values you would hope for the model to estimate in the ideal case:
##   i)  intercept= 32
##   ii) predA= 0.02
##   iii)predB= -2.4
##   iv) predA:predB = 0

m1<- lm(resp~predA*predB, data=d)
summary(m1)  

## b) Can the model recover the original model structure and estimate correct coefficients 
##  for the predictors?

# Answer:
# We did not entirely understand what was meant by "recover the original model structure".
# It would be possible to simply extract "predA", "predB", "interact" and "error" from the model.
# With these values the original model structure could then be reconstructed, if 
# one knew the original structures outline.

# If we understood it correctly, then there is no such thing as a wrong estimate of coefficients, if the input date 
# was sufficient enough for the given task. 
# Therefor, yes the model can predict correct coefficients for the predictors.

## c) What happens if you change the number of subjects? (specify the numbers you tried out)
for (i in c(1,2,3,4,5)){
  print("--------start---------")
  n <- 10000
  predA <- rnorm(n, 80, 20)
  predB <- rnorm (n, 30, 30)
  interact <- 0.02*(predA*predB) 
  error <- rnorm (n, 0, 50)
  resp <- 32 + 0.02*predA - 2.4*predB + interact + error
  d <- data.frame(predA, predB, resp)
  m1<- lm(resp~predA*predB, data=d)
  print(summary(m1))
  print("--------fin---------")
}

# Answer:
# For very low numbers of subjects the std. error for intercept grows very rapidly.
# n=10 -> std. Error ca. 80
# n=30 -> std. Error ca. 60
# n=200 -> std. Error ca. 20
# Also, the p-value grows significantly.
# n=10 -> std. p-val ca. 0.14
# n=30 -> std. p-val ca. 0.04
# n=200 -> std. p-val ca. 2.2e-13

# For very high numbers of subjects the std. error for intercept decreases very rapidly.
# n=2000 -> std. Error ca. 7
# n=8000 -> std. Error ca. 3
# Also, the p-value shrinks significantly.
# n=2000 -> std. p-val ca. 2.2e-16
# n=8000 -> std. p-val ca. 2.2e-16

## d) What happens if you change the variance of the error term? (specify the numbers you tried out)

for (i in c(1,2,3,4,5)){
  print("--------start---------")
  n <- 200
  predA <- rnorm(n, 80, 20)
  predB <- rnorm (n, 30, 30)
  interact <- 0.02*(predA*predB) 
  error <- rnorm (n, 0, 150)
  resp <- 32 + 0.02*predA - 2.4*predB + interact + error
  d <- data.frame(predA, predB, resp)
  m1<- lm(resp~predA*predB, data=d)
  print(summary(m1))
  print("--------fin---------")
}

# Answer:
# A lower variance results in significantly lower std. errors for every coefficient and in a much smaller p-value.
# var = 1 -> std. error intercept   ca. 0.38
#                       predA       ca. 0.004
#                       predB       ca. 0.009
#                       predA:predB ca. 0.0001
#             p-val = 2.2e-16
# var = 5 -> std. error intercept   ca. 1.88
#                       predA       ca. 0.02
#                       predB       ca. 0.04
#                       predA:predB ca. 0.0005
#             p-val = 2.2e-16
# var = 50 -> std. error intercept   ca. 18.67
#                       predA       ca. 0.23
#                       predB       ca. 0.48
#                       predA:predB ca. 0.006
#             p-val = 2.672e-08

# Higher values result in a higher std. errors for every coefficient.
# var = 100 -> std. error intercept   ca. 46.77
#                         predA       ca. 0.56
#                         predB       ca. 1.13
#                         predA:predB ca. 0.01
#             p-val = 9.554e-06
# var = 150 -> std. error intercept   ca. 70.34
#                         predA       ca. 0.82
#                         predB       ca. 1.79
#                         predA:predB ca. 0.02
#             p-val = 0.4015


## e) What happens if you change the effect sizes?
##Ans:

## Effect size and type II error are closely related to each other. if we increase the effect size
## then type II error will have less chances.


##  Next we include the above code into a loop to calculate the power of the experiment 

# number of simulated data sets
sim = 1000
n = 80
# results matrix
results = matrix(nrow=sim, ncol=4)
colnames(results) <- c("Intercept", "predA", "predB", "interaction")
for(i in c(1:sim)){
  predA <- rnorm(n, 80, 20)
  predB <- rnorm (n, 30, 30)
  interact <- 0.02*(predA*predB) 
  error <- rnorm (n, 0, 50)
  resp <- 32 + predA - 2.4*predB + interact + error
  d <- data.frame(predA, predB, resp)
  m1<- lm(resp~predA*predB, data=d)
  # store the resulting p-values in the results matrix
  results[i,] = summary(m1)$coefficients[,4]
}

## f. We use the above code and the results matrix to calculate power. Recall that the power is 
##  the probability of rejecting the Null hypothesis, given a specific effect size.
##  We can approximate this by calculating the proportion of simulated datasets, 
##  where the effect comes out significant, i.e. below 0.05. 
##  Calculate the power based on the simulations for all three effects of interest 
##  (i.e., predA, predB and the interaction) individually.

alpha = 0.05
print("Power for PredA")
print(length(which(results[,2] <alpha ))/sim)
print("Power for PredB")
print(length(which(results[,3] <alpha ))/sim)
print("Power for Interaction")
print(length(which(results[,4] <alpha ))/sim)

##[1] "Power for PredA"
##[1] 0.778
##[1] "Power for PredB"
##[1] 0.901
##[1] "Power for Interaction"
##[1] 0.618



## g. How does power change when you decrease your alpha level to 0.01?

##[1] "Power for PredA"
##[1] 0.551
##[1] "Power for PredB"
##[1] 0.744
##> print("Power for Interaction")
##[1] 0.359
## power has decreased

## h. How does power change, when you decrease the number of participants in each simulated data 
##  set to 80? (alpha-level = 0.05)

##[1] "Power for PredA"
##[1] 0.674
##[1] "Power for PredB"
##[1] 0.824
##[1] "Power for Interaction"
##[1] 0.53
## again the power has decreased
