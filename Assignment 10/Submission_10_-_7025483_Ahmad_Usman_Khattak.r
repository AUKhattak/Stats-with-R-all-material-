####################################################
#Week 11: Model Families and Logistic Regression
####################################################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 16. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name:                Martin Banzer,  Ahmad Usman Khattak,  Tulika Nayak
## Matriculation number:2548984,        7025483,              7025773


##################################################################################

##################################################################################
## Exercise 1: Logistic regression
##################################################################################
install.packages("lme4")
require(carData)
require(dplyr)
require(lme4)
require(ggplot2)

## Look at the dataset TitanicSurvival from the carData package.
str(TitanicSurvival)
summary(TitanicSurvival)
head(TitanicSurvival)


## a) Build a simple logistic regression model that models the probability of survival (binary) based on 
##  sex (categorical) and passengerClass (categorical) without an interaction and store it in mSurv. 
##  You have to use the glm() function and specify the family correctly.

mSurv = glm(data = TitanicSurvival, survived ~ sex + passengerClass, family = binomial(link = "logit"))

## b) Look at the summary. What group does the intercept correspond to?

summary(mSurv)

# Output:
#
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.1091     0.1728  12.203  < 2e-16 ***
# sexmale            -2.5150     0.1467 -17.145  < 2e-16 ***
# passengerClass2nd  -0.8808     0.1977  -4.456 8.34e-06 ***
# passengerClass3rd  -1.7231     0.1715 -10.047  < 2e-16 ***
#
# Answer:
# The intercept represents Female (sexfemale) passengers that took 1st class
# (passengerClass1st) who had 2.1091 log-odds of survival.


## c) Were men more likely to survive than women? Is the effect significant?
# Answer:
# No, men were not more likely to survive than women. The coefficient for "sexmale"
# is the difference in logit survival ratio between sexmale and sexfemale passengers.
# So, sexmale logit survival is 2.1091-2.5150 i.e. -0.4059. So survival odds is
# e^-0.4059 = 0.6664. So, probability of survival = 0.3999
#
# Yes, the effect is significant as p-value is much less than 0.05


## d) Imagine two passengers: Rose (female, 1st class passenger) and Jack (male, 3rd class passenger).
##  Calculate their expected survival on the logit scale (i.e. the scale of the model) either by hand or 
##  using predict() with a new data.frame
new_mSurv = glm(data = TitanicSurvival, survived ~ sex * passengerClass, family = binomial(link = "logit"))
summary(new_mSurv)

# output:
# Coefficients:
#                           Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                 3.3250     0.4549   7.309 2.68e-13 ***
# sexmale                    -3.9848     0.4815  -8.277  < 2e-16 ***
# passengerClass2nd          -1.2666     0.5485  -2.309   0.0209 *  
# passengerClass3rd          -3.3621     0.4748  -7.081 1.43e-12 ***
# sexmale:passengerClass2nd   0.1617     0.6104   0.265   0.7911    
# sexmale:passengerClass3rd   2.3039     0.5158   4.467 7.95e-06 ***
#
# Answer:
# Rose is female, 1st class which corresponds to the intercept.
# Therefore, her log-odds of survival is 3.3250
# So, her odds of survival is e^3.3250 = 27.7989
#
# Log-odds difference in survival between male 1st class passengers and male 
# 3rd class passengers is -3.3621+2.3039 = -1.0585
#
# So, Odds of survival for Jack is:
#e^(3.3250-3.9848-1.0585) = e^-1.7183 = 0.1794

## e) Transform your results from d) to the probability scale, using the formula given on the slides. 
##  You can check your calculation by asserting the probabilities lie in the 0-1 range. For whom does 
##  the model predict the higher probability of survival?
# Answer:
# Probability of survival for Rose:
# = 27.7989/(1 + 27.7989)
# = 0.9652
#
# Probability of survival for Jack:
# = 0.1794/(1 + 0.1794)
# = 0.1521
#
# The model predicts higher probability of survival for Rose

##################################################################################
## Exercise 2: Generalized Linear Mixed effect models
##################################################################################

## In this exercise, we will again look at connections between coffee consumption and sleep (among others). 
## The data set "coffee.csv" contains data from 10 students, who reported on 10 randomly chosen days of the year: 
##  sleep: how many hours of sleep they had in the previous night
##  mood: how happy they felt on a scale from 1 (very unhappy)-10 (extremely happy)
##  coffee: how many cups of coffee they had on that day
##  In addition, the maximal temperature on that day was entered into the dataset.

## Our research hypotheses are: 
## students consume more coffee, when they are tired
## students consume more coffee, if they are in a bad mood.
## students consume more coffee, when it is cold outside

## a) Download the data set from cms and read it in, store it in a variable called: coffeedat
coffeedat <- read.csv(file = 'coffee.csv')
head(coffeedat)
summary(coffeedat)

## b) Plot the number of consumed cups of coffee in three individual scatterplots by sleep, mood, and temperature. 
##  You can use geom_jitter() to get a nicer plot
ggplot(coffeedat, aes(x = coffee, y = sleep)) + geom_jitter()
ggplot(coffeedat, aes(x = coffee, y = mood)) + geom_jitter()
ggplot(coffeedat, aes(x = coffee, y = temperature)) + geom_jitter()

## c) Can you detect an obvious relationship in any of the plots? Which direction does it have?

# Answer:
# Sleep:
# It seems like people tend to drink less/no coffee if they have slept less than five or more than 9 hours. 
# However, 8 cups of coffee seems to be the maximum for most people within this range for sleep.

# Mood:
# The consumption seems to increase with a decreasing mood.

# Temperature:
# Small amounts of coffee seem to be consumed only, if the temperature is between 10 and 20.
# 5 cups of coffee are consumed between 10 and 30 temperature units.
# And more than 5 cups are again consumed with temperatures between 20 and 10.

## d) fit a simple linear regression model with all three predictors and store it in linmod
linmod <- lm(coffee ~ sleep + mood + temperature, data=coffeedat)

## e) fit a generalized linear model with the appropriate family (hint: coffee is a count variable) and
##  store it in poimod
poimod <- glm(coffee ~ sleep + mood + temperature, data=coffeedat, family=quasipoisson())

## f) Look at the two summaries, what changed?
summary(linmod)
summary(poimod)

# Answer:
# Poimod has a far lower error rate for all three predictors than linmod, which should correlate with a higher accuracy.
# The loss-values of poimod are also only half as high as for linmod.

## g) In fact, we have repeated measures in our design, so refit the model including a random intercept for
##  subject using glmer() with the correct family specification and store it in mixedpoi
mixedpoi <- glmer(coffee ~ sleep+mood+temperature + (1|subj), data = coffeedat, family =poisson())
## h) Look at the summary and report what changed in comparison to both linmod and poimod.
summary(mixedpoi)
## The Values are low for temp but overall on higher pattern.

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin
mixedlin <- glmer(coffee ~ sleep+mood+temperature + (1+sleep+mood|subj), data = coffeedat, family = gaussian())

## j) Compare the AIC for all four models. Which one has the best fit?
AIC(linmod, poimod, mixedpoi, mixedlin)

## k) And which model is conceptually the appropriate one? Why?
## among all models the one that seems appropriate is "mixedpoi", because what it does is random intercept for
## every subject (people).

## l) Finally, report on the effects of interest in light of our research hypotheses specified above for the 
##  model you chose in k)
## In the light of research hypothesis student tend to consume more coffee when they are tired and when they are in 
## bad mood but with temperature it does not show significant effect.
