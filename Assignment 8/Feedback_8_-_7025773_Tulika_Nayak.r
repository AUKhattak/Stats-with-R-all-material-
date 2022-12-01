### Stats with R Exercise sheet 7

#############################################################################
#Week8: Checking Assumptions underlying ANOVA and linear regression
#############################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 19. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates) name, matriculation number. 
## Name:                Martin Banzer,  Ahmad Usman Khattak,  Tulika Nayak
## Matriculation number:2548984,        7025483,              7025773

###############################################################################
###############################################################################

## The following line of code clears your workspace.

rm(list = ls())


#############################################################################
### Exercise 1
#############################################################################

#############################################################################
### Please, use ggplot to make plots in all exercises unless specified differently!
#############################################################################

library(languageR)
library(ggplot2)


##  We will again be using the lexdec dataset from library languageR. 
##  In sheet 4, we ran a t-test to test for differences in RT after a word vs after a non word.
##  In sheet 5, we looked at correlations between RT and frequency and length. In this sheet we will 
##  combine these analyses, look for interactions and again look at model assumptions
##  and model diagnostics

## a) Load the dataset lexdec from package languageR and store it in a variable called data

data("lexdec")
data <- lexdec 
str(data)
summary(data)

## b) Run a simple regression, just including Frequency as predictor and RT as the dependent variable
##  Store it in lm1
lm1 <- lm(RT~Frequency, data=data)

## c) Report and explain the effect of Frequency
lm1

# Answer:
# The negative value for the regression beta coefficient of "Frequency" shows, that
# a high frequency would result in a low predicted value for rt and vice versa.
# That is because the linear modle can be written as "RT = 6.58878 + (-0.04287) * Frequency".

## d) Make a scatterplot of RT by Frequency, including the regression line
ggplot(data, aes(Frequency, RT)) +
  geom_point() +
  stat_smooth(method = lm)

## e) Next, fit a model including Frequency and PrevType as predictors, store it in lm2
lm2 <- lm(RT~Frequency + PrevType, data=data)

## f) Report and explain the effects of Frequency and PrevType.
lm2

# Answer:
# Since the regression beta coefficient of "PrevType" is negative and lower then the 
# the also negative regression beta coefficient of "Frequency", prevtype has a 
# higher inpact on rt then frequency. However, rt benefits (i.e. gets reduced) from
# either one or both variables having a high value.
# That is because the linear model can be written as 
# "RT = 6.58878 + (-0.04316) * Frequency" + (-0.06557) * PrevType".

##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
 data$RT_pred = fitted(lm2)

## g) Now, plot the original data (RT by Frequency with different colors for PrevType), but use the 
## fitted values (RT_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines 
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.
 ggplot(data, aes(Frequency, RT, color=PrevType)) +
   geom_point() +
   geom_smooth(aes(Frequency, RT_pred), method = NULL)


## h) Run a regression model that includes also the interaction between Frequency and PrevType and store it
##  as lm3
 lm3<-lm(RT~Frequency * PrevType, data=data)
 lm3

## i) Plot the results of the model! (This time no need to specify the pred data set)
 data$RT_pred_int = fitted(lm3)
 ggplot(data, aes(Frequency, RT, color=PrevType)) +
   geom_point() +
   geom_smooth(aes(Frequency, RT_pred_int), method = NULL)

## j) Report the results of lm3 and interpret with the help of the graph in i)
 summary(lm3)
 
## as frequency increases, RT for word decreased slightly, in this case intercept is slightly above RT 6.5 for word
## as opposed to what we have seen in previous case where it was below RT 6.5. for non-word its more or less the 
## same

## k) Do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)
plot(lm3, which = seq(1,6))
#Please press enter every time to see the next plot 
 

## l) Interpret what you see in k) and possibly suggest further steps
# Answer:
# We see that Normal Q-Q plot is close to a straight line and
# Cook's distance is being affected due to some points with higher
# leverage.


## m) So, what assumptions are violated in the model as it is? Consider both your results from l and what you know 
##  about the data set from previous analyses.

# Answer:
# From the results of l) as well as previous analysis we see that the assumption of
# no bad outliers and uncorrelated predictors is violated.
