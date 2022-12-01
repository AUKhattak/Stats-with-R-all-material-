### Stats with R Exercise sheet 8

##########################
# Linear Mixed Effects Models
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 9. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.


## Please write below your (and your teammates) name, matriculation number. 
## Name:                Martin Banzer,  Ahmad Usman Khattak,  Tulika Nayak
## Matriculation number:2548984,        7025483,              7025773


###########################################################################################
###########################################################################################
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)


#####################################################
### 1. Linear mixed model for chicken growth 
#####################################################

## a) We will first look at the dataset ChickWeight, which is already loaded in base R. Check out 
##  the help page of the data set to understand how the data was collected and look at the summary
summary(ChickWeight)
?ChickWeight

## b) Let's plot the data. We will first follow the strategy from sheet 4, i.e. 
##  1. group the data by Diet and Time and use summarise() to get the mean and se (se() as provided below)
##    of weight. Assign resulting data set to aggData
se = function(x){sd(x)/sqrt(length(x))}

aggData <- ChickWeight %>% 
  group_by(Diet, Time) %>%
  summarise(mean_weight = mean(weight), se_weight = se(weight))

##  2. Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##    Also add errorbars (mean+/-1.96*se)
ggplot(aggData, aes(Time, mean_weight, col=Diet)) + 
  geom_line() + 
  geom_errorbar(aes(ymin=mean_weight-1.96*se_weight, ymax=mean_weight+1.96*se_weight))

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##  by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##  instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##  actual data
ggplot(ChickWeight, aes(Time, weight, col=Chick)) + 
  geom_line() +
  facet_grid(Diet ~ .)

## d) What do you observe, looking at c?

# Answer:
# The graphs for the diets 2,3 and 4 show a much more stable weight gain than the graph for diet 1 does.
# With a minimum of around 200g, diet 4 seems to result in the highest minimal weight, compared to the other graphs.
# Diet 4 also seems to have the lowest weight deviation among all diets.
# Diet 3 seems to have the biggest impact on weight gain at around day 16.
# Also, since some of the plotted lines are incomplete, it would be interesting to compare the mortality rate for 
# each diet (if an incomplete line means that a chick died).

## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##  looking for an interaction between time after birth and the diet type. Before running the model,
##  specify:
##  1) What fixed effect(s) do you enter into the model?

# Answer:
# Time, Diet

##  2) what random effect(s) should be included to account for the repeated measures structure of the data?

# Answer:
# Chick

##  3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?

# Answer:
# Time

## f) Run the model you specified in e) using lmer() and assign it to chickmod
chickmod <- lmer(weight ~ Time + Diet + (Time + 1 | Chick), data=ChickWeight, REML=F)
summary(chickmod)


## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull
chicknull <- lmer(weight ~ (1 | Chick), data = ChickWeight)


## h) compare the two models using the anova() function, which performs a likelihood ratio test
anova(chicknull, chickmod)


## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis

# Answer:
# The p-value is 2.2e-16. It is significantly less than 0.05 which means it is 
# statistically very significant. Thus the model with the smaller AIC must be 
# used i.e. chickmod. Thus Time and Diet have to used as predictors.


## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])

# Answer: 
# The ranef function has helped incorporate the random effects. The plot thus created
# shows how different Diets work together with Time, for each chick.


#####################################################
### 2. Random effect structures 
#####################################################

## a) Let's return to the lexdec data set and suppose, we want to look at effects of the word type of the 
## previously presented word (each subject saw a different randomized sequence) and effects of the complexity
## of the word itself, while taking into account the dependence between data points collected on the same 
## word and from the same subject. 
## Which of the following models has a maximal random effect structure given the experimental design?
## Motivate your choice.

m1 = lmer(RT ~ PrevType+ Complex+ (PrevType|Subject) + (Complex| Word), lexdec)
m2 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType| Word), lexdec)
m3 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType+Complex| Word), lexdec)
m4 = lmer(RT ~ PrevType+ Complex+ (Complex|Subject) + (PrevType| Word), lexdec)
m5 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (1| Word), lexdec)

##Answer
## m1 model seems to be the better model than rest of 4 models on the ground that PrevType and Complex are 
## having an affect on subject, therefore for subject its better if the slope is defined by them, Complexity seems
## to be having effect on word so it should explain its slope.


## b) You want to relate students' performance in the advanced algebra course in a summer school in Saarbrücken
##  to their final math grade in school. Performance is measured as the overall score in the final exam.
##  The summer school course has 200 participants, coming from 8 different partner Universities from all
##  over Germany. These 200 participants were randomly split into 10 tutorial groups, where each tutorial 
##  was held by a different tutor.
##  Given the design of your study, what random effects should you add to the model below?
##  Explain!!! If you want to, you can additionally add the random effects into the formula

## lmer(advancedalgebrascore ~ mathGrade, someData)


##answer
## For random effect i would choose partner university as the different tutor in different universities can make an
## impact on the student attending that course and i would add tutorial group after final exam keeping in mind the
## factor that individual tutor could be better at teaching or bad at it.
## 
