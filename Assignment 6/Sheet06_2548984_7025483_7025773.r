### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 12. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates') name, matriculation number. 
## Name:                Martin Banzer,  Ahmad Usman Khattak,  Tulika Nayak
## Matriculation number:2548984,        7025483,              7025773

###########################################################################################



#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(dplyr)
library(car)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

## a) For the further reference please use ?amis. 
## It may take some time to understand the dataset. 
?amis

## b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
## Feel free to make some plots and calculate some statistics in order to understand 
## the data.
data("amis")
data <- amis
str(data)

## c) All our columns have numeric type. Convert the categorical columns to factors.
for (i in 1:ncol(data)){
  data[,i] <- as.factor(data[,i])
}
str(data)

## d) Plot boxplots for the distribution of `speed` for each of the `period` values 
## (before, immediately after and after some time). Build 2 plots (each containing 3 
## boxplots) side by side depending on the `warning` variable.
## (For all plots here and below please use ggplot)
warning_1 <- data %>% filter(warning==1)
warning_2 <- data %>% filter(warning==2)

ggplot(warning_1, aes(period, as.integer(speed))) +
  geom_boxplot() + labs(x="period", y="speed", title="warning = 1")
ggplot(warning_2, aes(period, as.integer(speed))) +
  geom_boxplot() + labs(x="period", y="speed", title="warning = 2")

## e) What can you conclude looking at the plots? What can you say about people's 
## behaviour in different periods: before, immediately after and after some time?

# Answer:
# The graphs show, that having a warning sign correlates to a lower speed for all 
# three periods.
# Without a sign, people tend to maintain about the same speed in each period.
# However, with a sign, people slow down in period one and continue slowing down
# throughout period two. In period three they start accelerating again and get close
# to the speed of period three without a sign.

## f) What are your ideas about why the data with warning==2 (sites where no sign was 
## erected) was collected?

# Answer:
# warning==2 showcases the "normal" behavior of drivers. Without this data, the results
# from warning==1 couldn't be compared to anything and could therefore not be interpreted.


#######################
### Exercise 2: 1-way ANOVA
#######################

## a) First let's create a new data frame which will be used for all exercise 2.
## For the 1-way ANOVA we will be working with a subset of `amis` using only the 
## data for sites where warning signs were erected, which corresponds to warning==1. 
## Therefore first subset your data to filter out warning==2 and then apply group_by() and summarize() 
## to average "speed" over each "pair" and "period". 
## Assign this new data frame to the variable casted_data.
warning1_subset <- subset(data, warning == 1)
casted_data <- warning1_subset %>%
  group_by(pair, period) %>%
  summarise(avg_speed = mean(as.integer(speed)))


## b) Build boxplots of the average speed depending on "period".
casted_data %>% ggplot(aes(period, avg_speed)) +
  geom_boxplot()


## c) Looking at the boxplots, is there a difference between the periods?

# Answer:
# Yes. The avg_speed drops "right after" putting up warning sign as indicated by 
# period = 2, when compared to higher avg_speed that existed when there were no 
# warning signs (period = 1). But then "after some time" (period = 3) of putting  
# the sign avg_speed increases again and that too higher than "before" warning.


## d) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
## speed depending on the period and assign the result to aov1way
aov1way <- aov(avg_speed ~ period, data = casted_data)
aov1way

## Before we interpret the results, let's check the ANOVA assumptions and whether 
## they are violated or not and why.

## e) Independence assumption
## (Figure out the best way to check this assumption and give a detailed justified 
## answer to whether it is violated or not.)

# Answer:
# Assumptions are : Samples independent of each other, not correlated, randomly
# collected. The best way to check this assumption would be to see if same drivers 
# gave multiple observations

## f) Normality of residuals
##  First add the residuals to your casted data set, you find them in model$residuals
##  next, make a qqplot (using qqnorm() or geom_qq() ina ggplot) for the residuals and 
##  run the shapiro wilk test.
aov1way$residuals
qqnorm(aov1way$residuals)
shapiro.test(aov1way$residuals)

# Output of shapiro test:
# data:  aov1way$residuals
# W = 0.96144, p-value = 0.1662

## g) What do you conclude from your results in f?

# Answer :
# From f we see that there is normality of residuals 
# p-value is 0.1662 which is higher than 0.05

## h) Homogeneity of variance of residuals
##  First, plot the residuals by period (boxplots) to see whether variance differs between groups
##  Next, run Levene's test using the function leveneTest() (from library car) with the same syntax
##  as aov(). It indicates whether the variance is significantly different between groups (= not
##  homogeneous).
casted_data %>% ggplot(aes(period, aov1way$residuals)) +
  geom_boxplot()
leveneTest(avg_speed ~ period, data = casted_data)


## i) What do you conclude from your results in h?

# Answer: 
# We conclude that the variances are not homogenous

## j) Now we turn to the results. Look at the summary of aov1way
summary(aov(avg_speed ~ period, data = casted_data))

## k) State your conclusion
## Conclusion:
## we have F Value= 0.986 which is less and less f value means we are less likely to reject null hypothesis and
## when we look at p value we see 0.382 which is 38.2% which is way higher so we will not reject the null hypothesis.

## l) Please do pairwise t-tests of the same variables as in d) using pairwise.t.test().
pairwise.t.test(casted_data$avg_speed, casted_data$period, p.adjust.method="none" )

## m) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
pairwise.t.test(casted_data$avg_speed, casted_data$period, p.adjust.method="bonferroni" )

## n) If the results change  in m, why do they? What does Bonferroni correction do?

##ans:
## the p values have increased in m when we use bonferroni adjustment because now the alpha is being divided 
## by number of levels to prevent us from making type 1 error. that is what bonferroni correction does to make us
## prevent from making type 1 error.
#######################
### Exercise 3: 2-way ANOVA 
#######################
## a) Now we want to analyze the influence of 2 categorical variables 
## (period and warning) on the speed.
## So let's turn back to our initial dataset amis (not its subset with warning==1).
## First, we need to average the speed over each `pair`, `warning` and `period
## Cast your data again and assign the results to casted_data2.

data1<-amis
casted_data2 <- data1 %>%
  group_by(pair, warning ,period) %>%
  summarise(avg_speed = mean(as.integer(speed)))



## b) State the main difference between the applicability of 1-way and 2-way ANOVA.


##ans:
## in 1-way Anova we were looking for the affect of period on speed(one factor). but in 2-way anova we look for 
## the affect of 2 factors on the response variable simultaneously. so if we want to look for the affect of 
## warning and period both at the same time on response variable speed we will use 2-way anova.

## c) Do you think, we need to include an interaction term in the ANOVA?

##ans:
## since we could not rejected the null hypotheses in 1-way anova so there might be another factor that can 
## potentially be affecting  the response variable so we need to include interaction  to be sure if 
## there is an affect or not. in this case it is warning and period.


## d) Now apply the 2-way ANOVA: please use the function aov() with mean speed as the
## dependent variable, period and warning as predictor (independent) variables and depending on your
## answer in c) either including an interaction term, or not.
aov2<-aov(avg_speed ~ warning + period, data=casted_data2)
summary(aov2)

## e) Report the p-values and interpret the results in detail. Properly formulate the findings
##  with regard to the research question!

##ans:
##              Df Sum Sq Mean Sq F value  Pr(>F)   
##warning      1  122.8  122.81   8.592 0.00439 **
##period       1   26.0   26.04   1.822 0.18086  
##Residuals   81 1157.9   14.29   

## p value for warning is lower than 0.5 so we can say that warning is associated with speed
## p value for period is higher than 0.5 which means we cannot reject null hypothesis and period is not associated 
## with speed.
## in respect of research question we conclude from this 2-way anova test that warning sign does have affect on
## speed but the time this reading was taken (period) has no affect on speed.