### Stats with R Exercise sheet 3

#################################################################################
#Tests for Categorical Data and cleaning data
#################################################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 21th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Make sure to provide an answer to all subquestions, even if you are not sure
## about it. If you don"t succeed, make sure to describe what you tried.
## Submit your homework via cms


## Please write below your (and your teammates) name, matriculation number. 
## Name:                  Martin Banzer,  Ahmad Usman Khattak,  Tulika Nayak
## Matriculation number:  2548984,        7025483,              7025773

## Only 1 member needs to submit! 

#################################################################################
##  Exercise 1: Cleaning data
#################################################################################

## download the file insomnia.csv from cms
## The made-up dataset insomnia contains data of a survey on 60 students. 
## They are asked two questions: whether they regularly encounter sleep problems
## and what their preferred and most consumed drink is.

#+# a. Load the libraries stringr, dplyr and tidyr.
install.packages("stringr")
install.packages("dplyr")
install.packages("tidyr")

library(stringr)
library(dplyr)
library(tidyr)


#+# b. read in the data
insomnia_data <- read.csv(file = 'insomnia.csv')


#+# c. get a summary of the dataset
summary(insomnia_data)



#+# d. how many students encounter sleep problems?
insomnia_data$sleepProblem <- as.factor(insomnia_data$sleepProblem)
summary(insomnia_data)

# Answer:
# summary() showed the mean, median, quartiles of values in sleepProblem column 
# but sleepProblem column has 0 for False, 1 for True. Thus it should be factor.
# After changing the variable type to factor, we get 0:38 and 1:22 in the 
# summary() for sleepProblem. Thus, there are 22 students with sleep problems.


#+# e. how many different drinks do students name? (transform the variable into a 
## factor first)
insomnia_data$drink <- as.factor(insomnia_data$drink)
levels(insomnia_data$drink)
insomnia_data %>% count(drink)

# Answer:
# [1] " Coffee" "coffe"   "coffee"  "tea"     "Tea"     "tea "    "tee"     "water" 
# Students have named 3 diffrent drinks i.e. Coffee, Tea, Water. But there are 8 
# different variations in their mention due to data entry errors.

#+-# f. collapse factor levels which were spelled wrong. Make sure you first handle
## case and whitespace incongruencies, before you fix individual misspellings
## with gsub

#remove whitespaces
insomnia_data$drink <- gsub(" ", "", insomnia_data$drink)

#convert all strings to lower
insomnia_data$drink <- tolower(insomnia_data$drink)

#collapse all factor levels with wrong spellings
insomnia_data$drink <- gsub("tee", "tea", insomnia_data$drink)
insomnia_data$drink <- gsub("coffe", "coffee", insomnia_data$drink)
insomnia_data$drink <- gsub("coffeee", "coffee", insomnia_data$drink)



## You realize that most students had multiple exams in the week from Feb 22 to 
## Feb 26. As students had to learn a lot and were possibly worried, they might 
## misjudge or exaggerate their sleep problems as occurring "regularly"
## We want to exclude all data that was collected between and including Feb 15 
## and Feb 26!

#+# g.  First show how many data points will be concerned, you need to transform
##     the date column to a Date object first!
insomnia_data$date <- as.Date(insomnia_data$date)
summary(insomnia_data)
print(insomnia_data %>%
        filter(date >= as.Date("2021-02-15") &
                 date <= as.Date("2021-02-26")) %>%
        count())

# Answer:
# 10 rows of data are affected due to the exclusion


#+# h. Now filter out this part of the data and assign the result to clean
clean <- insomnia_data %>% 
  filter(date < as.Date("2021-02-15") | date > as.Date("2021-02-26"))
clean

#################################################################################
### Exercise 2: chi-squared test
#################################################################################

## consider the data set from above. If you had problems performing the
## required cleaning steps, note that you can also do them by hand
## Now consider we want to see whether the preferred drink influences sleep problems

#+# a. formulate in plain English what the Null hypothesis is in this context

# Answer:
# The preferred drink does not influence sleeping problems.

#+# b. conduct a chi-squared test to test this hypothesis using the function chisq.test()
##    and assign the result to chi
chi <- chisq.test(clean$drink, clean$sleepProblem)

#+# c. the last call produced a warning. To understand why this warning arises, look
##    at observed and expected frequencies of chi. Why do you think it produced the error?

# Answer:
# The problem might be that the expected values for sleepingProblem == 1 are very small (all less then 5).
# These small values can lead to a wrong approximation of p.


#-# d. What are the expected frequencies? Do we need to look at expected or 
##    observed frequencies?
chi$expected

# Answer:
# Since we need to compare the observed to the expected frequencies, we need to look at both.

#-# e. a possible solution is to sample more participants. Given that the smallest 
##    admissible value is 5, from which group(s) in terms of preferred drinks do
##    we have to sample more?
chi$observed

# Answer:
# We would need more samples for the water group, since water&(sleepProblem==1) => 1

#+# f. Assume we don't have the possibility to sample more students. Which test do
##    you have to run instead? How does it work roughly? Perform a suitable test
fish <- fisher.test(clean$drink, clean$sleepProblem)

# Answer:
# We must run the Fisher's Exact Test instead.
# This test calculates the sum over all possible 2*2 tables that were at least as
# extreme as the original data table. This sum is then compared to 0.05 to decide 
# about keeping (> 0.05) or rejecting (< 0.05) the null hypothesis. 
# (see slides for lecture 4, page 39)

#+# g. Lastly, what is the conclusion of your test? What have you learned and what 
##    have you not learned? 
fish

# Answer:
# Since the p-Value of the Fisher's Exact Test is 0.01815 and therefore less then
# 0.05, we must reject our null hypothesis and assume that preferred drinks
# do influence sleeping problems. However, we have not learned how the drinks 
# influence sleeping problems (whether they increase or decrease sleeping problems).

#################################################################################
##Exercise 3. Binomial distribution
#################################################################################
## Suppose there are 18 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

#+# a. Please calculate the probability of getting exactly 5 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.
dbinom(5,18,0.2, log=FALSE)
#-# b. Next please calculate the probability of answering 6 or more questions 
##    correctly by chance.
pbinom(6,18,0.2,  lower.tail = FALSE)
#################################################################################
#+#Exercise 4
#################################################################################
##   Consider the data set used in Ex 1&2. How would the experiment have to change
##   in order for you to choose McNemar's test over the ChiSquare test? 
##   What would be the problem of using the normal ChiSquare test in a case where 
##   McNemar's test would be more appropriate?
##Answer:
## McNemar's test is used for a data set where the values may change. In insomnia case, 
##if study was conducted in a way that it observed the data before and after drinking water or coffee (observing
##relation between dependent variable)then in that case we would choose McNemar test instead of ChiSquare test.
##if we use normal chisqaure test instead of McNemar's test, the problems is that it assumes independence
## and we are not looking for the independence so the answer of that test cannot be trusted.
 





