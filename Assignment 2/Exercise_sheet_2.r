### Stats with R Exercise sheet 2

###############################################################
# Deriving sampling distribution and confidence intervals
###############################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 14th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms


## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:

## Only 1 member needs to submit! 

###############################################################
### Exercise 1: Deriving sampling distributions
###############################################################
## In this exercise, we're going to derive sampling distributions for the mean with 
## different sizes.

## a) We will not use data from a normal distribution, but work with the poisson distribution, which is 
### often used for count data. We start by generating a large random sample of a poisson distribution
### with lambda = 1. Use rpois and create a sample of 1000 values, assign them to 'pdata'.
### Please first set a seed of 555 to have comparable results
set.seed(555)

## b) Take a look at your sample using the table() function and histogram and boxplot. 

## c) In what ways does this diverge from a normal distribution?
### Name at least 2 differences in reference to the plots and/or table

## d) Now, we are going to draw a smaller sample from our generated data set.
### Use the function sample() to create a sample of five instances from pdata.
### assign it to sample5

## e) draw another sample of 5 called sample5b

## f) calculate the mean of the two samples and store them in the vector means5

## g)   In order to draw a distribution of such a sample, we want to calculate the
###   mean of 1000 samples, each of size 5. However, we don't want to repeat 
###   question e and f 1000 times. Use a for loop to draw 1000 samples of size 5
###   and store the mean of each sample in the vector means5.

## h) Repeat the for-loop in question h, but use a sample size of 20. 
##    Assign this to 'means50' instead of 'means5'.

## i) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

## j) Draw histograms of means5 and means20. Describe in what way they differ

## k) Why do you observe a skew for means5, but not for means20?



###############################################################
### Exercise 2: Confidence interval
###############################################################

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.


## a) What does a confidence interval mean from the perspective of experiment replication?

## b) please install and load packages sciplot and lsr

## c) calculate 95% Confidence Intervals for pdata, sample5 and sample5b. You can
##    use the function ciMean()

## d) Why are these intervals so different?

## e) Is the true mean contained in the interval?

## f) In the following, we will check whether the CI behaves as expected.
### What is the true mean in our example?

## g) Change your for loop from above (means20) to calculate the confidence interval 
### insetad of the mean. Then check whether the confidence interval contains the
### true mean and save the result in the variable TrueMeanContained
### Hint: You will need to compare the mean to the lower and the upper bound of the confidence interval
### ciMean(YOURSAMPLE)[1] gives you the lower bound and ciMean(YOURSAMPLE)[2] the upper bound

## h) Given your results in TrueMeanContained, you now need to check, whether the interval really contains
### the mean 95% of the time. Does it?

## i) Confidence intervals are often used in plots. Lets construct a barplot with confidence intervals for
### the dataset chickwts, which contains weight of chickens after being administered different kinds of 
### food for 6 weeks.
### Use the function bargraph.CI to plot weight by feed, using the arguments response and x.factor

## j) Now additionally set the optional argument ci.fun to ciMean. How does the graph change and why?
### Hint: Look into the documentation of bargraph.CI.

