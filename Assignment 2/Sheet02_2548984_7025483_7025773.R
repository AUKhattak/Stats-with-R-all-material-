### Stats with R Exercise sheet 2

###############################################################
# Deriving sampling distribution and confidence intervals
###############################################################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 14th. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms


## Please write below your (and your teammates) name, matriculation number. 
## Name:                  Martin Banzer, Ahmad Usman Khattak, Tulika Nayak
## Matriculation number:  2548984, 7025483, 7025773

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
pdata <- rpois(1000, 1)

## b) Take a look at your sample using the table() function and histogram and boxplot. 
table(pdata)
hist(pdata, xlim = c(0,6), breaks = 10)
boxplot(pdata)

## c) In what ways does this diverge from a normal distribution?
### Name at least 2 differences in reference to the plots and/or table

# Answer:
# A normal distribution would have its maximum at the datapoints mean. 
# To the right and to the left of the datapoints mean, a normal distribution would 
# decrease in its frequency and eventually approximate a frequency 
# of zero.
# However, the data from the poisson distribution is in its form much closer to
# the right half of a normal distribution.

## d) Now, we are going to draw a smaller sample from our generated data set.
### Use the function sample() to create a sample of five instances from pdata.
### assign it to sample5
sample5 <- sample(pdata, 5)

## e) draw another sample of 5 called sample5b
sample5b <- sample(pdata, 5)

## f) calculate the mean of the two samples and store them in the vector means5
means5 <- c(mean(sample5),mean(sample5b))

## g)   In order to draw a distribution of such a sample, we want to calculate the
###   mean of 1000 samples, each of size 5. However, we don't want to repeat 
###   question e and f 1000 times. Use a for loop to draw 1000 samples of size 5
###   and store the mean of each sample in the vector means5.
means5 <- c()
for (i in 1:1000){
  sample5 <- sample(pdata, 5)
  means5 <- c(means5,mean(sample5))
}

## h) Repeat the for-loop in question h, but use a sample size of 20. 
##    Assign this to 'means50' instead of 'means5'.
means50 <- c()
for (i in 1:1000){
  sample5 <- sample(pdata, 20)
  means50<- c(means50,mean(sample5))
}

## i) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

# means5:
# The vector contains 1000 means of very small glances at pdata. Because the 
# samples only had a size of 5, mean(means5) is just a vague representation of 
# mean(pdata)

# means50:
# The vector contains 1000 means of reasonably sized glances at pdata.
# mean(means50) should be a much better representation of mean(pdata) than 
# means5 is, because for each sample means50 took much more datapoints into
# consideration.

## j) Draw histograms of means5 and means50. Describe in what way they differ
hist(means5, xlim = c(0,6), breaks = 10)
hist(means50, xlim = c(0,6), breaks = 10)

# Answer:
# The values in means50 are much more centered around mean(pdata).
# On the other hand, the values in means5 are much more all over the place.
# That is, because in samples with a size of only 5, extreme values have a much
# bigger influence on the result of mean(sample5). Therefore, the histogram of 
# means5 is less focused around mean(pdata).

## k) Why do you observe a skew for means5, but not for means50?
boxplot(means5)
boxplot(means50)

# Answer:
# This is again because of the influence of extreme values on the mean of
# different sample sizes (see answer for j)). Therefore, it is very unlikely 
# (but not impossible) to not observe a skew in boxplot(means5).

###############################################################
### Exercise 2: Confidence interval
###############################################################

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.


## a) What does a confidence interval mean from the perspective of experiment replication?

# Answer:
# The useful information that Confidence Interval gives about experiment 
# replication is that the the mean of a replication is likely to fall within 
# the Confidence Interval.

## b) please install and load packages sciplot and lsr
install.packages("sciplot")
install.packages("lsr")

library(sciplot)
library(lsr)

## c) calculate 95% Confidence Intervals for pdata, sample5 and sample5b. You can
##    use the function ciMean()
pdata_ci = ciMean(pdata)
print(pdata_ci)
#           2.5%    97.5%
#pdata 0.9634941 1.090506

sample5_ci = ciMean(sample5)
print(sample5_ci)
#             2.5%    97.5%
#sample5 0.4585555 1.441444

sample5b_ci = ciMean(sample5b)
print(sample5b_ci)
#               2.5%    97.5%
# sample5b 0.1611494 2.238851

## d) Why are these intervals so different?

# Answer:
# These intervals are so different because of the difference in sample size which
# causes difference in standard devidation.

## e) Is the true mean contained in the interval?

#Answer:
# Yes, the true mean is contained in the interval


## f) In the following, we will check whether the CI behaves as expected.
### What is the true mean in our example?

# Answer:
# The true mean in our example is 1.027
true_mean = mean(pdata)
print(true_mean)

## g) Change your for loop from above (means20) to calculate the confidence interval 
### insetad of the mean. Then check whether the confidence interval contains the
### true mean and save the result in the variable TrueMeanContained
### Hint: You will need to compare the mean to the lower and the upper bound of the confidence interval
### ciMean(YOURSAMPLE)[1] gives you the lower bound and ciMean(YOURSAMPLE)[2] the upper bound

TrueMeanContained <- c()
for (i in 1:1000){
  sample5 <- sample(pdata, 20)
  ci_mean_sample = ciMean(sample5)
  if ((true_mean > ci_mean_sample[1]) & (true_mean < ci_mean_sample[2])) {
    TrueMeanContained <- c(TrueMeanContained, TRUE)
  } else {
    TrueMeanContained <- c(TrueMeanContained, FALSE)
  }
}
#print(TrueMeanContained)


## h) Given your results in TrueMeanContained, you now need to check, whether the interval really contains
### the mean 95% of the time. Does it?

# Answer:
sum(TrueMeanContained)
# This returns the sum of "TRUE" values in the vector TrueMeanContained. Here we
# Here, we get 950 TRUE out of 1000 times checking with true mean. Thus, this is
# 95% of the time


## i) Confidence intervals are often used in plots. Lets construct a barplot with confidence intervals for
### the dataset chickwts, which contains weight of chickens after being administered different kinds of 
### food for 6 weeks.
### Use the function bargraph.CI to plot weight by feed, using the arguments response and x.factor

data("chickwts")
bargraph.CI( x.factor = feed,
             response = weight,
             data = chickwts,
             xlab = "Feed",
             ylab = "Mean weight"
            )

## j) Now additionally set the optional argument ci.fun to ciMean. How does the graph change and why?
### Hint: Look into the documentation of bargraph.CI.


bargraph.CI( x.factor = feed,
             response = weight,
             data = chickwts,
             ci.fun = ciMean,
             xlab = "Feed",
             ylab = "Mean weight"
)

?bargraph.CI
# Answer: 
# The graph changes because in "i" we haven't mentioned the ci.fun function to 
# be used. Hence the bargraph.CI defaults to the mean +/- 1 standard error.
# Where as, here we specify the ciMean() function for ci.fun. Thus, there is a
# change in the intervals specifications for plotting of the graph.

