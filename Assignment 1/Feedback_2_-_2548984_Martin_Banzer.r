### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 7. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms
## You are required to work together in groups of three students.
## You need to provide a serious attempt at solving each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates') name and matriculation number 
## and your assignment team number on cms.
## #team: 38
## Name:  Martin Banzer, Tulika Nayak         
## Matriculation number: 2548984 , 7025773

## Change the name of the file by adding your matriculation numbers
## (sheet01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## cms discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd() #+

## b) Get help with this function.
help("getwd")#+

## c) Change your working directory to another directory.
setwd("Solutions") #+

###############
### Exercise 2: Normal distribution plotting
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -4 to 4, by 0.1. Assign this to the 
##    variable x.
?seq()
x <- seq(-4,4,0.1) #+

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
help(dnorm)
y <- dnorm(x) #+

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x,y) #+


## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.6, and plot a line 
##    instead of the circles.
plot(x,y,type="l",ylim = c(0,0.6))#+

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(v = mean(x), lty = 2)#+

## f) Take a look at the beaver2 dataset. (You can see it by typing "beaver2".) 
##    Then select only the temperature part and store it in a variable "b2temp".
beaver2 #+-
b2temp <- beaver2[,3]#+

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
mean(b2temp)
sd(b2temp)
plot(b2temp,dnorm(b2temp,mean(b2temp),sd(b2temp)))#+

## h) We observe two additional temperature values (38.13 an 36.81). What's the 
##    likelihood that these temperatures (or more extreme ones) respectively 
##    come from the normal distribution from g)?

# Answer:
# According to the normal distribution of g, temperatures roughly below 37 and above
# 38 become increasingly unlikely. #-

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. Set the range of the x-axis between 36 to 39 using xlim. 
##    Fix the number of breaks to 10 using breaks
##    What do you observe?
samples <- sample(b2temp, size = 20)
hist(samples, xlim = c(36,39), breaks = 10)
samples <- sample(b2temp, size = 20)
hist(samples, xlim = c(36,39), breaks = 10)
samples <- sample(b2temp, size = 20)
hist(samples, xlim = c(36,39), breaks = 10)
samples <- sample(b2temp, size = 20)
hist(samples, xlim = c(36,39), breaks = 10)
samples <- sample(b2temp, size = 20)
hist(samples, xlim = c(36,39), breaks = 10) #+

# Observation:
# The resulting histograms do not always represent the expected normal 
# distribution with a peak at around 37. This is because the gathered 
# samples are taken at random without taking the normal distribution of 
# the dataset into consideration. However, most of the time the histograms
# do represent the expected normal distribution pretty well.#+

###############
### Exercise 3: data exploration and more histograms
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")
library(languageR)#+


## b) Specifically, we will deal with the dataset 'selfPacedReadingHeid'. 
##    This dataset should be available to you once you've loaded languageR.
##    Find out what experiment the data comes from
?selfPacedReadingHeid #+

# Answer:
# Description of selfPacedReadingHeid dataset as per documentation:
# Self-paced reading latencies for Dutch neologisms ending in the suffix -heid #+


##    Inspect 'selfPacedReadingHeid'. Look at the head, tail, 
##    and summary. What do head and tail show you?
str(selfPacedReadingHeid)
head(selfPacedReadingHeid)
tail(selfPacedReadingHeid)#+-

# Answer
# There are 1280 observations of 18 variables
# The head and tail show us the names of 18 columns. The words under the "Word"
# column have the words whose reading latencies are being inspected. Most of the
# columns have numeric data other than "Subject", "Condition" which are Factor data
# type and "Frequency", "BaseFrequency", "LengthInLetters", "NumberOfSynsets" which  
# are int datatype #+-



## c) The file contains multiple observations for each participant. Create a 
##   subset only including subject number PP002 and assign it to PP002.
PP002 <- selfPacedReadingHeid[which(selfPacedReadingHeid$Subject == "PP002"),] #+
PP002


##   How many observations are there for this participant, i.e. how many rows 
##   are in your subset?

# Answer
# There are 80 observations for participant PP002, since 80 rows are displayed for
# the data observations having Subject equal to PP002 #-



## d) Create a histogram (using hist()) of "RT" (logarithm of reading time) 
##    for PP002
hist(PP002$RT)#+

## e) Create a kernel density plot for this data using density()
den_data <- density(PP002$RT)
plot(den_data)#+

## f) What is the difference between the two?

# Answer
# Histogram uses bins/bars to visualise the continuous data here. Each bar 
# represents the frequency at each interval/bin. But, Kernel Density Plot uses
# Kernel smoothing to plot the same values. Thus it is not affected by bins
# and hence determines the distribution shape, much better. The difference is 
# also clearly visible in the above plots.#+

## g) Is this data likely from a normal distribution? How would you check ?
##    (describe in words, remember to comment out text)

# Answer
# The Kernel density plot gives an almost "bell curve" which is almost symmetric
# about the mean. In reality, normal data does show some deviation hence the bell 
# curve doesn't need to be completely symmetrical (Although Normal data is different 
# from Symmetrical data). For further checking we could use a QQ-plot #+



###############
### Exercise 4: Dataframes and boxplots
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 26 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 18 19 22 17 18 26 17 14 16 16 17 21 23 16 20 21 20 20 15 17 17 18 20 24


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? 

# Answer:
# It is a discrete ratio scale, since there could be a natural zero within the 
# numeric data and since the amount of "and then" must always be in whole numbers.#+

## b) The researcher is also interested in whether story telling is related to 
##    their reading habits. As a proxy, she asked the children, whether they have 
##    a library card. The following line codes that the first 13 observations are
##    from children with library card (Y) and the remaining 13 from children 
##    without (N). What measurements scale does this variable have?
lib = c(rep("Y",13),rep("N",13))

# Answer:
# 'lib' has a nominal scale. There is no particular relationship between "Y" and "N".
# Therefore, 'lib' has no real order as well. #+


## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pps', and your 
##    participants should be labeled from 1 to 26
pps <- seq(1,26,1)#+


## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 18, 19, 22, 17, 18, 26, 17, 14, 16, 16, 17, 
         21, 23, 16, 20, 21, 20, 20, 15, 17, 17, 18, 20, 24)#+


## e) Create a dataframe including pps, obs and lib. Assign this to 'stories'.
stories <- data.frame(pps, obs, lib)#+


## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pps' and 'lib'?

summary(stories)

#Answer:
#Variable 'pps' is of class: num and 'lib' is of class: character #+


## g) Change the class of 'pps' and 'lib' to factor. Why is factor a better
##     class for these variables? (answer for both separately)
stories$pps <- as.factor(stories$pps)
stories$lib <- as.factor(stories$lib)
str(stories)

#Output for str(stories)
#'data.frame':	26 obs. of  3 variables:
# $ pps: Factor w/ 26 levels "1","10","11",..: 1 12 20 21 22 23 24 25 26 2 ...
# $ obs: num  18 15 18 19 22 17 18 26 17 14 ...
# $ lib: Factor w/ 2 levels "N","Y": 2 2 2 2 2 2 2 2 2 2 ...

# 'pps' has numbers. So when we try to analyse the data using mathematical models,
# or graph plots, the output could depend on the value of 'pps'. Thus factor is a 
# much better class for 'pps' as it wouldn't affect the output if there's a change 
# in the value of the variable.

# Factor is a better class for 'lib' because it has only 2 possible values that is 
# Y or N. Hence these two values can be treated as possible levels and categories of
# the variable 'lib'#+




## h) Create a boxplot of obs for the two lib groups
boxplot(obs~lib) #+

## i) Are there outliers in one of the lib groups?

# Answer:
# Yes, the "Y" group has two outliers (22 and 26). #+

## j) Which group shows the larger interquartile range? Which one has the 
##    greater overall range?

# Answer:
# Group "N" has the larger interquartile range (roughly 21-17 = 4).
# Group "N" has the greater overall range (roughly 24-15 = 9).#+-

## k) Which group shows a negative or positive skew?

# Answer:
# Group "N" shows a negative skew.
# Group "Y" shows a normal skew.#+
 
## l) What is a whisker? Why is the upper whisker of group "Y" so short?

# Answer:
# The whiskers show the variability above and below the third and first 
# quartile of a boxplot.
# The upper whisker of group "Y" is so short, because the range of samples that
# are overstepping the third quartile is very small.#+-

## m) Compare the median of group Y with the mean - which one is plotted in your
##    boxplot? Why are they different?
mean(obs)

# Answer:
# Median is plotted in the boxplot. The thick line in the boxplot indicates the 
# median of each group. In the plot, Group 'Y' has different mean and median 
# because its data is not normally or symmetrically distributed #+-
