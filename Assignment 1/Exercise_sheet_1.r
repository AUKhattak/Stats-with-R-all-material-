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
## #team: #80
## Name: Mohamed Genedy
## Matriculation number: 2580760

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
getwd()

## b) Get help with this function.
help()

## c) Change your working directory to another directory.
setwd("C:/Users/Mohamed/Desktop/R")
###############
### Exercise 2: Normal distribution plotting
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -4 to 4, by 0.1. Assign this to the 
##    variable x.
#assign("x", seq(-4,4,by = 0.1))
#x <- seq(-4,4,by = 0.1)
x = seq(-4,4,by = 0.1)

##print(x)
## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
help(dnorm)
#lib = c(rep("Y",13),rep("N",13))
y <- dnorm(x)

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(y,
     #xaxt = "n", # If I don't want to label the x-axis
    # type = "l", # to plot as a line not dots
     main = "first plot",
     xlab= "x") 

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.6, and plot a line 
##    instead of the circles.

plot(y,
     #xaxt = "n", # If I don't want to label the x-axis
     type = "l", # to plot as a line not dots
     main = "first plot",
     xlab= "x" ,
     ylim = c(0,0.6) )

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.

help(abline)
mean = mean(y)
abline(v = mean, col = "blue", lty = 2)

## f) Take a look at the beaver2 dataset. (You can see it by typing "beaver2".) 
##    Then select only the temperature part and store it in a variable "b2temp".
beaver2
#tail(beaver2) if you want to show the end of the data set
b2temp <- beaver2['temp']
## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
the_mean = mean(b2temp$temp, na.rm = TRUE)
the_sd = sd(b2temp$temp, na.rm = TRUE)

#library(ggplot2)
#ggplot(data = b2temp, aes(x = x)) + 
#  stat_function(fun = dnorm, args = list(mean = mean))

dnormed <- dnorm(b2temp$temp, mean = the_mean, sd= the_sd )

plot(dnormed,
     type = "l", # to plot as a line not dots
     main = "temp plot",
     xlab= "x")

## h) We observe two additional temperature values (38.13 an 36.81). What's the 
##    likelihood that these temperatures (or more extreme ones) respectively 
##    come from the normal distribution from g)?
b2temp <- rbind(b2temp, c(38.13))
b2temp <- rbind(b2temp, c(36.81))
## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. Set the range of the x-axis between 36 to 39 using xlim. 
##    Fix the number of breaks to 10 using breaks
##    What do you observe?

samples = sample(dnormed , size=20)

help(sample)
hist(samples , xlim = c(36,39))
hist(samples)
###############
### Exercise 3: data exploration and more histograms
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")
library(languageR)
## b) Specifically, we will deal with the dataset 'selfPacedReadingHeid'. 
##    This dataset should be available to you once you've loaded languageR.
##    Find out what experiment the data comes from
##    Inspect 'selfPacedReadingHeid'. Look at the head, tail, 
##    and summary. What do head and tail show you?
selfPacedReadingHeid
head(selfPacedReadingHeid)
tail(selfPacedReadingHeid)
summary(selfPacedReadingHeid)
#Head shows the first 6 rows and tail shows the last 6 rows

## c) The file contains multiple observations for each participant. Create a 
##   subset only including subject number PP002 and assign it to PP002.
##   How many observations are there for this participant, i.e. how many rows 
##   are in your subset?
#newdata <- selfPacedReadingHeid[Subject== 'PP002']
newdata <- subset(selfPacedReadingHeid, selfPacedReadingHeid$Subject== 'PP002')
## we have 80 rows


## d) Create a histogram (using hist()) of "RT" (logarithm of reading time) 
##    for PP002

hist(newdata$RT)


## e) Create a kernel density plot for this data using density()
plot(density(newdata$RT),                                    # Modify main title & labels
     main = "My Kernel Density Plot",
     xlab = "X-Values",
     ylab = "Density")

## f) What is the difference between the two?

## g) Is this data likely from a normal distribution? How would you check ?
##    (describe in words, remember to comment out text)



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

## the data is discrete 


## b) The researcher is also interested in whether story telling is related to 
##    their reading habits. As a proxy, she asked the children, whether they have 
##    a library card. The following line codes that the first 13 observations are
##    from children with library card (Y) and the remaining 13 from children 
##    without (N). What measurements scale does this variable have?
lib = c(rep("Y",13),rep("N",13))

## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pps', and your 
##    participants should be labeled from 1 to 26
pps <- seq(1, 26, by=1)   


## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 18, 19, 22, 17, 18, 26, 17, 14, 16, 16, 17, 21, 23, 16, 20, 21, 20, 20, 15, 17, 17, 18, 20, 24)

## e) Create a dataframe including pps, obs and lib. Assign this to 'stories'.
stories <- data.frame(pps, obs, lib)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pps' and 'lib'?

class("stories$lib")
class("stories$pps")
## g) Change the class of 'pps' and 'lib' to factor. Why is factor a better
##     class for these variables? (answer for both separately)
lib_factor <- as.factor(lib)
pps_factor <- as.factor(pps)
class("lib_factor")
class("pps_factor")
stories2 <- data.frame(pps_factor, obs, lib_factor)

## h) Create a boxplot of obs for the two lib groups


boxplot(stories2$lib_factor)

## i) Are there outliers in one of the lib groups?

## j) Which group shows the larger interquartile range? Which one has the 
##    greater overall range?

## k) Which group shows a negative or positive skew?

## l) What is a whisker? Why is the upper whisker of group "Y" so short?

## m) Compare the median of group Y with the mean - which one is plotted in your
##    boxplot? Why are they different?

