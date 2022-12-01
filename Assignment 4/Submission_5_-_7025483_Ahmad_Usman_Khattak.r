### Stats with R Exercise sheet 4

##########################
# Week 5: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 28. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via cms

## Please write below your (and your teammates') name, matriculation number. 
## Name:                Martin Banzer,  Ahmad Usman Khattak,  Tulika Nayak
## Matriculation number:2548984,        7025483,              7025773

###########################################################################################
###########################################################################################

#####################################################
### 1. Restructuring, plotting, and t tests
#####################################################
library(lsr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(languageR)

## We will be working with the dataset lexdec from the package languageR
## In short, this data set contains reaction times from an experiment, where participants had 
## to decide whether something is a word or not (nonword). Only responses for real words are
## included and there were 79 measurements per participant.
## 
## Variables we will be interested in include 
## Subject (code for a participant)
## NativeLanguage (native language of participant)
## RT (log reaction time)
## Sex (of the participant)
## PrevType (whether the preceding word was a real word or a nonword)
## Class (whether the target word was denoting an animal or a plant)


## 1. Create the dataset lexdat, which is a copy of lexdec, but only includes the columns 
##  indicated above
data("lexdec")
str(lexdec)
lexdat <- lexdec[c("Subject", "NativeLanguage", "RT", "Sex", "PrevType", "Class")]
#str(lexdat)


## Say you are interested in the influence of the previous word type on lexical decision time.
## Before we start testing, we want to get an impression of the data and create a barplot of 
## the mean by prevType, including error bars that show the 95% CI.
## Here, we define a function to calculate the standard error, which is needed for the CI.
## (just execute the next line, as you will need the function in 2.)
se = function(x){sd(x)/sqrt(length(x))}


## 2. To start, we need to summarize the data. Use the functions group_by() in combination with
##  summarise(). In particular, you need to group by prevType and get the mean as well as the
##  se of RT. Store the result to summaryByPrevType
##  You will find examples of how the summarizing can be done here:
##  https://datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function
sum(is.na(lexdat$RT))
summaryByPrevType <- lexdat %>%
  group_by(PrevType) %>%
  summarise(mean_rt = mean(RT),
            se_rt = se(RT))


## 3. Describe the resulting data set (summaryByPrevType) in your own words
summaryByPrevType

#  PrevType mean_rt   se_rt
#  <fct>      <dbl>   <dbl>
#1 nonword     6.42 0.00829
#2 word        6.35 0.00833
#
#summaryByPrevType represents the mean and standard error of Reaction Time(RT)
#for identifying something (stimulus) presented to the participant right after 
#they have just encountered a "nonword" or a "word", which is given by PrevType.


## 4. Now use summaryByPrevType to create the barplot with error bars denoting the 95% CI
##  (i.e. mean +/-1.96 * se)
ggplot(summaryByPrevType, aes(x = PrevType, y = mean_rt)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_rt - 1.96*se_rt, ymax = mean_rt + 1.96*se_rt), width = 0.1) +
  geom_text(aes(label = round(mean_rt, 2)), size = 4, vjust = -0.5)


## 5. The barplot always starts at zero, which makes the portion of the graph, we are most 
##  interested in (i.e. the spread of the error bars) hard to perceive. As an alternative,
##  construct a line plot of the same data, again including error bars.
##  Hint: if you get a complaint, try to add group = 1 to your aes
ggplot(summaryByPrevType, aes(x = PrevType, y = mean_rt, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_rt - 1.96 * se_rt, ymax = mean_rt + 1.96 * se_rt), width = 0.2, color = "blue") +
  geom_text(aes(label = round(mean_rt, 2)), size = 4, vjust = -4)


## 6. Gauging from the plot, does it look like there's an important difference in mean RT 
##  after words compared to nonwords?

#Answer:
#Yes, there is a significant difference in the mean RT for after "word" and for
#after "nonword"


## 7. Let's go back to the original data frame "lexdat".
##  Now that you've taken a look at the data, you want to get into the stats.
##  You want to compute a t-test for the average RT after words vs nonwords.
##  Why can't you compute a t-test on the data as they are now? 
##  Hint: Which assumption is violated?
lexdat

#Answer:
#A simple display of the lexdat dataframe shows that there are 79 observations 
#for A1 alone. This means the data is a product of repeated measures. This 
#violates the independence assumption. 


## 8. We need to restructure the data to only one observation (average RT) per subject 
##  and word/nonword condition (PrevType). We will again use group_by and summarize, but
##  this time we have to group by Subject and PrevType, while we only need the mean to be 
##  stored, not the se. Assign the result to bySubj
bySubj <- lexdat %>%
  group_by(Subject, PrevType) %>%
  summarise(mean_rt = mean(RT))
bySubj


## 9. Create histograms of the RT data in bySubj depending on the preceding word 
##  type and display them side by side. Set the binwidth to 0.08
ggplot(bySubj, aes(x = mean_rt)) +
  geom_histogram(color = "black", fill = "skyblue", binwidth = 0.08)


## 10. Display the same data in density plots. 
mean_rt_density <- density(bySubj$mean_rt)
plot(mean_rt_density)
polygon(mean_rt_density, col = "skyblue")


## 11. Based on the histograms and the density plots - are these data normally 
##  distributed?

#Answer:
#Yes, the data is normally distributed

## 12. Create boxplots of the mean RT in bySubj by PrevType
boxplot(mean_rt~PrevType, data = bySubj)


## 13. Compute the t-test to compare the mean RT between decisions following on a word
##  vs a nonword using the data in bySubj.
##  Do you need a paired t-test or independent sample t-test? why?
nonword <- subset(bySubj, bySubj$PrevType == "nonword")
word <- subset(bySubj, bySubj$PrevType == "word")
t.test(nonword$mean_rt, word$mean_rt, paired = TRUE)

# Answer:
# We need a paired t-test, because we have measured each condition once with each 
# subject. And we are interested in the within-subject variability.

## 14. What does the output tell you? What conclusions do you draw?

# Answer:
# The output tells us, that there is a statistical difference between the mean_rt after
# a nonword and the mean_rt after a word (-> p-value < 0.05).
# Therefore, the subjects reaction times must have been influenced by what they had heared.

## 15. In addition to the long-format data we've just been working on, you may also 
## encounter data sets in a wide format (this is the format we have been using in 
## class examples.)
## Let's look at a different variable, namely the semantic class (Class) of the target 
## word instead of the type of the previous word. Again, summarize the dataset
## to obtain the mean RT by subject and class and transform the dataset to a 
## wide format. In addition to group_by() and summarize(), you will need the function 
## spread(). Assign the result to wide
wide <- lexdat %>%
  group_by(Subject, Class) %>%
  summarise(mean_rt = mean(RT)) %>%
  spread(Class,Subject)

## 16. Compute a t-test on the wide format data - note that for wide-format 
##  data you need to use a different syntax inside t.test()
animal <- subset(wide, wide$animal != "NA")
plant <- subset(wide, wide$plant != "NA")
t.test(animal$mean_rt, plant$mean_rt, paired = TRUE)

## 17. What do you conclude from this?

# Answer:
# It tells us, that the class of the heared word did not influence the mean_rt 
# of the subjects, becasue the p_value is > 0.05.

## 18. Now let's look at yet another question, namely whether the native language 
##  of the participant influences their reaction time. Check out the variable
##  NativeLanguage. Can you use a t-test to pursue this question and which type
##  of t-test would you use? Can you think of a situation, where a t-test would not 
##  be enough to test for a difference depending on the native language?

# Answer:
# Since "NativeLanguage" only contains the values "English" and "Other", we can't really say
# anything about the specific languages and their influence on the reactiontimes.
# We can only compare english to other languages.
# Because each subject only has one native language, we can't use a paired t-test.
# And since we can't be sure, that both samples have the same variance, we can't
# use an independent samples t-test.
# But we can use a Welch Two Sample t-test.

## 19. Use again group_by and summarize to obtain by subject means of RT, but
## this time with regard to NativeLanguage and assign it to bySubjNatLang
## Perform the t-test you decided for.
bySubjNatLang <- lexdat %>%
  group_by(Subject, NativeLanguage) %>%
  summarise(mean_rt = mean(RT))
english <- subset(bySubjNatLang, bySubjNatLang$NativeLanguage == "English")
other <- subset(bySubjNatLang, bySubjNatLang$NativeLanguage == "Other")
t.test(english$mean_rt, other$mean_rt, paired = FALSE)

## 20. What do you conclude?

# Answer:
# Since the p-value < 0.05 we know, that the subjects native language must
# have an influence on the reaction time.

## 21. Compute the effect size using Cohen's D.
cohensD(english$mean_rt, other$mean_rt)

## 22.  Which effect size do we get? How do you interpret this result?

# Answer:
# We get a large effect size, which should also be fairly interesting to look into, 
# because "Other" might have also included languages, that do not influence the reaction times
# that much. This lack of influence might have been balanced out by other languages, which 
# do influence the reaction times a lot.

## 23. Choose an appropriate plot to visualize the difference between group

boxplot(mean_rt~NativeLanguage, data = bySubjNatLang)


###############
### 2. T-Test
###############
## In this exercise we will try to explore the independent samples t-test 
## and its affect on different samples. 
## We will take the same example discussed in the lecture. A class has two tutors, and we want 
## to find out which tutor is better by comparing the performance of the students in the final 
## exam by tutor group. First set a seed to make sure your results can be reproduced

set.seed(8254)
## 1. Generate 15 samples from a normal distribution with mean 20 and sd 8 and save it in a variable 
##  called "tutor1_grades"
tutor1_grades <- rnorm(15,20,8)

## 2. Now we generate our second sample of size 15, this time for tutor 2 and with mean 35 and 
## sd 15
tutor2_grades <- rnorm(15,35,15)

## 3. Combine the two samples and store the result into one vector called "score" (it should 
##    first show all scores from tutor1 followed by the scores of tutor2)
score <- c(tutor1_grades, tutor2_grades)

## 4. Create a vector called tutor indicating which tutor the score belongs to: it should show 
##   "tutor1" 15 times followed by "tutor2" 15 times
tutor <- factor(rep(c("tutor1", "tutor2"), times=c(15,15)))

## 5. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.
data_frame <- data.frame(tutor, score)

## 6. Run the independent samples TTest (independentSamplesTTest()) and formulate the findings as discussed 
###  in the lecture. 
##	independentSamplesTTest() also provides the effect size (Cohen's d). How do you interpret the effect size?
ttest <- independentSamplesTTest(score~tutor, data_frame, var.equal=TRUE)
ttest

## 7. Time to play around!
##	repeat the whole experiment you performed above with different sample size, mean and standard deviation  
##	repeat it 3 times changing all the values (sample size, mean, sd) and formulate the findings.  
##	what do you observe when we keep the means and sd same?

samplesizes <- c(15,20,25)
mean1 <- c(20,35,40)
mean2 <- c(35,35,15)
sd1 <- c(8,10,17)
sd2 <- c(15,10,20)
for (i in 1:3){
  tutor1_grades <- rnorm(samplesizes[i],mean1[i],sd1[i])
  tutor2_grades <- rnorm(samplesizes[i],mean2[i],sd2[i])
  score <- c(tutor1_grades, tutor2_grades)
  tutor <- factor(rep(c("tutor1", "tutor2"), times=c(samplesizes[i],samplesizes[i])))
  data_frame <- data.frame(tutor, score)
  ttest <- independentSamplesTTest(score~tutor, data_frame, var.equal=TRUE)
  print(ttest)
}

##in the second iteration of the if loop, the means and the sds are the same for tutor1_grades and for tutor2_grades
##this results in a much higher p-value. This high p-value would lead to sticking to the null hypothesis
##however, if the means and sds are different (loop 1 and 3) we would discard the null hypothesis
##keeping the means and sds same also influences the effect size in a big way
##in loop 2 the effect size is only 0.08 while in the other loops it is at least 1.2 (my random results)
##this tells us, that there is no strong relationship between the two variables in loop 2
##but in the loops 1 and 3, the relationship between the two variables is pretty strong