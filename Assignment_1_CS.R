#Assignment 1
#Christina Simms 

#Question 1

AvengersData <- read.csv("avengers.csv")

#Here in Question 1 I have read in the data set from the cloned repo and saved it as 'AvengersData'. 

#Question 2

install.packages("tidyverse")
library(tidyverse)

AvengersDataClean <- AvengersData %>%
  filter(complete.cases(.))

AvengersDataClean %>%
  summarise(across(everything(), ~ sum(is.na(.))))

#Here in Question 2 I have first installed and loaded the tidyverse packages, then I went on to create a new object
#called 'AvengersDataClean' to store the new cleaned data in. After reviewing our lecture materials, I decided that
#instead of removing/checking for missing data in each variable separately, which is what we did in our lab, I would
#find an alternate option where one line of code (actually two) could remove all missing cases from the entire data set. 
#Using RStudio's online material, I used the pipe function and complete.cases function to filter out any missing cases. 
#To confirm that all missing data was removed, I used the summarise function that lets us know how many missing cases 
#are in each column. Each column came up as '0', indicating that there are no missing casses in that column or variable.

AvengersDataClean <-AvengersDataClean %>%
  mutate(CombatEffectiveness = agility + speed + strength + willpower)

summary(AvengersDataClean$CombatEffectiveness)

#Continuing Question 2, I have created a new column for the AvengersDataClean data set called 'CombatEffectiveness'
#which is the sum of the variables agility, speed, strength, and willpower. I used the mutate function to do this. I then
#used the 'summary' function to confirm that the new variable was created and properly summed the data of the four 
#variables of interest. 

AvengersData %>%
  #summarise(N = n())

AvengersDataClean %>%
  summarise(N = n())
#Here I also ran some additional code to get some descriptives to include in my report. 

#Question 3 

AvengersSubset <- AvengersDataClean %>%
  filter(superpower == "no",
         died =="yes")

AvengersSubset %>%
  summarise(N = n())

#For the first part of question three I have created a new object/data set 'AvengersSubset' and used the pipe function to 
#include only data for those who have died that did not have a superpower. 

AvengersSubset %>%
  summarise(N = n())

AvengersSubset %>%
  count(north_south) %>%
  mutate(percent = n / sum(n) * 100)

#Once again, please ignore this code, I am just using it to get some descriptives of the subsample we are looking at here. 

write.csv(AvengersSubset,
          "AvengersSubset.csv",
          row.names = FALSE)

install.packages("haven")
library(haven)

write_sav(AvengersSubset, "AvengersSubset.sav")

# I have saved this new dataset in both .csv and .sav formats above. To save in .sav format without losing any
#information (i.e., variable labels, missing data structure) I used the 'haven' package, which converts the file to open
#correctly in SPSS. 

AvengersSubset %>%
  summarise(
    across(c(CombatEffectiveness, kills, injuries),
           list(
             Mean = mean,
             SD = sd,
             Min = min,
             Max = max
           )))

AvengersSubset %>%
  group_by(north_south) %>%
  summarise(
    across(c(CombatEffectiveness, kills, injuries),
           list(
             Mean = mean,
             SD = sd,
             Min = min,
             Max = max,
             Range = ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)
           )))

SummaryLocation <- AvengersSubset %>%
  group_by(north_south) %>%
  summarise(
    across(c(CombatEffectiveness, kills, injuries),
           list(
             Mean = mean,
             SD = sd,
             Min = min,
             Max = max,
             Range = ~ max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)
           )))

view(SummaryLocation)
# Here you can see the coding for a general summary of CombatEffectiveness, kill, and injuries, as well as a grouped 
# summary depending on location. I used the pipe and across function to summarize the data in a neat singular output/tibble.
# In order to view the entire tibble, stored the grouped summary coding into a new object 'SummaryLocation', then used
# the view function to observe all output. 

#Question 4

# See report.

#Question 5

AvengersSubset <- AvengersSubset %>%
  mutate(AvengersSubMeanTotal = rowMeans(select(., CombatEffectiveness, kills, injuries),
                                         na.rm = TRUE))

# Initially I created a combined or total mean variable across the three variables we are measuring, using the
#mutate function. Doing this is setting the code up to "drop" each of the variables one at a time to see which has
#the least impact on the combined/total mean.

TotalMean <- mean(AvengersSubset$AvengersSubMeanTotal)
TotalMean

#I now have a combined/total mean value of 168.2146. 

AvengersSubset <- AvengersSubset %>%
  mutate(MeanRemovedCE = rowMeans(select (.,kills, injuries),
                                  na.rm = TRUE))
MeanRemovedCE <-mean(AvengersSubset$MeanRemovedCE)
MeanRemovedCE

# I now have a combined/total mean score of 3.554455 after removing the CombatEffectiveness variable.

AvengersSubset <- AvengersSubset %>%
  mutate(MeanRemovedKills = rowMeans(select (.,CombatEffectiveness, injuries),
                                     na.rm = TRUE))
MeanRemovedKills <-mean(AvengersSubset$MeanRemovedKills)
MeanRemovedKills

# I now have a combined/total mean score of 251.0446 after removing the kills variable.

AvengersSubset <- AvengersSubset %>%
  mutate(MeanRemovedInjuries = rowMeans(select (.,CombatEffectiveness, kills),
                                        na.rm = TRUE))
MeanRemovedInjuries <-mean(AvengersSubset$MeanRemovedInjuries)
MeanRemovedInjuries

# I now have a combined/total mean score of 250.0446 after removing the injuries variable.

abs(TotalMean - MeanRemovedCE) #164.6601
abs(TotalMean - MeanRemovedKills) #82.83005
abs(TotalMean - MeanRemovedInjuries) #81.83005

#Now I am using the absolute value function to compare the mean difference when each variable is individually
#removed. 

#Question 6
#See report. 

#Question 7
#See report. 

#Question 8

install.packages("pwr")
library(pwr)

pwr.t.test(
  d = 0.50,
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample",
  alternative = "two.sided")

# Here you can see the power analysis that was used to compare group differences between IQ scores in Avengers with 
#and without superpowers. For justification of parameter selection please see question 8 in the report. 

#Question 9
#See report. 

#Question 10

AvengersDataClean %>%
  count(superpower)

#Here I am confirming that the whole collected sample has enough participants (or Avengers) in each group (with out
#superpowers) to pass the power analysis.

install.packages("effectsize")
library(effectsize)

#Here I am just installing the packages needed for this analysis. 

library(metafor)

IQ_Superpower <- escalc(
  measure = "SMD",
  ti = 4.25,      
  n1i = 32,      
  n2i = 780,      
  var.names = c("dp","variance")
)

summary(IQ_Superpower)



