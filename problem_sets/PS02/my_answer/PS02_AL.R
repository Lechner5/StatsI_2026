#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse"),  pkgTest)

#####################
# Problem 1
#####################

experiment <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
expected_frequency <- matrix(c(
  sum(experiment[1, ]) * sum(experiment[, 1])/sum(experiment),
  sum(experiment[1, ]) * sum(experiment[, 2])/sum(experiment),
  sum(experiment[1, ]) * sum(experiment[, 3])/sum(experiment),
  sum(experiment[2, ]) * sum(experiment[, 1])/sum(experiment),
  sum(experiment[2, ]) * sum(experiment[, 2])/sum(experiment),
  sum(experiment[2, ]) * sum(experiment[, 3])/sum(experiment)
), nrow = 2, byrow = TRUE)
expected_frequency


chi_squared <- (experiment - expected_frequency)**2 / expected_frequency
chi_squared 
sum(chi_squared) # Chi-squared has a value of 3.791168
chi_sum <- sum(chi_squared)
chi_sum
# b

pchisq(chi_sum, 
       df = (nrow(experiment)-1)*(ncol(experiment)-1),
       lower.tail=FALSE)

# The probability for a chi squared value that is as big as our empirical chi² or bigger 
# is 0.1502. Therefore, we can not reject our H0 based on an alpha level of 0.1.

# c) standardizied residuals

# Row and Column Probabilities                                                                                                                  
p_row = rowSums(experiment) / sum(experiment)
p_column = colSums(experiment) / sum(experiment)

p_row_matrix <- cbind(p_row, p_row, p_row)
p_column_matrix <- rbind(p_column, p_column)
p_row_matrix
p_column_matrix 

# d

# 4. Pearson-Residuen berechnen
pearson_res <- (experiment - expected_frequency) / sqrt(expected_frequency * (1 - p_row_matrix) * (1 - p_column_matrix))
pearson_res
         

#####################
# Problem 2
#####################

# Question 2: Economics
# Chattopadhyay and Duflo were interested in whether women promote different policies than men.
# Answering this question with observational data is pretty difficult due to potential
# confounding problems 
# (e.g. the districts that choose female politicians are likely to systematically differ in other aspects too). 

# Hence, they exploit a randomized policy experiment in India, where since the mid-1990s, 1
# of village council heads have been randomly reserved for women. 


# A subset of the data from West Bengal can be found at the following link:

df_india <- readr::read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# Each observation in the data set represents a village and there are two villages associated
# with one GP (i.e. a level of government is called ”GP”).

#  The authors hypothesize that female politicians are more likely to support policies female voters want.
# Researchers found that more women complain about the quality of drinking water than men. 
# You need to estimate the effect of the reservation policy on the number of 
# new or repaired drinking water facilities in the villages

# (a) State a null and alternative (two-tailed) hypothesis

# H0: The number of of water facilities is not different between villages with reserved seats on the GP level and 
# villages without reserved seats at the GP level and 
# GP's without reserved seats

# H1:  The number of of water facilities is different between villages with reserved seats at the GP level and 
# villages without reserved seats at the GP level

# (b) Run a bivariate regression to test this hypothesis in R (include your code!)
model_india <- lm(data = df_india,
                  water ~ as.factor(reserved))
summary(model_india)

# Should be aquivalent to a t-test 
# Unabhängiger t-Test (Standardfall für zwei Gruppen)
t.test(water ~ reserved, 
       data = df_india, 
       # To have same assumptions as Regression (GEMINI)
       var.equal = TRUE)

# The t value of 2.344 is bigger than 1.96 and the p-value of 0.0197 is smaller than 
# the commonly used 0.05. Therefore, we reject the H0. 

# (c) Interpret the coefficient estimate for reservation policy.

# Villages in GP's with a reserved seat on average have 9 additional water facilities repaired or installed 
# since the policy of reserved seats started
