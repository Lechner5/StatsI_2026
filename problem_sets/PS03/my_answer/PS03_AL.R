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
lapply("tidyverse", pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc_sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")
colSums(is.na(inc_sub)) # No missing Values
### Question 1 ###
# We are interested in knowing how the difference in campaign spending between incumbent
# and challenger affects the incumbent’s vote share.

# 1. Run a regression where the outcome variable is voteshare and the explanatory variable
# is difflog.
model1 <- lm(voteshare ~ difflog, data = inc_sub)
summary(model1)


# 2. Make a scatterplot of the two variables and add the regression line
plot1 <- ggplot2::ggplot(data = inc_sub,
           aes(x = difflog,
               y = voteshare)) + 
  geom_point(alpha = 0.5) + 
  # Looked it up in StackOverflow
  geom_smooth(method='lm') + 
  # Theme
  theme_minimal() +
  # Labels
  labs(title = "Campaign Spending and Vote Share",
       x = "Difference in Campaign Spending (log)",
       y = "Vote Share")

ggplot2::ggsave("plot1.pdf",
                plot1,
                units = "in",
                width = 7,
                height = 5)

# 3. Save the residuals of the model in a separate object
errors_model1 <- model1$residuals
summary(errors_model1)

# 4. Write the prediction equation.

# votesharehat = alpha + difflog*ß1
# If we wanted to use R:
# yhat <- model1$coefficients[1] + model1$coefficients[2]*inc_sub$difflog
# For the concrete Values, see Part 1 of the question


##### Question 2 ###
# We are interested in knowing how the difference between incumbent and challenger’s spending
# and the vote share of the presidential candidate of the incumbent’s party are related.

# 1. Run a regression where the outcome variable is presvote and the explanatory variable
# is difflog
model2 <- lm(presvote ~ difflog, data = inc_sub)
summary(model2)

# 2. Make a scatterplot of the two variables and add the regression line.
plot2 <- ggplot2::ggplot(data = inc_sub, aes(x = difflog,y = presvote)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  # Theme
  theme_minimal() +
  # Labels
  labs(title = "Campaign Spending and Presidental Candidates Success",
       x = "Difference in Campaign Spending (log)",
       y = "Vote Share Presidental Candidate")



ggplot2::ggsave("plot2.pdf",
                plot2,
                units = "in",
                width = 7,
                height = 5)

# 3. Save the residuals of the model in a separate object.
errors_model2 <- model2$residuals

# 4. Write the prediction equation.
# Yhat = alpha + difflog*ß1
yhat2 <- model2$coefficients[1] + model2$coefficients[2]*inc_sub$difflog

### Question 3 ###
# We are interested in knowing how the vote share of the presidential candidate of the
# incumbent’s party is associated with the incumbent’s electoral success.

# 1. Run a regression where the outcome variable is voteshare and the explanatory variable
# is presvote.
model3 <- lm(voteshare ~ presvote, data = inc_sub)
summary(model3)

# 2. Make a scatterplot of the two variables and add the regression line
plot3 <- ggplot2::ggplot(data = inc_sub, 
                aes(x = presvote,
                    y = voteshare)) +
# Use Alpha to visually distinguish between the points
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
# Theme
theme_minimal() +
  # Labels
  labs(title = "Presidental Votes and Vote Share",
       x = "Presidental Votes",
       y = "Vote Share")

ggplot2::ggsave("plot3.pdf",
                plot3,
                units = "in",
                width = 7,
                height = 5)

# 3. Write the prediction equation.
# votesharehat = alpha + presvote*ß1
# If we want to use R to calculate predictions for every case:
# yhat3 <- model3$coefficients[1] + model3$coefficients[2] * inc_sub$presvote
# For the concrete Values, see Part 1 of the question

### Question 4 ### 
# The residuals from part (a) tell us how much of the variation in voteshare is not explained
# by the difference in spending between incumbent and challenger. The residuals in part (b)
# tell us how much of the variation in presvote is not explained by the difference in spending
# between incumbent and challenger in the district

# 1. Run a regression where the outcome variable is the residuals from Question 1 and the
# explanatory variable is the residuals from Question 2.

error_data <- data.frame(errors_model1, errors_model2)
model4 <- lm(errors_model1 ~ errors_model2, data = error_data)
summary(model4)

# 2. Make a scatterplot of the two residuals and add the regression line.
plot4 <- ggplot2::ggplot(data = error_data,
                aes(y = errors_model1, x = errors_model2)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm")
# Theme
  theme_minimal() +
  # Labels
  labs(title = "Residual Regression", 
       x = "Residual Model 2 (Presvote ~ difflog)",
       y = "Residual Model 1 (Voteshare ~ difflog)")

ggplot2::ggsave("plot4.pdf",
                plot4,
                units = "in",
                width = 7,
                height = 5)

# 3. Write the prediction equation.
#predicted error model 1 = alpha + predicted error model 2 * ß1
# Equals:
# Unexplained Variance in Voteshare = alpha + unexplained variance in Presvote * ß1

### Question 5 ###
#What if the incumbent’s vote share is affected by both the president’s popularity and the
# difference in spending between incumbent and challenger?

# 1. Run a regression where the outcome variable is the incumbent’s voteshare and the
# explanatory variables are difflog and presvote.
model5 <- lm(voteshare ~ presvote + difflog, data = inc_sub)
summary(model5)


# 2. Write the prediction equation.
# predicted voteshare = alpha + presvote * ß1 + difflog * ß2

# 3. What is it in this output that is identical to the output in Question 4?
# Why do you think this is the case?

# The effect of presvote is 0.256, 
# which is the same as the effect of the residuals of model 2 on the residuals of model 1 in Q4
# This is because a multivariate regression uses the part of a explanatory variable that is
# unexplained by the other explanatory variable. Therefore, we mathematically do the same thing. 


# Sources: 
  # https://stackoverflow.com/questions/15633714/adding-a-regression-line-on-a-ggplot