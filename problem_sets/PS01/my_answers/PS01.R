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

lapply(c("GGally", "tidyverse"),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Sample mean
sample_mean <- mean(y)
# Sample Standard deviation & Standard error
?sd() # Uses n-1 as denominator; Therefore, could be used.
sample_sd <- sd(y)
# We need the n for the Standard error
n <- length(na.omit(y))
standard_error <- sample_sd/sqrt(n)

# As N < 30 one should use the t-Distribution (not Normal Distribution)
alpha = 0.10
degrees_of_freedom = n - 1
# Now, calculate the upper and the lower bound
t_score <- qt(p = alpha / 2,
              df = degrees_of_freedom, 
              lower.tail = FALSE)
# Now we can calculate the lower bound & upper bound
lower_90 <- sample_mean - (t_score * standard_error)
upper_90 <- sample_mean + (t_score * standard_error) 
ci90 <- c(lower_90, upper_90)
ci90

# Part 2

#  Next, the school counselor was curious whether the average student IQ in her school
# is higher than the average IQ score (100) among all the schools in the country

#### Step 1
# Assumption 1: Random Sample
# Assumption 2: Continous Data
# Assumption 3: n >= 30 is violated => Therefore, we use one sample t-test

### Step 2
# H1: Sample > 100
# H0: Sample <= 100
# Alpha = 5 % = 0.05

### Step 3: Calculate a test static

# Option 1: t.test function
?t.test() # Only used the two sample test in the past
t.test(x = y,
       mu = 100,
       alternative = "greater", # "greater" = einseitig nach oben
       conf.level = 0.95        # Konfidenzniveau (standardmäßig 0.95)
)

# Option 2: Do it by hand
# one sample t-test
t_emp = (sample_mean - 100) / standard_error

### Step 4: P-Value

# Option 1:
# It can be read from the output (line 79 anf follwing) 
# It is 0.7215

#Option 2: Print t-emp
t_emp



### Step 5: Make a decision
# Option 1: with the t.test function
# p = 0.7215 > 0.05. Based on this value we can not refute our H0. 

# Option 2: Compare the the empirical t-value with
# the critical value (https://www.tdistributiontable.com/) 
# for 24 df and alpha = 0.05 and a one-tail test is 1.711
# As t_emp < 1.711, we can not reject H0

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

# Option 1. 