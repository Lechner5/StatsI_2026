# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2"),  pkgTest)

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check working directory
getwd()
setwd("C:\\Users\\Alexander\\Documents\\GitHub\\StatsI_2026\\datasets")
# Agenda
# (a.) Correlation
# (b.) Bivariate regression 

# -------------------------------#
# a. Correlation
# -------------------------------#

df <- read.csv("fictional_data.csv")
View(df)

# Create a scatter plot 
ggplot2::ggplot(data = df, 
                aes(x = edu, 
                    y = income)) + 
  geom_point()

plot(df$edu, df$income)


# Calculate correlation
stats::cor(df$edu, df$income)
# Correlation is approx 0.878. We observe a very strong positive linear relationship 


# Add correlation to scatter plot
ggplot2::ggplot(data = df, 
                aes(x = edu, 
                    y = income)) + 
  geom_point() + 
  geom_line()

# Improve visualization with detailed axis and main labels, save as PDF

# Run a t-test for the correlation coefficient
cor.test(df$edu, df$income)
sprintf("%.20f", 7.52e-07) 


# Check p-value

# Step 1: Assumptions

#There is a linear relationship
#Alpha = 0.05

# Step 2: Hypotheses

# H0: There is no relationship between x and y in the population 
# => rho(x, y) = 0

# H1: There is 



# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# -------------------------------#
# b. Bivariate regression
# -------------------------------#

# Question: Is there a relationship between education and income?
# Model: income = b0 + b1 * education
# Save model as object
model1 <- lm(income ~ edu, 
             data  = df)

# Output interpretation:
#  - Intercept (b0)
#  - Slope (b1)
model1
summary(model1)
# Alpha = 976.2. A average Person with no education is estimated to have an income of 976.2 (units?).
# ß1/?edu = 250.6. Accordingly, for every year of additional education, 
# we estimate the income to increase on avergae by 250.6.

# Conduct t-test for the slope of a regression line

# Check p-value

# Create a residual plot
mode.res <- resid(model1)

plot(df$edu, model.res,
     ylab = "Residuals",
     xlab = "University education (in Years")

# Is a homescedastic situation
# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Calculate 95% and 99% confidence intervals around your estimated coefficients
confint(model1, level = 0.95)
confint(model1, level = 0.99)
# Create a scatter plot w/ educate and income, add regression line
plot(df$edu, df$income)
abline(model1)

# What is the prediction equation?

# Make predictions for first observation in df

# Make predictions for a range of x values (specifically min to max of education)

# Add standard errors for your predicted values

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
# Rembmer: more variability in individual responses --> wider intervals

# Make predictions for x values not in data
# (1) Mean education
# (2) Only unique values of x

# Now, plot those predictions in base R

# Plot, regression line with confidence intervals
# First, save confidence intervals
# Second, create scatter plot
# Then, add regression line, lower bound, and upper bound

# And now try with ggplot2


# Add-on: Try it with Education of 15 Years.

predict(model1, newdata = data.frame(edu = c(0:8, 15)), se.fit = TRUE)

        