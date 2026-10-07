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

lapply(c("GGally", "tidyverse", "patchwork"),  pkgTest)

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
       # Alternative = "greater" for a one-sided test.
       alternative = "greater"
       conf.level = 0.95)        


# Option 2: Do it by hand
# calculate the empirical t-value to compare it to the critical value
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


# Part 1:

# Please plot the relationships among Y, X1, X2, and X3? What are the correlations
# among them (you just need to describe the graph and the relationships among them)?

# Option 1: Pair Plot

#################
# I knew there is some plot to present all the information at once,
# and had even an example from a lecture during my masters degree. 
# - which was based on "An Introduction into Statistical Learning with R" (James et al., 2013)
# I used the example to put these 2 prompts into Gemini:
## Prompt 1
# Based on an image I had, I put the following prompt into Gemini:
# How can I create this kind of plot in R (with a plot attached)

# Prompt 2: I asked Gemini: How can i Adjust the names shown 

# I received an example of ggally code adjusted the code Gemini put out,
# for the example, to fit the use case.
###############

# Select the numeric columns in the order shown in the image
vars <- expenditure[, 2:5]

# Create the Plot the ggally way
GGally::ggpairs(vars)
ggsave("pair_plot.pdf",
       plot = pair_plot,
       unit = "in",
       height = 5,
       width = 8)

# Option 2:
# Create 6 different Plots 
# 1: Y and X1
corr_plot1 <- ggplot2::ggplot(
  data = expenditure, 
  aes(x = X1,
      y = Y)) +
  geom_point() + 
  labs(title = "Peronal Income and Expenditure on Shelters",
       y = "Expenditure on Shelters",
       x = "Personal Income in State")

# 2: Y and X2 
corr_plot2 <- ggplot2::ggplot(
  data = expenditure, 
  aes(x = X2,
      y = Y)) +
  geom_point() + 
  labs(title = "Financial Insecurity and Expenditure on Shelters",
       y = "Expenditure on Shelters",
       x = "Financial Insecure Residents per 100k")

# 3: Y and x3
corr_plot3 <- ggplot2::ggplot(
  data = expenditure, 
  aes(y = Y, 
      x = X3)) +
    geom_point() +
    labs(title = "Urban Residents and Spending on Shelters",
         x = "‰ Residing in Urban Areas",
         y = "Expenditure on Shelters")
  
# 4: X1 and X2  
corr_plot4 <- ggplot2::ggplot(
    data = expenditure, 
    aes(x = X1, 
        y = X2)) +
    geom_point() +
    labs(title = "Financial Insecurities and Personal Income ",
         y = "Personal Income in State",
         x = "Financial insecure Residents per 100k")

# 5: x1 and x3
corr_plot5 <- ggplot2::ggplot(
    data = expenditure, 
    aes(y = X1, 
        x = X3)) +
    geom_point() +
    labs(title = "Personal Income and Urban Residents",
         x = "Personal Income in State",
         y = "‰ Residing in Urban Areas") 
  
# 6: x2 and x3
corr_plot6 <- ggplot2::ggplot(
    data = expenditure, 
    aes(y = X2, 
        x = X3)) +
    geom_point() +
    labs(title = "Financial Insecurites and Urban Residents",
         x = "Financial Insecure Residents per 100k",
         y = "‰ Residing in Urban Areas")

####
# I knew there is the layout() function to combine different plots.
# However, I looked in the documentation, and it (seemingly) only works for base R plots
# Therefore, I prompted Gemini:
# How can I use something equivalent to layout with ggplot plots?

plot_combined <- (corr_plot1|corr_plot2) / (corr_plot3|corr_plot4) / (corr_plot5|corr_plot6)
ggsave("plot_combined.pdf",
       plot = plot_combined,
       unit = "in",
       height = 10,
       width = 8)

### Question 2, Part 2
# Please plot the relationship between Y and Region? On average, which region has the
# highest per capita expenditure on housing assistance?

# Plotting Region(x) and Expenditures on Shelters (Y)
regions_plot  <- ggplot2::ggplot(
  data = expenditure,
  aes(x = as.factor(Region), 
      y = Y)) + 
  geom_point() +
  scale_x_discrete(name = "Region",
                   labels = c("Northeast",
                              "North Central",
                              "South",
                              "West")) + 
  labs(title = "Expenditures on Shelters in different Regions",
       y = "Per Capita Exp. on Shelters")
# Save it
ggsave("regions_plot.pdf",
       plot = regions_plot,
       units = "in",
       height = 5,
       width = 5)
# It looks like the States in the Region "West" have
# the biggest Expenditures on shelters.
# Behind the west I am not really sure about the ordering.

# I want to know it precisely
expenditure %>%
  dplyr::mutate(Region = factor(Region,
                                levels = c(1, 2, 3, 4),
                                labels = c("Northeast",
                                           "North Central",
                                           "South",
                                           "West"))) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarise(mean_expenditure = mean(Y))


### Question 2, Part 3
# Please plot the relationship between Y and X1? Describe this graph and the relationship. 
# Reproduce the above graph including one more variable Region and display
# different regions with different types of symbols and colors.

# I used following Gemini prompt to make my plot visually more appealing:
# How to change the text of my legend aes(shape) in ggplot?
# Additionally I used the ggplot2 website https://ggplot2.tidyverse.org/articles/ggplot2-specs.html
# to select fitting shapes

three_var_plot <- ggplot2::ggplot(data = expenditure, 
                aes(x = X1, 
                    y = Y,
                    shape = as.factor(Region))) + 
  geom_point() + 
  labs(title = "Peronal Income and Expenditure on Shelters",
       y = "Expenditure on Shelters",
       x = "Personal Income in State") +
  scale_shape_manual(
    # Legend title
    name = "Region",
    values = c("1" = 0, "2" = 8, "3" = 16, "4" = 23),
    labels = c("1" = "Northeast", 
               "2" = "North Central", 
               "3" = "South", 
               "4" = "West"))
ggsave("3var_plot.pdf",
       plot = three_var_plot)