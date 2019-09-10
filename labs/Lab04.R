# R Programming (EDA 2)
# Adapted from R4DS Chapter 7: Exploratory Data Analysis

# Prerequisite: use preload function to get tidyverse and descr load them

library(tidyverse)
library(descr) # Describe attributes of objects/variables

# Explore the diamonds dataset
?diamonds

# Examine the variables using the class function
class(cut)
sapply(diamonds,class)

# Frequency table from descr package
attach(diamonds)
fre(cut,plot=F) # can add a plot by default

# Frequency table, alternative method
with(diamonds, {freq(cut, plot=T)})

# Create tibble object out of diamonds dataset
diam_tb=as_tibble(diamonds)
class(diam_tb) # How is this different from a data frame?

# Plot the bar chart using ggplot2
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut)) 

# Better plot with percentage on y axis
ggplot(data = diamonds,aes(cut)) +
  geom_bar(mapping = aes(y = (..count..)/sum(..count..))) + theme_bw() +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent")

# Exercise 1
# Save the chart into PNG, PDF and SVG formats
# What are the differences among all these formats?

# Plot another variable carat

ggplot(data = diam_tb) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# Exercise 2
# Can you improve the visualization of this chart?
# What is the difference between barchart and histogram?

# Subsetting the dataset
smaller <- diamonds %>% 
  filter(carat < 3)

# Histogram of smaller dataset
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# Polygon
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1) + theme_bw() 

# Exercise 3
# Can you change colors?
# Hint: use the RcolorBrewer package

# Continuous variables

ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

# Exercise 4
# What method you will use to analyze:
# Dependent variable: continuous, Independent variable: discrete
# Dependent variable: discrete, Independent variable: continuous
# Dependent variable: continuous, Independent variable: continuous





