# Descriptive statistics test analysis - Import your data ----

# Citations

# https://www.rdocumentation.org/packages/DescTools/versions/0.99.34/topics/Mode
# http://math.furman.edu/~dcs/courses/math47/R/library/fBasics/html/015A-BasicStatistics.html
# https://www.researchgate.net/post/Different_result_of_skewness_and_kurtosis-any_thoughts
# https://www.datacamp.com/community/tutorials/make-histogram-basic-r

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.

# 

# Install/load packages

#install.packages("fBasics")
#install.packages("moments")
#install.packages("e1071")
library(gdata)
library(fBasics)
library(DescTools)
library(e1071)

# Load packages

test_data <- read.xls(file.choose())
test_data

# Descriptives

basicStats(test_data$total_multiple_choice) # do not use skewness and kurtosis from this package
Mode(test_data$total_multiple_choice)
skewness(test_data$total_multiple_choice, type = 2) # use type 2 skewness and kurtosis from package e1071
kurtosis(test_data$total_multiple_choice, type = 2) # use type 2 skewness and kurtosis from package e1071

basicStats(test_data$speaking_average) # do not use skewness and kurtosis from this package
Mode(test_data$speaking_average)
kurtosis(test_data$speaking_average, type = 2) # use type 2 skewness and kurtosis from package e1071
skewness(test_data$speaking_average, type = 2) # use type 2 skewness and kurtosis from package e1071

# Histograms

?hist
hist(test_data$total_multiple_choice, breaks = 6, main = "Histogram of Total Multiple Choice Scores",
     xlab = "Number Correct", ylab = "Frequency")

hist(test_data$speaking_average, breaks = 6, main = "Histogram of Average Speaking Scores",
     xlab = "Speaking Score", ylab = "Frequency")

