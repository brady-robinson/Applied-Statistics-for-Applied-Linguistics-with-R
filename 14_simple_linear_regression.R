# Simple Linear regression - Import your data ----

# Citations

# https://www.datacamp.com/community/tutorials/linear-regression-R
# http://www.sthda.com/english/articles/40-regression-analysis/167-simple-linear-regression-in-r/
# https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781788627306/1/ch01lvl1sec18/r-packages-for-regression
# https://stats.idre.ucla.edu/spss/output/regression-analysis/
# https://www.programmingr.com/examples/r-dataframe/add-delete-rows/
# http://www.sthda.com/english/wiki/qq-plots-quantile-quantile-plots-r-base-graphs
# http://www.r-tutor.com/elementary-statistics/simple-linear-regression/normal-probability-plot-residuals

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.

# Our research question today is simple: Does amount of time spent studying account for scores on a speaking 
# test? Remember that these data are made up for educational purposes.

# Install/load packages

library(gdata)
library(ggplot2)
library(dplyr)
library(car)


# Load packages

regression_data <- read.xls(file.choose())
regression_data

# Simple Linear regression - Assumptions ----

# Assumption #1: You have one dependent variable that is measured at the continuous level. 

# Assumption #2: You have one independent variable that is measured at the continuous level. 

# Assumption #3: There needs to be a linear relationship between the dependent and independent variables.

# Assumption #4: You should have independence of observations, which you can easily check using the Durbin-Watson statistic.

# Assumption #5: There should be no significant outliers.

# Assumption #6: Your data needs to show homoscedasticity.

# Assumption #7: You need to check that the residuals (errors) of the regression line are approximately normally distributed.

# Scatterplot

ggplot(regression_data, aes(study_time, speaking_score)) + geom_point()
ggplot(regression_data, aes(study_time, speaking_score)) + geom_point() + stat_smooth(method = lm, se = FALSE)

# Regression

regression_model <- lm(speaking_score ~ study_time, data = regression_data)
summary(regression_model)
regression_model

# Outlier test

outlierTest(regression_model)

# Remove outlier

regression_data <- regression_data[-c(91),]
regression_data

# Scatterplot without outlier

ggplot(regression_data, aes(study_time, speaking_score)) + geom_point()
ggplot(regression_data, aes(study_time, speaking_score)) + geom_point() + stat_smooth(method = lm, se = FALSE)

# Regression (repeated without outlier)

regression_model <- lm(speaking_score ~ study_time, data = regression_data)
summary(regression_model)
regression_model
regression_model$residuals

# Durbin Watson

durbinWatsonTest(regression_model)

# Scatterplot predicted versus residuals - homoscedasticity

predicted_values <- predict(regression_model)
residual_values <- regression_model$residuals

ggplot(regression_model, aes(predicted_values, residual_values)) + geom_point()

# Histogram of standardized residuals

ggplot(regression_model, aes(regression_model$residuals)) + geom_histogram(binwidth = 0.1)

# Normal P-P Plot of Regression Standardized Residual

qqPlot(regression_model$residuals)
