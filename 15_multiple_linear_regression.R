# Multiple Linear regression - Import your data ----

# Citations

# https://www.statmethods.net/stats/rdiagnostics.html
# https://rpubs.com/Hank_Stevens/prp
# https://www.rdocumentation.org/packages/car/versions/3.0-7/topics/avPlots
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/160-multicollinearity-essentials-and-vif-in-r/
# https://stackoverflow.com/questions/24305271/extracting-standardized-coefficients-from-lm-in-r

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.

# Our research question today: Can we "predict" a speaking score in learners of English? Imagine that we recruited 100
# participants to perform a speaking test. We test recorded their age, anxiety levels, average study time, and 
# whether or not they lived in an English speaking country for any amount of time. Remember that this scenario and 
# are not real, and are for educational purposes.

# Install/load packages

library(gdata)
library(ggplot2)
library(dplyr)
library(car)
library(MASS)

# Load packages

regression_data <- read.xls(file.choose())
regression_data

# Multiple Linear regression - Assumptions ----

# Assumption #1: You have one dependent variable that is measured at the continuous level. 
# Assumption #2: You have two or more independent variables that are measured either at the continuous or nominal level.
# Assumption #3: You should have independence of observations (i.e., independence of residuals)
# Assumption #4: There needs to be a linear relationship between (a) the dependent variable and each of your independent variables, 
# and (b) the dependent variable and the independent variables collectively
# Assumption #5: Your data needs to show homoscedasticity of residuals
# Assumption #6: Your data must not show multicollinearity
# Assumption #7: There should be no significant outliers
# Assumption #8:You need to check that the residuals (errors) are approximately normally distributed

# Regression (repeated without outlier)

regression_model <- lm(speaking_score ~ study_time + age + anxiety_level +
                         study_time + lived_eng, data = regression_data)
summary(regression_model)

# Outlier test

outlierTest(regression_model)

# Standardized regression coefficients

scaled_model <- lm(scale(speaking_score) ~ scale(study_time) + scale(age) + scale(anxiety_level) +
                     scale(study_time) + scale(lived_eng), data = regression_data)
coef(scaled_model)

# Diagnostics

influence(regression_model)

# Durbin Watson

durbinWatsonTest(regression_model)

# Histogram of standardized residuals

ggplot(regression_model, aes(regression_model$residuals)) + geom_histogram()

# Normal P-P Plot of Regression Standardized Residual

qqPlot(regression_model$residuals)

# Linearity studentized residuals versus predicted values 
# (the independent variables collectively are linearly related to the dependent variable)

sresid <- studres(regression_model)
predicted_values <- predict(regression_model)
frame_for_plot <- as.data.frame(cbind(sresid, predicted_values))
ggplot(frame_for_plot, aes(sresid, predicted_values)) + geom_point() # Homoscedasticity

# Linearity
# (each independent variable is linearly related to the dependent variable)
# Partial regression plots (av plots)

avPlot(regression_model, variable = "age")
avPlot(regression_model, variable = "anxiety_level")
avPlot(regression_model, variable = "study_time")
avPlot(regression_model, variable = "lived_eng")

# Multicollinearity

vif(regression_model)
summary(regression_model)
