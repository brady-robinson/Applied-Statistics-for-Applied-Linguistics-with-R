# Binomial logistic regression - Import your data ----

# Citations

# https://www.datacamp.com/community/tutorials/logistic-regression-R
# https://towardsdatascience.com/implementing-binary-logistic-regression-in-r-7d802a9d98fe
# https://www.wikihow.com/Calculate-Weighted-Average
# https://www.rdocumentation.org/packages/rcompanion/versions/2.3.25/topics/nagelkerke
# https://www.rdocumentation.org/packages/ResourceSelection/versions/0.3-5/topics/hoslem.test
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
# https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression
# https://stats.stackexchange.com/questions/5304/why-is-there-a-difference-between-manually-calculating-a-logistic-regression-95/5320#5320

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.

# 

# Install/load packages

install.packages("MASS")
install.packages("rcompanion")
install.packages("ResourceSelection")
install.packages("survey")
library(gdata)
library(ggplot2)
library(dplyr)
library(car)
library(MASS)
library(rcompanion)
library(ResourceSelection)
library(survey)

# Load packages

regression_data <- read.xls(file.choose())
regression_data

# Binomial logistic regression - Assumptions ----

# Assumption #1: You have one dependent variable that is dichotomous (i.e., a nominal variable with two outcomes). 

# Assumption #2: You have one or more independent variables that are measured on either a continuous or nominal scale. 
# If one of your independent variables was measured at the ordinal level, it can still be entered in a binomial logistic 
# regression, but it must be treated as either a continuous or nominal variable. It cannot be entered as an ordinal variable.

# Assumption #3: You should have independence of observations and the categories of the dichotomous dependent variable and 
# all your nominal independent variables should be mutually exclusive and exhaustive.

# Assumption #4: You should have a bare minimum of 15 cases per independent variable, although some recommend as high as 50 
# cases per independent variable.

# Assumption #5: Your data must not show multicollinearity

# Assumption #6: There should be no significant outliers, high leverage points or highly influential points

# Regression

regression_model <- glm(pass_fail ~ speaking_score + age + lived_eng + study_time, data = regression_data, family = binomial)
summary(regression_model)

# Probabilities

probabilities <- predict(regression_model, type = "response")

# Predictions

predictions <- ifelse(probabilities > 0.5, "pass", "fail")

# Confusion matrix

table(predictions, regression_data$pass_fail)
prop_correct_pass = 16/(16+19) # number classified as pass divided by total number pass
prop_correct_fail = 55/(55+10) # number classified as fail divided by total number fail
percent_correct_total = prop_correct_fail*65 + prop_correct_pass*35 # raw numbers represent total fail and total pass

# Wald

regTermTest(regression_model, ~age)
regTermTest(regression_model, ~study_time)
regTermTest(regression_model, ~lived_eng)
regTermTest(regression_model, ~speaking_score)

# Odds ratio

exp(coef(regression_model))

# 95% Confidence intervals

exp(confint.default(regression_model))

# Nagelkerke R Square

nagelkerke(regression_model)

# Hosmer-Lemeshow Goodness of Fit

hoslem.test(regression_data$pass_fail,fitted(regression_model))
