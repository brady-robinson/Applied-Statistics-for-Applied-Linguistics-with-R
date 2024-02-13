# Independent-samples t-test - Import your data ----
ind_t_test_data <- read.csv(file = file.choose())
ind_t_test_data
descriptives <- summary(ind_t_test_data)
descriptives

# Independent-samples t-test - Assumptions ----
# Assumptions for the independent-samples t-test:
# Assumption #1: You have one dependent variable that is measured at the continuous level.
# Assumption #2: You have one independent variable that consists of two categorical, 
#                independent groups (i.e., a dichotomous variable).
# Assumption #3: You should have independence of observations, 
#                which means that there is no relationship between the observations in each 
#                group of the independent variable or between the groups themselves.
-------------------------------------------------------------------------------
# Assumption #4: There should be no significant outliers in the two groups of your 
#                independent variable in terms of the dependent variable.
  # -- boxplot
boxplot_both_groups <- boxplot(ind_t_test_data$explicit_feedback, ind_t_test_data$implicit_feedback)
-------------------------------------------------------------------------------
# Assumption #5: Your dependent variable should be approximately normally distributed 
#                for each group of the independent variable.
  # -- skewness and kurtosis
install.packages("psych")
library(psych)
skewness_both_groups <- skew(ind_t_test_data)
skewness_both_groups
kurtosis_both_groups <- kurtosi(ind_t_test_data)
kurtosis_both_groups
  # -- Shapiro-Wilk test
shapiro_explicit <- shapiro.test(ind_t_test_data$explicit_feedback)
shapiro_explicit
shapiro_implicit <- shapiro.test(ind_t_test_data$implicit_feedback)
shapiro_implicit
-------------------------------------------------------------------------------
# Assumption #6: You have homogeneity of variances (i.e., the variance is equal in each group of your independent variable).
  # -- Levene's test
install.packages("car")
library(car)
vec <- c(ind_t_test_data$explicit_feedback, ind_t_test_data$implicit_feedback)
vec
group <- c(rep(1, length(ind_t_test_data$explicit_feedback)), 
                     rep(2, length(ind_t_test_data$implicit_feedback)))
group

levene_statistic <- leveneTest(vec,group,center=mean)
levene_statistic
# Independent-samples t-test - Running the t-test ----
# Indpendent-samples t-test
t_test_statistic <- t.test(ind_t_test_data$explicit_feedback,ind_t_test_data$implicit_feedback,
       paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)
t_test_statistic
# Independent-samples t-test - All statistics ----

descriptives
boxplot_both_groups
skewness_both_groups
kurtosis_both_groups
levene_statistic
t_test_statistic
# Paired-samples t-test - Import your data ----
prd_t_test_data <- read.csv(file = file.choose())
prd_t_test_data
paired_descriptives <- summary(prd_t_test_data)
paired_descriptives