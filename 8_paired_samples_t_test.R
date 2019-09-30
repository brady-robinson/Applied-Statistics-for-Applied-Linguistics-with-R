# Paired-samples t-test - Assumptions ----
--------------------------------------------------------------------------------------------
# Assumption #1: You have one dependent variable that is measured at the continuous level.
# Assumption #2: You have one independent variable that consists of two categorical, 
#                independent groups (i.e., a dichotomous variable).
# Assumption #3: There should be no significant outliers in the two groups of your independent 
# variable in terms of the dependent variable.
--------------------------------------------------------------------------------------------
# Assumption #4: Your dependent variable should be approximately normally distributed for each 
# group of the independent variable.
# -- For a paired-samples t-test, the assumption of normality and no outliers is tested on the 
#   differences between the paired-values, not the values of the paired groups themselves. 
# -- If these assumptions are violated, run the Wilcoxon signed-rank test or sign test.

# Normality
boxplot_paired <- boxplot(prd_t_test_data$Difference)
boxplot_paired

# Shapiro-Wilk test
shapiro_paired <- shapiro.test(prd_t_test_data$Difference)
shapiro_paired

# Paired-samples t-test - Running the t-test
# Paired-samples t-test - Running the t-test ----

paired_t_test_statistic <- t.test(prd_t_test_data$Pre_test,prd_t_test_data$Post_test,
                           paired = TRUE)
paired_t_test_statistic

# Paired-samples t-test - Effect size ----

# To calculate an effect size, called d or Cohen's d, for a paired-samples t-test you 
# need to divide the mean difference by the standard deviation of the difference

paired_cohens_d <- mean(prd_t_test_data$Difference)/sd(prd_t_test_data$Difference)
paired_cohens_d

# Paired-samples t-test - All statistics ----

prd_t_test_data
paired_descriptives
boxplot_paired
shapiro_paired
paired_t_test_statistic
paired_cohens_d