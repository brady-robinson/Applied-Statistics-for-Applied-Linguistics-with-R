# Two-way ANOVA - Import your data ----

# These data are for educational purposes, and should not be used
# to inform your opinion regarding the examples.

# The code that follows is adapted from:
# Citation: http://www.sthda.com/english/wiki/two-way-anova-test-in-r
# Citation: http://homepages.inf.ed.ac.uk/bwebb/statistics/Factorial_ANOVA_in_R.pdf

# The Data

# The first grouping variable is gender: 1 represents males and 2 represents females. 
# The second grouping variable is class type:
# Class 1: Attending the on-site ESL classes
# Class 2: Attending the on-online ESL classes
# Class 3: Attending the hybrid (with both on-site and on-line components and requires more time) classes 

# The last column has the outcome variable, end of semester test scores. The test is out of 70.

# We wonder if there is an interaction between gender and class type in terms of performance on the final exam. 
# In other words, we wonder whether males and females benefited differently from the three instructional types.

two_way_data <- read.csv(file.choose())
two_way_data
str(two_way_data)

library(dplyr)

two_way_data$gender <- factor(two_way_data$gender, 
                              levels = c(1, 2),
                              labels = c("M", "F"))

two_way_data$class_type <- factor(two_way_data$class_type, 
                                  levels = c(1, 2, 3),
                                  labels = c("C1", "C2", "C3"))

two_way_data
str(two_way_data)

table(two_way_data$gender, two_way_data$class_type)

# Two-way ANOVA - Assumptions ----

# Assumption #1: You have one dependent variable that is measured 
#                at the continuous level.

# Assumption #2: You have two ("two-way") independent, categorical variables.
#                Factorial ANOVA, of which two-way is one type, can have more
#                than two independent variables.

# Assumption #3: You should have independence of observations.

# Assumption #4: There should be no significant outliers in the groups 
#                of your independent variable in terms of the dependent 
#                variable.

# Assumption #5:Your dependent variable should be approximately normally 
#               distributed for each group of the independent variable.

# Assumption #6: You have homogeneity of variances.


#install.packages("ggpubr")
library("ggpubr")
boxplot <- ggboxplot(two_way_data, x = "class_type", y = "final_test_score", color = "gender",
                     palette = c("#00AFBB", "#E7B800"))
boxplot

library(car)
two_way_an_levene <- leveneTest(final_test_score ~ class_type*gender, 
                                data = two_way_data, center = "mean")
two_way_an_levene

interaction_plot <- interaction.plot(x.factor = two_way_data$class_type, trace.factor = two_way_data$gender, 
                                     response = two_way_data$final_test_score, fun = mean, 
                                     type = "b", legend = TRUE, 
                                     xlab = "Class Type", ylab="Final Test Score",
                                     pch=c(1,19), col = c("#00AFBB", "#E7B800"))
interaction_plot

means_table <- with(two_way_data, tapply(final_test_score, list(gender, class_type), mean))
means_table

# Two-way ANOVA - Running the two-way ANOVA ----

library(car)
two_way_anova <- aov(final_test_score ~ gender * class_type, 
                     data = two_way_data)
summary(two_way_anova)


anova_output <- Anova(two_way_anova, type = "III")
anova_output

tukey_post_hoc <- TukeyHSD(two_way_anova)
tukey_post_hoc

# Extract the residuals

#install.packages("MASS")
library(MASS)

studentized_residuals <- studres(two_way_anova)

two_way_aov_residuals <- residuals(object = two_way_anova)
two_way_aov_residuals

two_way_data_with_residuals <- cbind(two_way_data,two_way_aov_residuals)
two_way_data_with_residuals

two_way_data_with_both_residuals <- cbind(two_way_data_with_residuals, studentized_residuals)
two_way_data_with_both_residuals

# Run Shapiro-Wilk test
male_subset <- subset(two_way_data_with_residuals, gender == "M")
c1_male <- subset(male_subset, class_type == "C1")
c2_male <- subset(male_subset, class_type == "C2")
c3_male <- subset(male_subset, class_type == "C3")

female_subset <- subset(two_way_data_with_residuals, gender == "F")
c1_female <- subset(female_subset, class_type == "C1")
c2_female <- subset(female_subset, class_type == "C2")
c3_female <- subset(female_subset, class_type == "C3")

shapiro.test(x = c1_male$two_way_aov_residuals)
shapiro.test(x = c2_male$two_way_aov_residuals)
shapiro.test(x = c3_male$two_way_aov_residuals)

shapiro.test(x = c1_female$two_way_aov_residuals)
shapiro.test(x = c2_female$two_way_aov_residuals)
shapiro.test(x = c3_female$two_way_aov_residuals)

# Two-way ANOVA - All statistics ----

require("dplyr")
descriptives_two_way <- group_by(two_way_data, class_type, gender) %>%
  summarise(
    count = n(),
    mean = mean(final_test_score, na.rm = TRUE),
    sd = sd(final_test_score, na.rm = TRUE)
  )

descriptives_two_way
two_way_an_levene
interaction_plot
boxplot
means_table
tukey_post_hoc

two_way_data_with_both_residuals

shapiro.test(x = c1_male$two_way_aov_residuals)
shapiro.test(x = c2_male$two_way_aov_residuals)
shapiro.test(x = c3_male$two_way_aov_residuals)

shapiro.test(x = c1_female$two_way_aov_residuals)
shapiro.test(x = c2_female$two_way_aov_residuals)
shapiro.test(x = c3_female$two_way_aov_residuals)
