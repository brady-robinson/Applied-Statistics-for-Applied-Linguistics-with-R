# One-way ANOVA - Import your data ----

# Description of the data

# Remember that these data are not real, only for educational purposes.

# Group 1: Attending the on-site ESL classes
# Group 2: Attending the on-online ESL classes
# Group 3: Attending the hybrid (with both on-site and on-line components and 
#          requires more time) classes 

# The outcome is their performance on a motivation-for-learning-ESL questionnaire. 
# The questionnaire was a Likert-scale with responses ranging from 1 to 10.  
# The question is if these learners in the three different groups are different in 
# terms of their level of motivation for learning ESL. The hypothesis is that the 
# ones in group 3 have a higher level of motivation. Here we are not making any causal 
# inference, meaning we are not claiming attending the hybrid classes increases 
# motivation. The opposite may be very well true. The ones with higher motivation 
# may choose to attend the hybrid classes. So, it is important to emphasize we want 
# to figure out if they are different, and with the current data, we can't answer the 
# question of why they are different.

anova_data <- read.csv(file.choose())
anova_data


# One-way ANOVA - Assumptions ----

# Assumption #1: You have one dependent variable that is measured at the continuous 
#                level.

# Assumption #2: You have one independent variable that consists of three or more 
#                categorical, independent groups.

# Assumption #3: You should have independence of observations.

# Assumption #4: There should be no significant outliers in the groups of your 
#                independent variable in terms of the dependent variable.

boxplot_descriptives <- function(input_data){
  vector_1 = c()
  vector_2 = c()
  vector_3 = c()
  counter = 1
  
  for (element_1 in input_data$level_of_motivation){
    if (input_data[counter,][[1]] == 1){
      vector_1 <- c(vector_1, input_data[counter,][[2]])
    } else if (input_data[counter,][[1]] == 2){
      vector_2 <- c(vector_2, input_data[counter,][[2]])
    } else if (input_data[counter,][[1]] == 3){
      vector_3 <- c(vector_3, input_data[counter,][[2]])
    }
    counter = counter + 1
  }
  print(summary(vector_1))
  print("Standard deviation")
  print(sd(vector_1))
  print("N")
  print(length(vector_1))
  
  print(summary(vector_2))
  print("Standard deviation")
  print(sd(vector_2))
  print("N")
  print(length(vector_2))
  
  print(summary(vector_3))
  print("Standard deviation")
  print(sd(vector_3))
  print("N")
  print(length(vector_3))
  
  boxplot(vector_1, vector_2, vector_3)
}

boxplot_descriptives(anova_data)

# Assumption #5: Your dependent variable should be approximately normally distributed 
#                for each group of the independent variable. You can run a 
#                non-parametric test such as the Kruskal-Wallis H test.


# Assumption #6: You have homogeneity of variances. If violated, interpret the results 
#                of the Welch ANOVA.

library(car)

anova_data_df <- as.data.frame(anova_data)
colnames(anova_data_df) <- c("groups", "level_of_motivation")
anova_data_df

levene_anova_stat <- leveneTest(as.numeric(anova_data_df$level_of_motivation), as.character(anova_data_df$groups))
levene_anova_stat

# One-way ANOVA - Running the one-way ANOVA ----

aov_stat <- aov(level_of_motivation ~ groups, data = anova_data)
anova_stat <- summary(aov_stat)
anova_stat

tukey_post_hoc <- TukeyHSD(aov_stat)
tukey_post_hoc

res <- residuals(object = aov_stat)
normality_assumption <- shapiro.test(x = res)


install.packages("sjstats")
library(sjstats)
effect_size <- omega_sq(aov_stat)
effect_size

# Citation: code borrowed from http://www.sthda.com/english/wiki/one-way-anova-test-in-r

# One-way ANOVA - All statistics ----
boxplot_descriptives(anova_data)
normality_assumption
levene_anova_stat
anova_stat
tukey_post_hoc
effect_size
