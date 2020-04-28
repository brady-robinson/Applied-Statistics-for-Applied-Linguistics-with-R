# One-way MANOVA - Import your data ----

# Citations
# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.
# School 1: on-site ESL classes
# School 2: online ESL classes
# School 3: hybrid (with both on-site and on-line components and requires more time) ESL classes 

# For each learner, there are two outcome measures, a speaking score and writing score. 20 learners attended
# ESL classes at an on-site school (in-person lessons), 20 attended an online school, and 20 attended a hybrid
# school where ESL classes required in-person and online coursework. We are interested in whether there is a
# difference between the three types of school (instruction). To answer this question, we will investigate the
# two outcomes variables of speaking and writing scores.


# Install packages
# install.packages("gdata")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("ggplot2")
# install.packages("pastecs")
# install.packages("mvnormtest")
install.packages("assertr")
install.packages("heplots")

# Load packages
library(gdata)
library(dplyr)
library(reshape2)
library(ggplot2)
library(pastecs)
library(mvnormtest)
library(assertr)
library(stats)
library(heplots)
library(car)

manova_data <- read.xls(file.choose())
manova_data
columns_to_keep <- c("School", "Speaking_Score", "Writing_Score")
manova_data <- manova_data[columns_to_keep]
manova_data

# Factors

manova_data$School<-factor(manova_data$School, levels = c(1, 2, 3), labels = c("on_site", "online", "hybrid"))
str(manova_data)

# One-way MANOVA - Assumptions ----

# Assumption #1: You have two or more dependent variables that are measured at the continuous level.

# Assumption #2: You have one independent variable that consists of two or more categorical, independent groups.

# Assumption #3: You should have independence of observations.

# Assumption #4: There should be no univariate or multivariate outliers.

# Assumption #5: There needs to be multivariate normality.

# Assumption #6: There should be no multicollinearity.

# Assumption #7: There should be a linear relationship between the dependent variables for each group of the independent variable.

# Assumption #8: You should have an adequate sample size.

# Assumption #9: There should be homogeneity of variance-covariance matrices.

# Assumption #10: There should be homogeneity of variances.


# Boxplots

# First we need to restructure the data into long format:
manova_data_melt <- melt(manova_data, id=c('School'), measured=c('Speaking_Score', 'Writing_Score'))
names(manova_data_melt) <- c('School', 'Outcome_Measure', 'Score')

# plot
box_plot <- ggplot(manova_data_melt, aes(School, Score, color = Outcome_Measure))
box_plot + geom_boxplot() + labs(x='School Type', y='Speaking/Writing Scores', color='Outcome_Measure') + scale_y_continuous(breaks=seq(0,100, by=5))

# descriptives

by(manova_data$Speaking_Score, manova_data$School, stat.desc, basic=F)
by(manova_data$Writing_Score, manova_data$School, stat.desc, basic=F)

# multivariate normality
?mshapiro.test()

# Running the MANOVA test
residuals_manova <- manova(cbind(Speaking_Score, Writing_Score) ~ School, 
                           data = manova_data)
summary(residuals_manova, test="Wilks")
summary.aov(residuals_manova)

# Univariate normality
c1_subset <- subset(manova_data, School == "on_site")
c1_subset

c2_subset <- subset(manova_data, School == "online")
c2_subset

c3_subset <- subset(manova_data, School == "hybrid")
c3_subset

shapiro.test(x = c1_subset$Speaking_Score)
shapiro.test(x = c1_subset$Writing_Score)
shapiro.test(x = c2_subset$Speaking_Score)
shapiro.test(x = c2_subset$Writing_Score)
shapiro.test(x = c3_subset$Speaking_Score)
shapiro.test(x = c3_subset$Writing_Score)

# Sample size assumption

dim(c1_subset)
dim(c2_subset)
dim(c3_subset)

# Assumption of the Equality of Variance-covariance Matrices

output_boxm <- boxM(manova_data[,2:3], manova_data[,1])
summary(output_boxm)

# Levene's test for homogeneirty of variance
leveneTest(manova_data$Speaking_Score, manova_data$School, center = mean)
leveneTest(manova_data$Writing_Score, manova_data$School, center = mean)


# Multicollinearity

corr_output <- cor.test(manova_data$Speaking_Score, manova_data$Writing_Score, method = "pearson")
corr_output

# Linearity

ggplot(c1_subset, aes(Speaking_Score, Writing_Score)) + geom_point()
ggplot(c2_subset, aes(Speaking_Score, Writing_Score)) + geom_point()
ggplot(c3_subset, aes(Speaking_Score, Writing_Score)) + geom_point()

# Multivariate outliers

cols_for_maha <- c("Speaking_Score", "Writing_Score")
data_for_maha <- manova_data[cols_for_maha]
data_for_maha
sort(maha_dist(data_for_maha))
?sort

means <- colMeans(data_for_maha)
covx <- cov(data_for_maha)

maha_values <- mahalanobis(data_for_maha,means, covx)
sort(maha_values)

# Running the MANOVA

# bind outcome variables into one variable

bound_outcomes <- cbind(manova_data$Speaking_Score, manova_data$Writing_Score)

# Create the MANOVA model

manova_model <- manova(bound_outcomes ~ School, data = manova_data)
summary(manova_model, intercept = TRUE) # default Pillai test
summary(manova_model, intercept = TRUE, test = "Wilks")

# Following up on the significant results with a one-way anova

summary.aov(manova_model)


# Post hoc to look at the contrasts

speaking_model <- lm(Speaking_Score ~ School, data=manova_data)
writing_model <- lm(Writing_Score ~ School, data=manova_data)

summary.lm(speaking_model)
summary.lm(writing_model)

# install.packages("multcomp")
library(multcomp)
post_hocs_speaking <- glht(speaking_model, linfct = mcp(School = "Tukey"))
post_hocs_writing <- glht(writing_model, linfct = mcp(School = "Tukey"))
summary(post_hocs_speaking)
summary(post_hocs_writing)


# Effect size

install.packages("lsr")
library(lsr)
effect_size_speaking <- etaSquared(speaking_model)
effect_size_writing <- etaSquared(writing_model)
effect_size_speaking
effect_size_writing

