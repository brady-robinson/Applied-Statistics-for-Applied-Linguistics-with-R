# One-way ANCOVA - Import your data ----

# All of this code was directly adapted from https://gaopinghuang0.github.io/2017/11/04/ANCOVA-notes
# so please see this great blog at the link provided for more information.
# The blog linked above draws a lot of the code from this book:
# "Discovering Statistics using R (2012)” by Andy Field
# Both of these resources are great and we thank them both for informing the code below

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.
# Group 1: Attending the on-site ESL classes
# Group 2: Attending the on-online ESL classes
# Group 3: Attending the hybrid (with both on-site and on-line components and requires more time) classes 

# For each learner, there are two sets of measures of motivation, one pre-program (before they attended the courses) 
# and one post-program (when they finished the courses). Now, we want to compare the motivation level of the three 
# groups at the end of the courses; however, after making adjustments for their motivation level before they started 
# the courses. In other words, we want to control for the effect of already existing differences in the motivation 
# level of learners while comparing their motivation levels at the end of the courses. In this case, 
# pre-program motivation is the covariate, and post-program motivation is the dependent variable.

imported_ancova_data <- read.csv(file.choose())
imported_ancova_data
imported_ancova_data$group <- factor(imported_ancova_data$group, levels = c(1:3),
                                     labels = c("Class 1", "Class 2", "Class 3"))
str(imported_ancova_data)

#install.packages("pastecs")
library(pastecs)
options(digits=2)  # round output to 2 digits
by(imported_ancova_data$post_program_motivation, imported_ancova_data$group, stat.desc, basic=F)
summary(imported_ancova_data)

#install.packages("reshape2")
library(reshape2)
restructured_imported_ancova <-melt(imported_ancova_data, id=c("group"), 
                                    measured=c("pre_program_motivation", "post_program_motivation"))
restructured_imported_ancova
names(restructured_imported_ancova)<-c("group", "pre_or_post_motiv", "motiv_level")
restructured_imported_ancova

library(ggplot2)
boxplot <- ggplot(restructured_imported_ancova, aes(group, motiv_level))
boxplot + geom_boxplot() + facet_wrap(~pre_or_post_motiv) + labs(x="Group", y ="Motivation Level")

# Levene's test for homogeneirty of variance
library(car)
leveneTest(imported_ancova_data$post_program_motivation, imported_ancova_data$group, center = median)

# The covariate (pre-program movivation) and the independent variable (group) 
# should be independent
motivationModel.1<-aov(pre_program_motivation ~ group, data = imported_ancova_data)
summary(motivationModel.1)




# One-way ANCOVA - Assumptions ----

# Assumption #1: You have one dependent variable that is measured at the 
#                continuous level.
# Assumption #2: You have one independent variable that consists of two 
#                or more categorical, independent groups.
# Assumption #3: You have one covariate variable that is measured at the 
#                continuous level.
# Assumption #4: You should have independence of observations, which means 
#                that there	is no relationship between the observations in 
#                each group of the independent variable	or between the 
#                groups themselves.
# Assumption #5: The covariate should be linearly related to the dependent 
#                variable at each	level of the independent variable.
# Assumption #6: You should have homogeneity of regression slopes.
# Assumption #7: Your dependent variable should be approximately normally 
#                distributed for each group of the independent variable.
# Assumption #8: There should be homoscedasticity.
# Assumption #9: There should be homogeneity of variances.
# Assumption #10: There should be no significant outliers in the groups.


# One-way ANCOVA - Running the ANCOVA ----

# the ANCOVA with orthognal contrasts
contrasts(imported_ancova_data$group)<-cbind(c(-2,1,1), c(0,-1,1))
motivation_model <- aov(post_program_motivation ~ pre_program_motivation + group, data = imported_ancova_data)
ancova_model <- Anova(motivation_model, type="III")
ancova_model

# Normality with standardized residuals
motiv_stdres = rstandard(motivation_model)
motiv_stdres

imported_ancova_data_with_residuals <- cbind(imported_ancova_data,motiv_stdres)
imported_ancova_data_with_residuals

c1_subset <- subset(imported_ancova_data_with_residuals, group == "Class 1")
c1_subset

c2_subset <- subset(imported_ancova_data_with_residuals, group == "Class 2")
c2_subset

c3_subset <- subset(imported_ancova_data_with_residuals, group == "Class 3")
c3_subset

shapiro.test(x = c1_subset$motiv_stdres)
shapiro.test(x = c2_subset$motiv_stdres)
shapiro.test(x = c3_subset$motiv_stdres)

# Homoscedasticity
predicted_values <- predict.lm(motivation_model)
imported_ancova_data_with_residuals_predicted <- cbind(imported_ancova_data_with_residuals,predicted_values)
imported_ancova_data_with_residuals_predicted

c1_subset_pred <- subset(imported_ancova_data_with_residuals_predicted, group == "Class 1")
c1_subset_pred

c2_subset_pred <- subset(imported_ancova_data_with_residuals_predicted, group == "Class 2")
c2_subset_pred

c3_subset_pred <- subset(imported_ancova_data_with_residuals_predicted, group == "Class 3")
c3_subset_pred

plot(x = c1_subset_pred$predicted_values, y = c1_subset_pred$motiv_stdres)
plot(x = c2_subset_pred$predicted_values, y = c2_subset_pred$motiv_stdres)
plot(x = c3_subset_pred$predicted_values, y = c3_subset_pred$motiv_stdres)


# Adjusted means for the covariate
# install.packages("effects")
library(effects)
adjusted_means<-effect("group", motivation_model, se=TRUE)
summary(adjusted_means)
adjusted_means$se

# ANOVA when covariate is not accounted for
anova_model<-aov(post_program_motivation ~ group, data = imported_ancova_data)
summary(anova_model)

# Post hoc -- the glht function lets us look at adjusted means
# install.packages("multcomp")
library(multcomp)
post_hocs<-glht(motivation_model, linfct = mcp(group = "Tukey"))
summary(post_hocs)
confint(post_hocs)

# Visualizations for heteroscedastiicty and normality
plot(motivation_model)

# Homogeneity of regression slopes
scatter <- ggplot(imported_ancova_data, aes(pre_program_motivation, post_program_motivation, colour = group))
scatter + geom_point(aes(shape = group), size = 3) + 
  geom_smooth(method = "lm", aes(fill = group), alpha = 0.1) +
  labs(x = "Pre-program motivation", y = "Post-program motivation")

# Homogeneity of regression slopes with different ANCOVA
homogen_reg_slopes <- update(motivation_model, .~. + pre_program_motivation:group)
output_homogen_reg_slopes <- Anova(homogen_reg_slopes, type="III")  # from car package
output_homogen_reg_slopes

# Effect size

#install.packages("lsr")
library(lsr)
effect_size <- etaSquared(motivation_model)
effect_size

# One-way ANCOVA - All statistics ----

imported_ancova_data
by(imported_ancova_data$post_program_motivation, imported_ancova_data$group, stat.desc, basic=F)
summary(imported_ancova_data)
leveneTest(imported_ancova_data$post_program_motivation, imported_ancova_data$group, center = median)
summary(motivationModel.1)
ancova_model
effect_size
shapiro.test(x = c1_subset$motiv_stdres)
shapiro.test(x = c2_subset$motiv_stdres)
shapiro.test(x = c3_subset$motiv_stdres)
plot(x = c1_subset_pred$predicted_values, y = c1_subset_pred$motiv_stdres)
plot(x = c2_subset_pred$predicted_values, y = c2_subset_pred$motiv_stdres)
plot(x = c3_subset_pred$predicted_values, y = c3_subset_pred$motiv_stdres)
summary(adjusted_means)
adjusted_means$se
summary(post_hocs)
confint(post_hocs)
plot(motivation_model)
output_homogen_reg_slopes