# Chi-square test - Import your data ----

# This data is not real, just for learning. 
# Imagine that in a classroom-based research study, 
# the researcher wondered if males and females 
# prefer "pair-work" activity differently. 
# So, the data shows the responses of the males 
# and females to the question if they prefer 
# to do 'pair-work" activities rather than working
# on their own.

# install package to import from SPSS
install.packages("foreign")
library(foreign)

# import file path
lookup_file <- file.choose()
# read in the file and save it to a variable
frequency_table <- read.spss(lookup_file, to.data.frame = TRUE)
frequency_table



# Chi-square test - Assumptions ----

# Assumption #1 - You have two categorical variables. A categorical variable 
#                 can be either a nominal variable or an ordinal variable. 
#                 Even though you can use a chi-square test for association 
#                 with ordinal variables, they have to be treated as nominal 
#                 variables, losing their ordered nature.
# Assumption #2 - You should have independence of observations, which means that
#                 there is no relationship between the observations in the groups 
#                 of the categorical variables or between the groups themselves.
# Assumption #3 - This assumption relates to the nature of your data in order to 
#                 provide a valid result: all cells should have expected counts 
#                 greater than five.

# Chi-square test - Running the chi-square test ----
chi_stat <- chisq.test(frequency_table, correct=FALSE)
chi_stat

# check for assumption # 3
expected_counts <- chi_stat$expected
expected_counts

# phi for effect size
install.packages("psych")
library(psych)

# create custom matrix as a workaround for glitch
custom_data <- matrix(c(18, 10, 7, 15), nrow = 2)
custom_data
phi_stat <- phi(custom_data)
phi_stat

# Chi-square test - All statistics ----
frequency_table
chi_stat
expected_counts
phi_stat