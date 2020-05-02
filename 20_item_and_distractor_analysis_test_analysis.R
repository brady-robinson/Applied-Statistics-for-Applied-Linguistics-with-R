# Descriptive statistics test analysis - Import your data ----

# Citations

# https://cran.r-project.org/web/packages/CTT/CTT.pdf

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.

# 

# Install/load packages

install.packages("CTT")

library(gdata)
library(psych)
library(CTT)
library(DescTools)

# Import data

test_data <- read.xls(file.choose())
test_data

# Cronbach's Alpha with all items

CronbachAlpha(test_data[,5:19])

# Item analysis

alpha(test_data[,5:19])

# Item facility

item_facility <- colMeans(test_data[,5:19], na.rm=TRUE)

# Distractor analysis

data(CTTdata)
data(CTTkey)
CTTdata
CTTkey

distractorAnalysis(CTTdata, CTTkey)
