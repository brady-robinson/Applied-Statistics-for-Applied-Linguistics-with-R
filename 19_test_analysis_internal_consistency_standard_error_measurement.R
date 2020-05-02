# Descriptive statistics test analysis - Import your data ----

# Citations

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.

# 

# Install/load packages

library(gdata)
library(fBasics)
library(DescTools)

# Import data

test_data <- read.xls(file.choose())
test_data

# Internal consistency

cron_alpha <- CronbachAlpha(test_data[,5:19])

# Standard error of measurement

st_dev <- basicStats(test_data$total_multiple_choice)[14,]
# standard_error_measurement = Standard_deviation*sqrt(1 - Cronbach's_alpha)
standard_error_measurement = st_dev*sqrt(1 - cron_alpha)
