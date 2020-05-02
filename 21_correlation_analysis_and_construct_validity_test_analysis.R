# Correlation analysis and construct validity test analysis - Import your data ----

# Citations

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.

# 

# Install/load packages

install.packages("Hmisc")
library(gdata)
library(Hmisc)


# Import data

test_data <- read.xls(file.choose())
test_data

# Correlation analysis between subsections

rcorr(as.matrix(test_data[,45:49]), type = c("spearman"))

# Correlation analysis between total scores of each section

rcorr(as.matrix(cbind(test_data$total_multiple_choice,
                      test_data$speaking_average)),
      type = c("pearson"))
