# Inter- and intra-rater consistency test analysis - Import your data ----

# Citations

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

# The Data

# These data are for educational purposes only, and should not inform you opinion about the subject matter.

# 

# Install/load packages

library(gdata)
library(Hmisc)

# Import data

test_data <- read.xls(file.choose())
test_data

# Inter-rater reliability correlation matrix

rcorr(as.matrix(cbind(test_data$ra1_avg_ro1_ro2,
                      test_data$ra2_avg_ro1_ro2)),
      type = c("spearman"))

# Intra-rater reliability correlation matrix

rcorr(as.matrix(cbind(test_data$ra1_ro1_avg,
                      test_data$ra1_ro2_avg)),
      type = c("spearman"))

rcorr(as.matrix(cbind(test_data$ra2_ro1_avg,
                      test_data$ra2_ro2_avg)),
      type = c("spearman"))
