# ################################################### #
# CUNY MSDA - IS607 : DATA ACQUISITION AND MANAGEMENT
#
# Week 1 Assignment
# ################################################### #

cat("1. Versions of R and RStudio installed:")

# R Version from R.version$version.string
print("R version 3.1.0 (2014-04-10)")

# RStudio Version from "About" Menu:
print("R Studio Version: 0.98.977")

#
cat("2. Version of PostgreSQL installed:")
# PostgreSQL Version from "psql --version"
print("psql (PostgreSQL) 9.3.5")

#
cat("3. R package DMwR installation and loading")
# Checking and Installing DMwR package
if(!require(DMwR)) {
        install.packages("DMwR")
}

# Loading DMwR package
library(DMwR)

# Loading data set sales
data(sales)

# Number of observations in sales data set : 401146
cat("Total number of observations: ", nrow(sales))
