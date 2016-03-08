# dei.R
#
# Version 1.0
#
# Alex Paciorkowski
#
# December 17, 2015
#
# This is an R script that takes as input DEI data in csv format, and outputs results of analyses.
#
# Requires: R 3.1.2, cluster, pvclust, rgl, FactoMineR, 
#
# Usage: $ Rscript dei.R
#
cat("\n**************************\n")
cat("Welcome to DEI version 1.0\n")
cat("\nPaciorkowski Lab, 2016\n")
cat("\nThis software performs analysis of data from the Developmental Encephalopathy Inventory.\n")
cat("Takes as input .csv files appropriately formatted (see user documentation please.)\n")
cat("Analyses done here include:\n")
cat("Comparison of means between categories (t-test)\n")
cat("Correlations across categories\n")
cat("Covariance analysis\n")
cat("Cluster analysis\n")
cat("Principal Component Analysis\n")
cat("***************************\n")

cat("Please enter filename 1: ")
x <- readLines("stdin",n=1L)
if (x == "")
  cat("\nNo file name entered. Program terminated.\n")
cat("You entered ")
  cat(x)
  cat( "\n")

cat("Please enter filename 2: ")
y <- readLines("stdin",n=1L)
if (y == "")
  cat("\nNo file name entered. Program terminated.\n")
cat("You entered ")
  cat(y)
  cat( "\n")

cat("\nOkay, proceeding with analysis with files ") 
  cat(x) 
  cat(" and ") 
  cat(y) 
  cat(".\n")

# Get the data
data_x <- read.csv(x,header=TRUE, sep=",")
data_y <- read.csv(y,header=TRUE, sep=",")

cat("There are ") 
cat(nrow(data_x))
cat(" subjects in ")
cat(x)
cat(" and ")
cat(nrow(data_y))
cat(" in ")
cat(y)
cat(".\n")

# Remove Subject ID column (first column)

clean_x <- x[-1]
clean_y <- y[-1]

# Library of categories

category <- names(clean_x)

# Comparison of means between categories (t-test)

#for (i in category) {
#t.test(clean_x$i,clean_y$i)
#}




# Correlations across categories



# Covariance analysis

# Cluster analysis

# Principal Component Analysis
