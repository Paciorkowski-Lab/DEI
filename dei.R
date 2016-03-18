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
# Requires: Hmisc, cluster, pvclust, FactoMineR, 
#
# Usage: $ Rscript dei.R
#

cat("\n**************************\n")
cat("Welcome to DEI version 1.0\n")
cat("\nPaciorkowski Lab, 2016\n")
cat("\nThis software performs analysis of data from the Developmental Encephalopathy Inventory.\n")
cat("Takes as input .csv files appropriately formatted (see user documentation please.)\n")
cat("Analyses done here include:\n")
cat("Comparison of means between categories (Wilcoxon / Mann Whitney U)\n")
cat("Correlations across categories\n")
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

# Set up written report for all results

write("Developmental Encephalopathy Inventory Analysis", file = "DEI_Analysis.txt", append = FALSE, sep = "\n")

write(date(), file = "DEI_Analysis.txt", append = TRUE, sep = "\n")

write("\nResults for the files:", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")

write(x, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
write(y, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")

# Set up written report for only significant results (p<0.05)

write("Developmental Encephalopathy Inventory Analysis", file = "DEI_Analysis_Significant.txt", append = FALSE, sep = "\n")

write(date(), file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")

write("\nSignificant results only (p<0.05) for the files:", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")

write(x, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
write(y, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
write("**********", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")

# Manage Subject ID column (first column)

row.names(data_x)<-data_x$Subject
row.names(data_y)<-data_y$Subject

clean_x <- data_x[-1]
clean_y <- data_y[-1]

# Library of categories

category <- names(clean_x)

# Comparison of means between categories (Wilcoxon / Mann Whitney U)

cat("\nComparison of means between categories (Wilcoxon / Mann Whitney U)...\n")

write("**********", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
write("Results of comparison of means between categories (Wilcoxon / Mann Whitney U)", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")

# for (i in category) try ({
#	result <- wilcox.test(unlist(clean_x[i]), unlist(clean_y[i]))

for (i in category) try ({
	result_mean_x <- mean(unlist(clean_x[i]))
	result_mean_y <- mean(unlist(clean_y[i]))
	result <- wilcox.test(unlist(clean_x[i]), unlist(clean_y[i]))
	lapply(i, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
	write("Mean of x:", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
	lapply(result_mean_x, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
	write("Mean of y:", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
	lapply(result_mean_y, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
	write("P-value:", file = "DEI_Analysis.txt", append = TRUE, sep ="\n")
	lapply(result$p.value, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
	if (result$p.value < 0.05) {
		write("**SIGNIFICANT**", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		lapply(i, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
		write("Mean of x:", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
		lapply(result_mean_x, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
		write("Mean of y:", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
		lapply(result_mean_y, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
		write("P-value:", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
		lapply(result$p.value, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
		write("**SIGNIFICANT**", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
		write("**********", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
	}
	write("**********", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
}, silent = TRUE )


# Correlations across categories
# This uses library Hmisc so we can use rcorr()
# Input data must be matrix

library(Hmisc)

category_rev <- rev(category)

cat("\nCorrelation analysis using spearman rho...\n")

write("**********", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
write("Correlations for: ", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
lapply(x, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")

for (i in category) {
	for (j in category_rev) try ({
		result <- rcorr(as.matrix(clean_x[i]), as.matrix(clean_x[j]), type = "spearman")
		lapply(i, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		lapply(j, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		write("Spearman rho:", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		lapply(result$r, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		write("P-value:", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		lapply(result$P, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		if (result$P[1,2] < 0.05) {
			write("**SIGNIFICANT**", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
			lapply(i, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			lapply(j, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			write("Spearman rho:", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			lapply(result$r, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			write("P-value:", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			lapply(result$P, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			write("**SIGNIFICANT**", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			write("**********", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n") }
		write("**********", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
	}, silent = TRUE)
}

write("Correlations for: ", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
lapply(y, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")

for (i in category) {
	for (j in category_rev) try ({
		result <- rcorr(as.matrix(clean_y[i]), as.matrix(clean_y[j]), type = "spearman")
		lapply(i, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		lapply(j, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		write("Spearman rho:", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		lapply(result$r, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		write("P-value:", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		lapply(result$P, write, file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
		if (result$P[1,2] < 0.05) {
			write("**SIGNIFICANT**", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
			lapply(i, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			lapply(j, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			write("Spearman rho:", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			lapply(result$r, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			write("P-value:", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			lapply(result$P, write, file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			write("**SIGNIFICANT**", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n")
			write("**********", file = "DEI_Analysis_Significant.txt", append = TRUE, sep = "\n") }
		write("**********", file = "DEI_Analysis.txt", append = TRUE, sep = "\n")
	}, silent = TRUE)
}

cat("\nWarnings are silenced since during testing they weren't helpful...\n")

# Cluster analysis
# Generates figures in Rplots.pdf in the same directory

library(pvclust)

cat("\nCluster analysis...\n")

# Only analyzes the 12 main categories (summary scores)

clean_x_summary <- data_x[,2:13]

# Removes NA values

clean_x_summary_nona <- na.omit(clean_x_summary)

fit <- pvclust(clean_x_summary_nona, method.hclust="ward.D", method.dist="euclidean")

plot(fit)

clean_y_summary <- data_y[,2:13]

clean_y_summary_nona <- na.omit(clean_y_summary)

fit <- pvclust(clean_y_summary_nona, method.hclust="ward.D", method.dist="euclidean")

plot(fit)

# Principal Component Analysis

library(FactoMineR)

cat("\nPrincipal Component Analysis...\n")

result <- PCA(clean_x_summary)
result <- PCA(clean_y_summary)

# Final words
cat("\nAll t-test and correlation test results are found in 'DEI_Analysis.txt' in the current working directory.\n")
cat("\nAll significant (p<0.05) results are found in 'DEI_Analysis_Significant.txt' in the current working directory.\n")
cat("\nCluster analysis results are found in Rplot.pdf output.\n")
cat("\nPCA results are also found in Rplot.pdf output.\n")
cat("\nGoodbye.\n")
