################################################################################
# Project:      Medical Data PCA Analysis
# Objective:    Reduce dimensionality using PCA and identify outliers
# Author:       Kim Shellenberger
# Date:         2024
# Description:  Principal Component Analysis (PCA) to understand data structure,
#               identify outliers, and reduce dimensions. Includes comprehensive
#               data quality checks and statistical analysis.
################################################################################

# SECTION 1: LOAD REQUIRED PACKAGES
library(tidyverse)    # Data manipulation suite

# SECTION 2: LOAD DATA
# TODO: Update file path to your data location
medical_raw_data <- read.csv("medical_raw_data.csv")

# SECTION 3: DATA EXPLORATION
# Examine dataset structure and dimensions
str(medical_raw_data)

# SECTION 4: DATA QUALITY ASSESSMENT
# Check for duplicate records in ID fields
cat("Duplicates in Customer_id:", sum(duplicated(medical_raw_data$Customer_id)), "\n")
cat("Duplicates in Interaction:", sum(duplicated(medical_raw_data$Interaction)), "\n")
cat("Duplicates in UID:", sum(duplicated(medical_raw_data$UID)), "\n")

# Check for missing values
cat("\nTotal NA values in dataset:", sum(is.na(medical_raw_data)), "\n")
cat("NA values per column:\n")
print(colSums(is.na(medical_raw_data)))

# SECTION 5: OUTLIER DETECTION
# Create boxplots and extract statistics for each numerical variable
# This identifies extreme values that may skew analysis

cat("\n=== POPULATION ===\n")
boxplot.stats(medical_raw_data$Population)
boxplot(medical_raw_data$Population, main = "Population Distribution")

cat("\n=== ADDITIONAL CHARGES ===\n")
boxplot.stats(medical_raw_data$Additional_charges)
boxplot(medical_raw_data$Additional_charges, main = "Additional Charges Distribution")

cat("\n=== AGE ===\n")
boxplot.stats(medical_raw_data$Age)
boxplot(medical_raw_data$Age, main = "Age Distribution")

cat("\n=== NUMBER OF CHILDREN ===\n")
boxplot.stats(medical_raw_data$Children)
boxplot(medical_raw_data$Children, main = "Number of Children Distribution")

cat("\n=== INCOME ===\n")
boxplot.stats(medical_raw_data$Income)
boxplot(medical_raw_data$Income, main = "Income Distribution")

cat("\n=== VITAMIN D LEVELS ===\n")
boxplot.stats(medical_raw_data$VitD_levels)
boxplot(medical_raw_data$VitD_levels, main = "Vitamin D Levels Distribution")

cat("\n=== DOCTOR VISITS ===\n")
boxplot.stats(medical_raw_data$Doc_visits)
boxplot(medical_raw_data$Doc_visits, main = "Doctor Visits Distribution")

cat("\n=== FULL MEALS EATEN ===\n")
boxplot.stats(medical_raw_data$Full_meals_eaten)
boxplot(medical_raw_data$Full_meals_eaten, main = "Full Meals Eaten Distribution")

cat("\n=== VITAMIN D SUPPLEMENT ===\n")
boxplot.stats(medical_raw_data$VitD_supp)
boxplot(medical_raw_data$VitD_supp, main = "Vitamin D Supplement Distribution")

cat("\n=== INITIAL DAYS ===\n")
boxplot.stats(medical_raw_data$Initial_days)
boxplot(medical_raw_data$Initial_days, main = "Initial Days Distribution")

cat("\n=== TOTAL CHARGE ===\n")
boxplot.stats(medical_raw_data$TotalCharge)
boxplot(medical_raw_data$TotalCharge, main = "Total Charge Distribution")

# SECTION 6: DATA CLEANING
# Standardize missing values - convert string 'NA' to proper NA
medical_raw_data[medical_raw_data == 'NA'] <- NA

# Verify cleaning
cat("\nAfter standardizing NA values:\n")
cat("Total NA values:", sum(is.na(medical_raw_data)), "\n")
print(colSums(is.na(medical_raw_data)))

# SECTION 7: MISSING VALUE IMPUTATION
# TODO: Implement median imputation for missing numerical values
# Example: medical_raw_data$Column <- ifelse(is.na(medical_raw_data$Column), 
#                                            median(medical_raw_data$Column, na.rm = TRUE),
#                                            medical_raw_data$Column)
medical_raw_data$Age <- replace(medical_raw_data$Age, is.na(medical_raw_data$Age), median(medical_raw_data$Age, na.rm=TRUE))
medical_raw_data$Initial_days <- replace(medical_raw_data$Initial_days, is.na(medical_raw_data$Initial_days), median(medical_raw_data$Initial_days, na.rm=TRUE))
medical_raw_data$Income <- replace(medical_raw_data$Income, is.na(medical_raw_data$Income), median(medical_raw_data$Income, na.rm=TRUE))
medical_raw_data$Children <- replace(medical_raw_data$Children, is.na(medical_raw_data$Children), median(medical_raw_data$Children, na.rm=TRUE))

# Reclass columns as numeric
medical_raw_data$Age <- as.numeric(medical_raw_data$Age)
medical_raw_data$Children <- as.numeric(medical_raw_data$Children)
medical_raw_data$Income <- as.numeric(medical_raw_data$Income)
medical_raw_data$Initial_days <- as.numeric(medical_raw_data$Initial_days)
medical_raw_data$Population <- as.numeric(medical_raw_data$Population)
medical_raw_data$Doc_visits <- as.numeric(medical_raw_data$Doc_visits)
medical_raw_data$Full_meals_eaten <- as.numeric(medical_raw_data$Full_meals_eaten)
medical_raw_data$VitD_supp <- as.numeric(medical_raw_data$VitD_supp)

# Change 0 to No and 1 to Yes
medical_raw_data$Overweight[medical_raw_data$Overweight =="0"] <- "No"
medical_raw_data$Overweight[medical_raw_data$Overweight =="1"] <- "Yes"
medical_raw_data$Anxiety [medical_raw_data$Anxiety =="0"] <- "No"
medical_raw_data$Anxiety[medical_raw_data$Anxiety =="1"] <- "Yes"

#Summary of data set
str(medical_raw_data)

#zip codes had dropped leading zeroes 
library(zipcodeR)
medical_raw_data$Zip <- c(normalize_zip(medical_raw_data$Zip))

#drop first column due to redundancy
medical_raw_data <- subset(medical_raw_data, select = -1)

#Create Mode function for treating NAs in character variables
mode_funct <- function(x) {
  col_tbl <- table(x)
  names(col_tbl)[which(col_tbl==max(col_tbl))]
}

#change character columns with NA to Mode using the Mode function
medical_raw_data$Anxiety<-replace(medical_raw_data$Anxiety,is.na(medical_raw_data$Anxiety), mode_funct(medical_raw_data$Anxiety))
medical_raw_data$Soft_drink<-replace(medical_raw_data$Soft_drink,is.na(medical_raw_data$Soft_drink), mode_funct(medical_raw_data$Soft_drink))
medical_raw_data$Overweight<-replace(medical_raw_data$Overweight,is.na(medical_raw_data$Overweight), mode_funct(medical_raw_data$Overweight))
                                                                    
#Proof NA are eliminated
colSums(is.na(medical_raw_data))

# Use the already-cleaned in-memory object directly
library(tidyverse)
medical_clean <- medical_raw_data
PCA_test <-medical_clean[,c(11,15:16,19,23:26,42:44)]
PCA_test1 <-prcomp(PCA_test[,c(1:11)], center=TRUE, scale. = TRUE)
PCA_test1$rotation

#Use package factoextra to run eigenvalue and scree plot
library(factoextra)
fviz_eig(PCA_test1)
fviz_eig(PCA_test1, choice = "eigenvalue", addlabels = TRUE)

sum(is.na(medical_clean))
str(PCA_test)


