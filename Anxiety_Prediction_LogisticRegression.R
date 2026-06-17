################################################################################
# Project:      Anxiety Prediction - Logistic Regression
# Objective:    Predict patient anxiety status using survey response items
# Author:       Kim Shellenberger
# Date:         2024
# Description:  Binary logistic regression model to predict anxiety (Yes/No)
#               based on 8 survey response items. Includes exploratory analysis,
#               model specification, and diagnostic evaluation.
################################################################################

# SECTION 1: LOAD REQUIRED PACKAGES
library(ggplot2)       # Data visualization
library(mosaic)        # Mosaic plots for categorical data
library(caret)         # Classification and regression tools
library(tidyverse)     # Data manipulation suite
library(lessR)         # Additional statistical functions

# SECTION 2: LOAD DATA
# TODO: Update file path to your data location
med <- read.csv("medical_clean.csv")  # Load medical dataset
med

# SECTION 3: DEPENDENT VARIABLE PREPARATION
# Extract anxiety variable and examine distribution
Anxiety <- data.frame(med$Anxiety)
Anxiety_table <- table(med$Anxiety)
Anxiety_table

# Visualize dependent variable distribution
barchart(Anxiety_table, main = "Anxiety Status Distribution")

# SECTION 4: INDEPENDENT VARIABLES PREPARATION
# Extract survey response items (columns 43-50)
i_var <- med[, c(43:50)]
i_var

# SECTION 5: DATA ENCODING
# Convert binary anxiety variable to numeric (No=0, Yes=1)
med$Anxiety[med$Anxiety == "No"] <- "0"
med$Anxiety[med$Anxiety == "Yes"] <- "1"
med$Anxiety <- as.numeric(med$Anxiety)

# Verify encoding
table(med$Anxiety)

# SECTION 6: EXPLORATORY DATA ANALYSIS
# Univariate statistics for independent variables
print(summary(i_var))
print(table(med$Anxiety))

# Boxplot visualization of survey responses
boxplot(i_var,
        main = "Survey Response Distribution",
        xlab = "Survey Items",
        ylab = "Response Scale")

# SECTION 7: BIVARIATE ANALYSIS
# Combine variables for bivariate analysis
all_var <- data.frame(Anxiety, i_var)

# Mosaic plots for each survey item vs. anxiety (relationship visualization)
mosaicplot(Item1~med.Anxiety, data = all_var, color = TRUE, main = "Item 1 vs. Anxiety")
mosaicplot(Item2~med.Anxiety, data = all_var, color = TRUE, main = "Item 2 vs. Anxiety")
mosaicplot(Item3~med.Anxiety, data = all_var, color = TRUE, main = "Item 3 vs. Anxiety")
mosaicplot(Item4~med.Anxiety, data = all_var, color = TRUE, main = "Item 4 vs. Anxiety")
mosaicplot(Item5~med.Anxiety, data = all_var, color = TRUE, main = "Item 5 vs. Anxiety")
mosaicplot(Item6~med.Anxiety, data = all_var, color = TRUE, main = "Item 6 vs. Anxiety")
mosaicplot(Item7~med.Anxiety, data = all_var, color = TRUE, main = "Item 7 vs. Anxiety")
mosaicplot(Item8~med.Anxiety, data = all_var, color = TRUE, main = "Item 8 vs. Anxiety")

# SECTION 8: DATA EXPORT
# Create and save clean dataset for model building
all_var_clean <- data.frame(med$Anxiety, i_var)
summary(all_var_clean)

# TODO: Update file path to your output location
# write_csv(all_var_clean, "clean_med_log_reg.csv")

#Initial Logistic Regression
Logit (med.Anxiety ~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8, data = all_var_clean)

#Reduced model minus Item3
Logit (med.Anxiety ~ Item1 + Item2 + Item4 + Item5 + Item6 + Item7 + Item8, data = all_var_clean)

#Reduced model minus Item3, Item7
Logit (med.Anxiety ~ Item1 + Item2 + Item4 + Item5 + Item6 + Item8, data = all_var_clean)

#Reduced model minus Item3, Item7, Item1
Logit (med.Anxiety ~ Item2 + Item4 + Item5 + Item6 + Item8, data = all_var_clean)

#Reduced model minus Item3, Item7, Item1, Item4
Logit (med.Anxiety ~ Item2 + Item5 + Item6 + Item8, data = all_var_clean)

#Reduced model minus Item3, Item7, Item1, Item4, Item6
Logit (med.Anxiety ~ Item2 + Item5 + Item8, data = all_var_clean)
reduced <- glm(formula = med.Anxiety ~ Item2 + Item5 + Item8, data = all_var_clean, family = binomial())
summary(reduced)

#McFadden's R-squared value
with(summary(reduced), 1 - deviance/null.deviance)
