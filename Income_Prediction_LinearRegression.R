################################################################################
# Project:      Income Prediction - Multiple Linear Regression
# Objective:    Predict patient income using survey response items
# Author:       Kim Shellenberger
# Date:         2024
# Description:  Multiple linear regression model to predict income based on
#               8 survey response items. Includes exploratory analysis,
#               model diagnostics, and visualization of relationships.
################################################################################

# SECTION 1: LOAD REQUIRED PACKAGES
library(summarytools)  # Summary statistics
library(ggplot2)       # Data visualization
library(plotly)        # Interactive plots
library(mosaic)        # Mosaic plots for categorical data
library(tidyverse)     # Data manipulation suite

# SECTION 2: LOAD DATA
# TODO: Update file path to your data location
med <- read.csv("medical_clean.csv")  # Load medical dataset

# SECTION 3: INDEPENDENT VARIABLES PREPARATION
# Extract survey response items (columns 43-50)
i_var <- med[, c(43:50)]


# SECTION 4: EXPLORATORY DATA ANALYSIS
# Univariate statistics for dependent variable (Income)
descr(med$Income)
descr(i_var)

# SECTION 5: DATA VISUALIZATION
# Boxplot of survey responses distribution
boxplot(i_var,
        main = "Survey Response Items Distribution",
        xlab = "Survey Items",
        ylab = "Response Scale")

# Boxplot of income distribution
boxplot(med$Income,
        main = "Patient Income Distribution",
        ylab = "Annual Income ($)")

# SECTION 6: DATA PREPARATION
# Combine dependent and independent variables
all_var <- data.frame(med$Income, i_var)
all_var

# TODO: Update file path to your output location
# write_csv(all_var, "clean_med_regression.csv")

# SECTION 7: DATA QUALITY CHECKS
# Examine data structure and missing values
str(all_var)
summary(all_var)
colSums(is.na(all_var))  # Check for missing data

# SECTION 8: BIVARIATE VISUALIZATIONS
# Scatter plots to examine relationships between each survey item and income
# Visual inspection helps identify linear relationships and outliers
# Dollar formatting on y-axis improves readability

# Bivariate visualization - Survey Item 1 vs. Income
ggplot(all_var, 
       aes(x = Item1, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item 1 Response\",
       y = "Annual Income\",
       title = \"Survey Responses vs. Income\",
       subtitle = \"Item 1 - Interpretation of slope indicates effect on income\")"

#Item 2
ggplot(all_var, 
       aes(x = Item2, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item 2 Response",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item 2")

#Item 3
ggplot(all_var, 
       aes(x = Item3, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item3",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item3")

#Item 4
ggplot(all_var, 
       aes(x = Item4, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item4",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item4")

#Item 5
ggplot(all_var, 
       aes(x = Item5, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item5",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item5")

#Item 6
ggplot(all_var, 
       aes(x = Item6, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item6",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item6")

#Item 7
ggplot(all_var, 
       aes(x = Item7, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item7",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item7")

#Item 8
ggplot(all_var, 
       aes(x = Item8, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item8",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item8")

# SECTION 9: MULTIPLE LINEAR REGRESSION MODELING
# Objective: Build model to predict income using survey responses
# Approach: Iterative backward elimination to identify significant predictors

#Initial Multiple linear regression model
# Full model with all 8 survey items as predictors
# Each coefficient represents the effect of 1-unit increase in survey response on annual income
fit1 <- lm(med.Income ~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8, data = all_var )
summary(fit1)  # Review: R-squared, F-statistic, individual p-values
plot(fit1)     # Diagnostic plots: residuals, Q-Q plot, scale-location, leverage

# SECTION 10: MODEL REFINEMENT - BACKWARD ELIMINATION
# Remove non-significant variables (p > 0.05) iteratively
# Goal: Maximize adjusted R-squared while minimizing model complexity

#Reduced model (remove Item1)
fit2 <- lm(med.Income ~ Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8, data = all_var )
summary(fit2)  # Compare adjusted R-squared and AIC

#Reduced model (remove Item1, Item6)
fit3 <- lm(med.Income ~ Item2 + Item3 + Item4 + Item5 + Item7 + Item8, data = all_var )
summary(fit3)  # Model improvement?

#Reduced model (remove Item1, Item6, Item3)
fit4 <- lm(med.Income ~ Item2 + Item4 + Item5 + Item7 + Item8, data = all_var )
summary(fit4)

#Reduced model (remove Item1, Item6, Item3, Item7)
fit5 <- lm(med.Income ~ Item2 + Item4 + Item5 + Item8, data = all_var )
summary(fit5)

# SECTION 11: FINAL MODEL
# Parsimonious model with optimal balance of simplicity and explanatory power
#Reduced model (remove Item1, Item6, Item3, Item7, Item5)
fit6 <- lm(med.Income ~ Item2 + Item4 + Item8, data = all_var )
summary(fit6)  # Final model coefficients, p-values, and R-squared
plot(fit6)     # Diagnostic plots for final model assumptions

