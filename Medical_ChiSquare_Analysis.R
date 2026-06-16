################################################################################
# Project:      Medical Chi-Square Analysis
# Objective:    Test association between marital status and complication risk
# Author:       Kim Shellenberger
# Date:         2024
# Description:  Analyzes relationship between patient marital status and
#               complication risk using Chi-Square test of independence.
################################################################################

# SECTION 1: LOAD REQUIRED PACKAGES
# Note: Install packages manually if needed using install.packages()
library(ggplot2)       # Data visualization
library(dvmisc)        # Statistical utilities        # Statistical utilities
library(infer)         # Statistical inference
library(plyr)          # Data manipulation
library(pastecs)       # Descriptive statistics
library(plotly)        # Interactive visualizations
library(summarytools)  # Summary statistics
library(tidyverse)     # Data manipulation suite
library(dplyr)         # Data frame operations

# SECTION 2: LOAD DATA
# TODO: Update file path to your data location
med <- read.csv("medical_clean.csv")  # Load medical dataset

# SECTION 3: EXPLORATORY DATA ANALYSIS
# Display dataset structure and summary statistics
str(med)
view(dfSummary(med))

# SECTION 4: VISUALIZATION
# Create proportional bar chart showing relationship
ggplot(med, aes(Marital, fill = Complication_risk)) + 
  geom_bar(position = "fill") +
  labs(title = "Marital Status vs. Complication Risk",
       x = "Marital Status",
       y = "Proportion")

# SECTION 5: CONTINGENCY TABLE
# Create count table for categorical variables
c_var <- table(med$Marital, med$Complication_risk)
c_var
summary.table(c_var)

# SECTION 6: CHI-SQUARE TEST OF INDEPENDENCE
# Test null hypothesis: marital status and complication risk are independent
results <- chisq.test(c_var)
results
# Interpret: p-value < 0.05 suggests variables are associated

# SECTION 7: SUPPORTING UNIVARIATE ANALYSES
# Bar chart for Marital Status distribution
med %>%
  count(Marital) %>%
  plot_ly(x = ~Marital, y = ~n) %>%
  add_bars()

# Bar chart for Initial Administration distribution
med %>%
  count(Initial_admin) %>%
  plot_ly(x = ~Initial_admin, y = ~n) %>%
  add_bars()

# SECTION 8: CONTINUOUS VARIABLE ANALYSIS
# Boxplot and statistics for Age (outlier detection)
boxplot(med$Age, main = "Age Distribution", ylab = "Years")
boxplot.stats(med$Age)
descr(med$Age)

# Boxplot and statistics for Doctor Visits
boxplot(med$Doc_visits, main = "Doctor Visits Distribution", ylab = "Visits")
boxplot.stats(med$Doc_visits)

# SECTION 9: INCOME STATISTICS
stat.desc(med$Income)
summary(med$Age)

med %>%
  plot_ly(x = ~Income, y = ~Age) %>%
  add_markers()

med %>%
  count(Marital, Complication_risk) %>%
 plot_ly (x = ~Marital, y = ~n, color = ~Complication_risk) %>%
  add_bars() %>%
layout(barmode = "stack")

#Two continuous variable descriptive stats for bivariate

#Group one veriable by quantile
Quantile_Age <- quant_groups(med$Age, groups = 4, probs = NULL, quantile.list = NULL,
             cut.list = NULL)

#View new grouped data
Quantile_Age
str(Quantile_Age)

#Create new table with grouped data and other continous variable
bivar_continuous = data.frame(Quantile_Age, med$Income)

#View new table 
bivar_continuous

#Descriptive statistics table for bivariate continuous variable grouped by the grouped variable

###   https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html   6.1 Special Case of descr() with stby()
#When used to produce split-group statistics for a single variable, stby() assembles everything into a single table instead of displaying a series of one-column tables.

#with(tobacco, 
#     stby(data    = BMI, 
#          INDICES = age.gr, 
#          FUN     = descr,      ####

with(bivar_continuous,
     stby(data = med$Income,
          INDICES = Quantile_Age,
          FUN = descr))