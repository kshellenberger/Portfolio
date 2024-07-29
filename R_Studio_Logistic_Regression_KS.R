#Load libraries
library(summarytools)
library(ggplot2)
library(plotly)
library(mosaic)
library(caret)
library(tidyverse)
library(lessR)

# Use package tidyverse to read in the medical_raw_data.csv
med <-read.csv ("C:\\Users\\Kim\\Desktop\\D208 pt1\\med_data\\medical_clean.csv")
med

#Create a table for the count of yes/no binary dependent variable
Anxiety <- data.frame (med$Anxiety)
Anxiety_table <- table(med$Anxiety)
Anxiety_table

#Univariate for dependent variable
barchart(Anxiety_table, main = "Anxiety Univariate Chart")

#Create new table with just the Independent variables
i_var<-med[,c(43:50)]
i_var

#Change No to 0 and Yes to 1 and make the variable numeric
med$Anxiety[med$Anxiety =="No"] <- "0"
med$Anxiety[med$Anxiety =="Yes"] <- "1"
med$Anxiety <- as.numeric(med$Anxiety)

#Make anxiety a table 
table(med$Anxiety)

#Univariate stats for variables (dependent and independent)
descr(i_var)
descr(med$Anxiety)

#Univariate visualization for variables (dependent and independent)
boxplot(i_var,
        main = "Survey Responses",
        xlab = "Survey",
        ylab = "Responses")

#Create df for the 9 variables for bivariate stats
all_var = data.frame(Anxiety, i_var)

#Bivariate visualizations
mosaicplot(Item1~med.Anxiety, data = all_var, color = TRUE)
mosaicplot(Item2~med.Anxiety, data = all_var, color = TRUE)
mosaicplot(Item3~med.Anxiety, data = all_var, color = TRUE)
mosaicplot(Item4~med.Anxiety, data = all_var, color = TRUE)
mosaicplot(Item5~med.Anxiety, data = all_var, color = TRUE)
mosaicplot(Item6~med.Anxiety, data = all_var, color = TRUE)
mosaicplot(Item7~med.Anxiety, data = all_var, color = TRUE)
mosaicplot(Item8~med.Anxiety, data = all_var, color = TRUE)

#Create cleaned and wrangled df for the 9 variables
all_var_clean = data.frame(med$Anxiety, i_var)
summary (all_var_clean)
write_csv(all_var_clean, "C:\\Users\\Kim\\Desktop\\D208 pt1\\med_data\\clean_med_log_reg.csv")

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
