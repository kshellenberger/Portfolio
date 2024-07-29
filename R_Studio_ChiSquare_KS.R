#Load packages
library(ggplot2)
install.packages("dvmisc")
library(dvmisc)
library(infer)
library(plyr)
install.packages("pastecs")
library(pastecs)
library(plotly)
install.packages("summarytools")
library(summarytools)
library(tidyverse)
library(dplyr)

#Read in the .csv
med <-read.csv ("C:\\Users\\Kim\\Desktop\\D207\\medical_clean.csv")

#Review sample of the data
str(med)
view(dfSummary(med))

#Proportional bar chart
ggplot(med, aes(Marital, fill = Complication_risk)) + geom_bar(position = "fill")

#Create count table for the 2 variables ratios
c_var = data.frame(med$Marital, med$Complication_risk)
c_var = table(med$Marital, med$Complication_risk)
c_var

summary.table (c_var)

#Chisq_test
results <- c_var %>% 
  chisq.test (c_var)

#See results
results

#Univariate bar charts for categoricals
med %>%
  count(Marital) %>%
  plot_ly(x = ~Marital, y = ~n) %>%
  add_bars()

med %>%
  count(Initial_admin) %>%
  plot_ly(x = ~Initial_admin, y = ~n) %>%
  add_bars()

#Univariate boxplots/stats for continous variables
boxplot(med$Age,
        main = "Age",
        ylab = "Age")
boxplot.stats(med$Age)

descr(med$Age)

boxplot(med$Doc_visits,
        main = "Doctor Visits",
        ylab = "Visits" )
boxplot.stats(med$Doc_visits)


#Bivariate Scatterplots for continuous variables (income and age)
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