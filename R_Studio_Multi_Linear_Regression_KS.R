#Load packages

library(summarytools)
library(ggplot2)
library(plotly)
library(mosaic)
library(tidyverse)

# Use package tidyverse to read in the medical_raw_data.csv
med <-read.csv ("C:\\Users\\Kim\\Desktop\\D208 pt1\\med_data\\medical_clean.csv")

#Create new table with just the Independent variables
i_var<-med[,c(43:50)]


#Univariate stats for variables (dependent and independent)
descr(med$Income)
descr(i_var)

#Univariate visualization for variables (dependent and independent)
boxplot(i_var,
        main = "Survey Responses",
        xlab = "Survey",
        ylab = "Responses")

boxplot(med$Income,
        main = "Patient Income",
        ylab = "$"
        )

#Create df for the 9 variables 
all_var = data.frame(med$Income, i_var)
all_var
write_csv(all_var, "C:\\Users\\Kim\\Desktop\\D208 pt1\\med_data\\clean_med.csv")

#Review sample of the data and look at stats
str(all_var)
summary(all_var)
colSums(is.na(all_var))

#Bivariate visualizations
#Item 1
ggplot(all_var, 
       aes(x = Item1, y = med.Income)) +
  geom_point(color="cornflowerblue", 
             size = 1.5, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 225000)) +
  scale_x_continuous(breaks = seq(1:8), 
                     limits=c(0, 8)) + 
  labs(x = "Survey Item1",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item1")

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
  labs(x = "Survey Item2",
       y = "",
       title = "Survey Responses vs. Income",
       subtitle = "Item2")

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

#Initial Multiple linear regression model
fit1 <- lm(med.Income ~ Item1 + Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8, data = all_var )
summary(fit1)
plot(fit1)

#Reduced model (remove Item1)
fit2 <- lm(med.Income ~ Item2 + Item3 + Item4 + Item5 + Item6 + Item7 + Item8, data = all_var )
summary(fit2)

#Reduced model (remove Item1, Item6)
fit3 <- lm(med.Income ~ Item2 + Item3 + Item4 + Item5 + Item7 + Item8, data = all_var )
summary(fit3)

#Reduced model (remove Item1, Item6, Item3)
fit4 <- lm(med.Income ~ Item2 + Item4 + Item5 + Item7 + Item8, data = all_var )
summary(fit4)

#Reduced model (remove Item1, Item6, Item3, Item7)
fit5 <- lm(med.Income ~ Item2 + Item4 + Item5 + Item8, data = all_var )
summary(fit5)

#Reduced model (remove Item1, Item6, Item3, Item7, Item5)
fit6 <- lm(med.Income ~ Item2 + Item4 + Item8, data = all_var )
summary(fit6)
plot(fit6)

