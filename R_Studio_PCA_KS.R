# Use package tidyverse to read in the medical_raw_data.csv
library(tidyverse)
medical_raw_data <-read.csv ("C:\\Users\\Kim\\Desktop\\D206\\e9d8sm5uf8df75k650df (2)\\medical_raw_data.csv")

#Summary of data set
str(medical_raw_data)

#Identify if there are any duplicates in the unique id fields
sum(duplicated(medical_raw_data$Customer_id))
sum(duplicated(medical_raw_data$Interaction))
sum(duplicated(medical_raw_data$UID))

#Provides the sum of entire data set and columns for NA
sum(is.na(medical_raw_data))
colSums(is.na(medical_raw_data))

# Create boxplots to identify outliers for numerical data columns 
#Get stats for each boxplot

boxplot.stats (medical_raw_data$Population)
boxplot(medical_raw_data$Population)

boxplot.stats (medical_raw_data$Additional_charges)
boxplot(medical_raw_data$Additional_charges) 

boxplot.stats (medical_raw_data$Age)
boxplot(medical_raw_data$Age)

boxplot.stats(medical_raw_data$Children)
boxplot(medical_raw_data$Children)

boxplot.stats (medical_raw_data$Income)
boxplot(medical_raw_data$Income)

boxplot.stats (medical_raw_data$VitD_levels)
boxplot(medical_raw_data$VitD_levels)

boxplot.stats (medical_raw_data$Doc_visits)
boxplot(medical_raw_data$Doc_visits)

boxplot.stats (medical_raw_data$Full_meals_eaten)
boxplot(medical_raw_data$Full_meals_eaten)

boxplot.stats (medical_raw_data$VitD_supp)
boxplot(medical_raw_data$VitD_supp)

boxplot.stats (medical_raw_data$Initial_days)
boxplot(medical_raw_data$Initial_days)

boxplot.stats (medical_raw_data$TotalCharge)
boxplot(medical_raw_data$TotalCharge)

#Make NA all consistently NA
medical_raw_data[medical_raw_data == 'NA'] <- NA

#Provides the sum of entire data set and columns for NA
sum(is.na(medical_raw_data))
colSums(is.na(medical_raw_data))



#####Fixes for the found items needing to be cleaned#####


#Turn NA into the median for the entire column
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

# Write clean data to .csv
write.csv(medical_raw_data, "C:\\Users\\Kim\\Desktop\\D206\\D206_medical_clean.csv", row.names=FALSE)

#Read in the clean .csv and limit the PCA test to the 11 necessary columns - Run PCA
library(tidyverse)
medical_clean <-read.csv ("C:\\Users\\Kim\\Desktop\\D206\\D206_medical_clean.csv")
PCA_test <-medical_clean[,c(11,15:16,19,23:26,42:44)]
PCA_test1 <-prcomp(PCA_test[,c(1:11)], center=TRUE, scale. = TRUE)
PCA_test1$rotation

#Use package factoextra to run eigenvalue and scree plot
library(factoextra)
fviz_eig(PCA_test1)
fviz_eig(PCA_test1, choice = "eigenvalue", addlabels = TRUE)

sum(is.na(medical_clean))
str(PCA_test)


