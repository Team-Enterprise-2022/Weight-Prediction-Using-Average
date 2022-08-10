# Packages that may or may not be used at some point in this program
library(AppliedPredictiveModeling)
library(caret)
library(e1071)
library(tidymodels)
library(magrittr)
library(dplyr)
library(rsample)
library(recipes)
library(corrplot)
library(readxl)
library(tidyverse)
library(vip)
library(rpart)
library(rpart.plot)
library(ranger)
library(tune)
library(dials)
library(parsnip)
library(workflows)
library(yardstick)
library(pROC)
library(readr)
library(lubridate)

# Read in the input dataset
MERGED_with_WEIGHTS <- read_csv("Grad School/CAPSTONE/MERGED_with_WEIGHTS_elliot.csv")
View(MERGED_with_WEIGHTS)

# Perform various data and attribution manipulations
MERGED_with_WEIGHTS$Date_Char <- as.character(MERGED_with_WEIGHTS$Date)  
MERGED_with_WEIGHTS$Year <- substr(MERGED_with_WEIGHTS$Date_Char,1,4)
MERGED_with_WEIGHTS$X61and107pilot_weights <- MERGED_with_WEIGHTS$`61and107pilot_weights`
MERGED_with_WEIGHTS$X107pilot_weights <- MERGED_with_WEIGHTS$`107pilot_weights`
MERGED_with_WEIGHTS$X107uas_weights <- MERGED_with_WEIGHTS$`107uas_weights`

# Attribute statistics
unique(MERGED_with_WEIGHTS[c("Year")])
colnames(MERGED_with_WEIGHTS)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#HOBBY WEIGHTS ACCURACY MEASURE 1 (2015-2019)
# Create the training set
HOBBY_WEIGHTS_SUBSET_TRAINING <- subset(MERGED_with_WEIGHTS, Year=="2015" | Year=="2016" | Year=="2017" | Year=="2018" | Year=="2019",
                                    select=c(Date_Char, Year, Zip.Code, hobby_weights))
# Create the average target weights by zip code
Mean_Hobby_Weights_By_Zip_TRAINING <- HOBBY_WEIGHTS_SUBSET_TRAINING %>% group_by(Zip.Code) %>% 
    summarise_at(vars(hobby_weights), list(Average_Hobby_Weight=mean))
# Merge the average target weights into the training set by zip code
HOBBY_WEIGHTS_TRAINING <- merge(HOBBY_WEIGHTS_SUBSET_TRAINING, Mean_Hobby_Weights_By_Zip_TRAINING, by="Zip.Code") 
# Calculate the error for the training set
HOBBY_WEIGHTS_TRAINING$ERROR <- abs(HOBBY_WEIGHTS_TRAINING$hobby_weights - HOBBY_WEIGHTS_TRAINING$Average_Hobby_Weight)

# Create the test set
HOBBY_WEIGHTS_SUBSET_TESTING <- subset(MERGED_with_WEIGHTS, Year=="2020",
                                        select=c(Date_Char, Year, Zip.Code, hobby_weights))
# Merge the average target weights into the test set by zip code
HOBBY_WEIGHTS_TESTING <- merge(HOBBY_WEIGHTS_SUBSET_TESTING, Mean_Hobby_Weights_By_Zip_TRAINING, by="Zip.Code") 
# Calculate the error for the test set
HOBBY_WEIGHTS_TESTING$ERROR <- abs(HOBBY_WEIGHTS_TESTING$hobby_weights - HOBBY_WEIGHTS_TESTING$Average_Hobby_Weight)

#-------------------------------------------------------------------------------
#HOBBY WEIGHTS R-SQUARED - MEASURE 1
# Calculate the R-squared for the training set
R2_HW_TRAINING1 <- (cor(HOBBY_WEIGHTS_TRAINING$hobby_weights, HOBBY_WEIGHTS_TRAINING$Average_Hobby_Weight))^2
R2_HW_TRAINING1

# Calculate the R-squared for the test set
R2_HW_TESTING1 <- (cor(HOBBY_WEIGHTS_TESTING$hobby_weights, HOBBY_WEIGHTS_TESTING$Average_Hobby_Weight))^2
R2_HW_TESTING1

#-------------------------------------------------------------------------------
#HOBBY WEIGHTS MSE/MAE/RMSE - MEASURE 1
# Calculate the total amount of records/rows for the training set
n_hb_TRAINING1 <- nrow(HOBBY_WEIGHTS_TRAINING)
n_hb_TRAINING1

# Calculate the mean square error for the training set
HOBBY_WEIGHTS_TRAINING$ERROR_SQ <- HOBBY_WEIGHTS_TRAINING$ERROR^2
MSE_HB_1_TRAINING <- sum(HOBBY_WEIGHTS_TRAINING$ERROR_SQ)*(1/n_hb_TRAINING1) 
MSE_HB_1_TRAINING

# Calculate the mean absolute error for the training set
MAE_HB_1_TRAINING <- sum(HOBBY_WEIGHTS_TRAINING$ERROR)*(1/n_hb_TRAINING1)
MAE_HB_1_TRAINING

# Calculate the root mean square error for the training set
RMSE_HB_1_TRAINING <- (sum(HOBBY_WEIGHTS_TRAINING$ERROR_SQ)/n_hb_TRAINING1)^(1/2)
RMSE_HB_1_TRAINING

#-------------------------------------------------------------------------------
# Calculate the total amount of records/rows for the test set
n_hb_TESTING1 <- nrow(HOBBY_WEIGHTS_TESTING)
n_hb_TESTING1

# Calculate the mean square error for the test set
HOBBY_WEIGHTS_TESTING$ERROR_SQ <- HOBBY_WEIGHTS_TESTING$ERROR^2
MSE_HB_1_TESTING <- sum(HOBBY_WEIGHTS_TESTING$ERROR_SQ)*(1/n_hb_TESTING1) 
MSE_HB_1_TESTING

# Calculate the mean absolute error for the test set
MAE_HB_1_TESTING <- sum(HOBBY_WEIGHTS_TESTING$ERROR)*(1/n_hb_TESTING1)
MAE_HB_1_TESTING

# Calculate the root mean square error for the test set
RMSE_HB_1_TESTING <- (sum(HOBBY_WEIGHTS_TESTING$ERROR_SQ)/n_hb_TESTING1)^(1/2)
RMSE_HB_1_TESTING

#-------------------------------------------------------------------------------
#HOBBY WEIGHTS ACCURACY MEASURE 2 (2015-2020)
# Create the training set
HOBBY_WEIGHTS_SUBSET_TRAINING_2 <- subset(MERGED_with_WEIGHTS, Year=="2015" | Year=="2016" | Year=="2017" | Year=="2018" | Year=="2019" | Year=="2020",
                                        select=c(Date_Char, Year, Zip.Code, hobby_weights))
# Create the average target weights by zip code
Mean_Hobby_Weights_By_Zip_TRAINING_2 <- HOBBY_WEIGHTS_SUBSET_TRAINING_2 %>% group_by(Zip.Code) %>% 
    summarise_at(vars(hobby_weights), list(Average_Hobby_Weight=mean))
# Merge the average target weights into the training set by zip code
HOBBY_WEIGHTS_TRAINING_2 <- merge(HOBBY_WEIGHTS_SUBSET_TRAINING_2, Mean_Hobby_Weights_By_Zip_TRAINING_2, by="Zip.Code") 
# Calculate the error for the training set
HOBBY_WEIGHTS_TRAINING_2$ERROR <- abs(HOBBY_WEIGHTS_TRAINING_2$hobby_weights - HOBBY_WEIGHTS_TRAINING_2$Average_Hobby_Weight)

# Create the test set
HOBBY_WEIGHTS_SUBSET_TESTING_2 <- subset(MERGED_with_WEIGHTS, Year=="2021",
                                       select=c(Date_Char, Year, Zip.Code, hobby_weights))
# Merge the average target weights into the test set by zip code
HOBBY_WEIGHTS_TESTING_2 <- merge(HOBBY_WEIGHTS_SUBSET_TESTING_2, Mean_Hobby_Weights_By_Zip_TRAINING_2, by="Zip.Code") 
# Calculate the error for the test set
HOBBY_WEIGHTS_TESTING_2$ERROR <- abs(HOBBY_WEIGHTS_TESTING_2$hobby_weights - HOBBY_WEIGHTS_TESTING_2$Average_Hobby_Weight)

#-------------------------------------------------------------------------------
#HOBBY WEIGHTS R-SQUARED - MEASURE 2
# Calculate the R-squared for the training set
R2_HW_TRAINING2 <- (cor(HOBBY_WEIGHTS_TRAINING_2$hobby_weights, HOBBY_WEIGHTS_TRAINING_2$Average_Hobby_Weight))^2
R2_HW_TRAINING2

# Calculate the R-squared for the test set
R2_HW_TESTING2 <- (cor(HOBBY_WEIGHTS_TESTING_2$hobby_weights, HOBBY_WEIGHTS_TESTING_2$Average_Hobby_Weight))^2
R2_HW_TESTING2

#-------------------------------------------------------------------------------
#HOBBY WEIGHTS MSE/MAE/RMSE - MEASURE 2
# Calculate the total amount of records/rows for the training set
n_hb_TRAINING2 <- nrow(HOBBY_WEIGHTS_TRAINING_2)
n_hb_TRAINING2

# Calculate the mean square error for the training set
HOBBY_WEIGHTS_TRAINING_2$ERROR_SQ <- HOBBY_WEIGHTS_TRAINING_2$ERROR^2
MSE_HB_2_TRAINING <- sum(HOBBY_WEIGHTS_TRAINING_2$ERROR_SQ)*(1/n_hb_TRAINING2) 
MSE_HB_2_TRAINING

# Calculate the mean absolute error for the training set
MAE_HB_2_TRAINING <- sum(HOBBY_WEIGHTS_TRAINING_2$ERROR)*(1/n_hb_TRAINING2)
MAE_HB_2_TRAINING

# Calculate the root mean square error for the training set
RMSE_HB_2_TRAINING <- (sum(HOBBY_WEIGHTS_TRAINING_2$ERROR_SQ)/n_hb_TRAINING2)^(1/2)
RMSE_HB_2_TRAINING

#-------------------------------------------------------------------------------
# Calculate the total amount of records/rows for the test set
n_hb_TESTING2 <- nrow(HOBBY_WEIGHTS_TESTING_2)
n_hb_TESTING2

# Calculate the mean square error for the test set
HOBBY_WEIGHTS_TESTING_2$ERROR_SQ <- HOBBY_WEIGHTS_TESTING_2$ERROR^2
MSE_HB_2_TESTING <- sum(HOBBY_WEIGHTS_TESTING_2$ERROR_SQ)*(1/n_hb_TESTING2) 
MSE_HB_2_TESTING

# Calculate the mean absolute error for the test set
MAE_HB_2_TESTING <- sum(HOBBY_WEIGHTS_TESTING_2$ERROR)*(1/n_hb_TESTING2)
MAE_HB_2_TESTING

# Calculate the root mean square error for the test set
RMSE_HB_2_TESTING <- (sum(HOBBY_WEIGHTS_TESTING_2$ERROR_SQ)/n_hb_TESTING2)^(1/2)
RMSE_HB_2_TESTING

#-------------------------------------------------------------------------------
#AVERAGE HOBBY WEIGHTS MSE/MAE/RMSE
# Average R-squared values are calculated from the Measure 1 and 2 test sets
R2_AVERAGE_HB_TEST <- (R2_HW_TESTING1 + R2_HW_TESTING2)/2
R2_AVERAGE_HB_TEST

# Average mean square error values are calculated from the Measure 1 and 2 test sets
MSE_AVERAGE_HB_TEST <- (MSE_HB_1_TESTING + MSE_HB_2_TESTING)/2
MSE_AVERAGE_HB_TEST

# Average mean absolute error values are calculated from the Measure 1 and 2 test sets
MAE_AVERAGE_HB_TEST <- (MAE_HB_1_TESTING + MAE_HB_2_TESTING)/2
MAE_AVERAGE_HB_TEST

# Average root mean square error values are calculated from the Measure 1 and 2 test sets
RMSE_AVERAGE_HB_TEST <- (RMSE_HB_1_TESTING + RMSE_HB_2_TESTING)/2
RMSE_AVERAGE_HB_TEST

# An array with the average accuracy statistics is created
HBStats <- c(R2_AVERAGE_HB_TEST, MSE_AVERAGE_HB_TEST, MAE_AVERAGE_HB_TEST, RMSE_AVERAGE_HB_TEST)
HBStats_Array <- array(HBStats)
dimnames(HBStats_Array)=list( c("Average R-Squared", "Average MSE", "Average MAE", "Average RMSE"))
HBStats_Array

#----------------------------------------------------------------------------------------------------------------------

 

#----------------------------------------------------------------------------------------------------------------------
#PILOT PART 107 ACCURACY MEASURE 1 (2015-2019) 
# Create the training set
PILOT_107_SUBSET_TRAINING <- subset(MERGED_with_WEIGHTS, Year=="2015" | Year=="2016" | Year=="2017" | Year=="2018" | Year=="2019",
                                        select=c(Date_Char, Year, Zip.Code, X107pilot_weights))
# Create the average target weights by zip code
Mean_P107_Weights_By_Zip_TRAINING <- PILOT_107_SUBSET_TRAINING %>% group_by(Zip.Code) %>% 
    summarise_at(vars(X107pilot_weights), list(Average_P107_Weight=mean))
# Merge the average target weights into the training set by zip code
PILOT_107_TRAINING <- merge(PILOT_107_SUBSET_TRAINING, Mean_P107_Weights_By_Zip_TRAINING, by="Zip.Code") 
# Calculate the error for the training set
PILOT_107_TRAINING$ERROR <- abs(PILOT_107_TRAINING$X107pilot_weights - PILOT_107_TRAINING$Average_P107_Weight)

# Create the test set
PILOT_107_SUBSET_TESTING <- subset(MERGED_with_WEIGHTS, Year=="2020",
                                    select=c(Date_Char, Year, Zip.Code, X107pilot_weights))
# Merge the average target weights into the test set by zip code
PILOT_107_TESTING <- merge(PILOT_107_SUBSET_TESTING, Mean_P107_Weights_By_Zip_TRAINING, by="Zip.Code") 
# Calculate the error for the test set
PILOT_107_TESTING$ERROR <- abs(PILOT_107_TESTING$X107pilot_weights - PILOT_107_TESTING$Average_P107_Weight)

#-------------------------------------------------------------------------------
#PILOT 107 WEIGHTS R-SQUARED - MEASURE 1
# Calculate the R-squared for the training set
R2_PP107_TRAINING1 <- (cor(PILOT_107_TRAINING$X107pilot_weights, PILOT_107_TRAINING$Average_P107_Weight))^2
R2_PP107_TRAINING1

# Calculate the R-squared for the test set
R2_PP107_TESTING1 <- (cor(PILOT_107_TESTING$X107pilot_weights, PILOT_107_TESTING$Average_P107_Weight))^2
R2_PP107_TESTING1

#-------------------------------------------------------------------------------
#PILOT 107 WEIGHTS MSE/MAE/RMSE - MEASURE 1
# Calculate the total amount of records/rows for the training set
n_P107_TRAINING1 <- nrow(PILOT_107_TRAINING)
n_P107_TRAINING1

# Calculate the mean square error for the training set
PILOT_107_TRAINING$ERROR_SQ <- PILOT_107_TRAINING$ERROR^2
MSE_P107_1_TRAINING <- sum(PILOT_107_TRAINING$ERROR_SQ)*(1/n_P107_TRAINING1) 
MSE_P107_1_TRAINING

# Calculate the mean absolute error for the training set
MAE_P107_1_TRAINING <- sum(PILOT_107_TRAINING$ERROR)*(1/n_P107_TRAINING1)
MAE_P107_1_TRAINING

# Calculate the root mean square error for the training set
RMSE_P107_1_TRAINING <- (sum(PILOT_107_TRAINING$ERROR_SQ)/n_P107_TRAINING1)^(1/2)
RMSE_P107_1_TRAINING

#-------------------------------------------------------------------------------
# Calculate the total amount of records/rows for the test set
n_P107_TESTING1 <- nrow(PILOT_107_TESTING)
n_P107_TESTING1

# Calculate the mean square error for the test set
PILOT_107_TESTING$ERROR_SQ <- PILOT_107_TESTING$ERROR^2
MSE_P107_1_TESTING <- sum(PILOT_107_TESTING$ERROR_SQ)*(1/n_P107_TESTING1) 
MSE_P107_1_TESTING

# Calculate the mean absolute error for the test set
MAE_P107_1_TESTING <- sum(PILOT_107_TESTING$ERROR)*(1/n_P107_TESTING1)
MAE_P107_1_TESTING

# Calculate the root mean square error for the test set
RMSE_P107_1_TESTING <- (sum(PILOT_107_TESTING$ERROR_SQ)/n_P107_TESTING1)^(1/2)
RMSE_P107_1_TESTING

#-------------------------------------------------------------------------------
#PILOT PART 107 ACCURACY MEASURE 2 (2015-2020)
# Create the training set
PILOT_107_SUBSET_TRAINING_2 <- subset(MERGED_with_WEIGHTS, Year=="2015" | Year=="2016" | Year=="2017" | Year=="2018" | Year=="2019" | Year=="2020",
                                    select=c(Date_Char, Year, Zip.Code, X107pilot_weights))
# Create the average target weights by zip code
Mean_P107_Weights_By_Zip_TRAINING_2 <- PILOT_107_SUBSET_TRAINING_2 %>% group_by(Zip.Code) %>% 
    summarise_at(vars(X107pilot_weights), list(Average_P107_Weight=mean))
# Merge the average target weights into the training set by zip code
PILOT_107_TRAINING_2 <- merge(PILOT_107_SUBSET_TRAINING_2, Mean_P107_Weights_By_Zip_TRAINING_2, by="Zip.Code") 
# Calculate the error for the training set
PILOT_107_TRAINING_2$ERROR <- abs(PILOT_107_TRAINING_2$X107pilot_weights - PILOT_107_TRAINING_2$Average_P107_Weight)

# Create the test set
PILOT_107_SUBSET_TESTING_2 <- subset(MERGED_with_WEIGHTS, Year=="2021",
                                   select=c(Date_Char, Year, Zip.Code, X107pilot_weights))
# Merge the average target weights into the test set by zip code
PILOT_107_TESTING_2 <- merge(PILOT_107_SUBSET_TESTING_2, Mean_P107_Weights_By_Zip_TRAINING_2, by="Zip.Code") 
# Calculate the error for the test set
PILOT_107_TESTING_2$ERROR <- abs(PILOT_107_TESTING_2$X107pilot_weights - PILOT_107_TESTING_2$Average_P107_Weight)

#-------------------------------------------------------------------------------
#PILOT 107 WEIGHTS R SQUARED - MEASURE 2
# Calculate the R-squared for the training set
R2_PP107_TRAINING2 <- (cor(PILOT_107_TRAINING_2$X107pilot_weights, PILOT_107_TRAINING_2$Average_P107_Weight))^2
R2_PP107_TRAINING2

# Calculate the R-squared for the test set
R2_PP107_TESTING2 <- (cor(PILOT_107_TESTING_2$X107pilot_weights, PILOT_107_TESTING_2$Average_P107_Weight))^2
R2_PP107_TESTING2

#-------------------------------------------------------------------------------
#PILOT 107 WEIGHTS MSE/MAE/RMSE - MEASURE 2
# Calculate the total amount of records/rows for the training set
n_P107_TRAINING2 <- nrow(PILOT_107_TRAINING_2)
n_P107_TRAINING2

# Calculate the mean square error for the training set
PILOT_107_TRAINING_2$ERROR_SQ <- PILOT_107_TRAINING_2$ERROR^2
MSE_P107_2_TRAINING <- sum(PILOT_107_TRAINING_2$ERROR_SQ)*(1/n_P107_TRAINING2) 
MSE_P107_2_TRAINING

# Calculate the mean absolute error for the training set
MAE_P107_2_TRAINING <- sum(PILOT_107_TRAINING_2$ERROR)*(1/n_P107_TRAINING2)
MAE_P107_2_TRAINING

# Calculate the root mean square error for the training set
RMSE_P107_2_TRAINING <- (sum(PILOT_107_TRAINING_2$ERROR_SQ)/n_P107_TRAINING2)^(1/2)
RMSE_P107_2_TRAINING

#-------------------------------------------------------------------------------
# Calculate the total amount of records/rows for the test set
n_P107_TESTING2 <- nrow(PILOT_107_TESTING_2)
n_P107_TESTING2

# Calculate the mean square error for the test set
PILOT_107_TESTING_2$ERROR_SQ <- PILOT_107_TESTING_2$ERROR^2
MSE_P107_2_TESTING <- sum(PILOT_107_TESTING_2$ERROR_SQ)*(1/n_P107_TESTING2) 
MSE_P107_2_TESTING

# Calculate the mean absolute error for the test set
MAE_P107_2_TESTING <- sum(PILOT_107_TESTING_2$ERROR)*(1/n_P107_TESTING2)
MAE_P107_2_TESTING

# Calculate the root mean square error for the test set
RMSE_P107_2_TESTING <- (sum(PILOT_107_TESTING_2$ERROR_SQ)/n_P107_TESTING2)^(1/2)
RMSE_P107_2_TESTING

#-------------------------------------------------------------------------------
#AVERAGE PILOT 107 WEIGHTS MSE/MAE/RMSE
# Average R-squared values are calculated from the Measure 1 and 2 test sets
R2_AVERAGE_P107_TEST <- (R2_PP107_TESTING1 + R2_PP107_TESTING2)/2
R2_AVERAGE_P107_TEST

# Average mean square error values are calculated from the Measure 1 and 2 test sets
MSE_AVERAGE_P107_TEST <- (MSE_P107_1_TESTING + MSE_P107_2_TESTING)/2
MSE_AVERAGE_P107_TEST

# Average mean absolute error values are calculated from the Measure 1 and 2 test sets
MAE_AVERAGE_P107_TEST <- (MAE_P107_1_TESTING + MAE_P107_2_TESTING)/2
MAE_AVERAGE_P107_TEST

# Average root mean square error values are calculated from the Measure 1 and 2 test sets
RMSE_AVERAGE_P107_TEST <- (RMSE_P107_1_TESTING + RMSE_P107_2_TESTING)/2
RMSE_AVERAGE_P107_TEST

# An array with the average accuracy statistics is created
P107Stats <- c(R2_AVERAGE_P107_TEST, MSE_AVERAGE_P107_TEST, MAE_AVERAGE_P107_TEST, RMSE_AVERAGE_P107_TEST)
P107Stats_Array <- array(P107Stats)
dimnames(P107Stats_Array)=list( c("Average R-Squared", "Average MSE", "Average MAE", "Average RMSE"))
P107Stats_Array

#----------------------------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------------------------
#PILOT PART 61 & 107 ACCURACY MEASURE 1 (2015-2019) 
# Create the training set
PILOT_61and107_SUBSET_TRAINING <- subset(MERGED_with_WEIGHTS, Year=="2015" | Year=="2016" | Year=="2017" | Year=="2018" | Year=="2019",
                                    select=c(Date_Char, Year, Zip.Code, X61and107pilot_weights))
# Create the average target weights by zip code
Mean_P61and107_Weights_By_Zip_TRAINING <- PILOT_61and107_SUBSET_TRAINING %>% group_by(Zip.Code) %>% 
    summarise_at(vars(X61and107pilot_weights), list(Average_P61and107_Weight=mean))
# Merge the average target weights into the training set by zip code
PILOT_61and107_TRAINING <- merge(PILOT_61and107_SUBSET_TRAINING, Mean_P61and107_Weights_By_Zip_TRAINING, by="Zip.Code") 
# Calculate the error for the training set
PILOT_61and107_TRAINING$ERROR <- abs(PILOT_61and107_TRAINING$X61and107pilot_weights - PILOT_61and107_TRAINING$Average_P61and107_Weight)

# Create the test set
PILOT_61and107_SUBSET_TESTING <- subset(MERGED_with_WEIGHTS, Year=="2020",
                                   select=c(Date_Char, Year, Zip.Code, X61and107pilot_weights))
# Merge the average target weights into the test set by zip code
PILOT_61and107_TESTING <- merge(PILOT_61and107_SUBSET_TESTING, Mean_P61and107_Weights_By_Zip_TRAINING, by="Zip.Code") 
# Calculate the error for the test set
PILOT_61and107_TESTING$ERROR <- abs(PILOT_61and107_TESTING$X61and107pilot_weights - PILOT_61and107_TESTING$Average_P61and107_Weight)

#-------------------------------------------------------------------------------
#PILOT 61 AND 107 WEIGHTS R SQUARED - MEASURE 1
# Calculate the R-squared for the training set
R2_PP61and107_TRAINING1 <- (cor(PILOT_61and107_TRAINING$X61and107pilot_weights, PILOT_61and107_TRAINING$Average_P61and107_Weight))^2
R2_PP61and107_TRAINING1

# Calculate the R-squared for the test set
R2_PP61and107_TESTING1 <- (cor(PILOT_61and107_TESTING$X61and107pilot_weights, PILOT_61and107_TESTING$Average_P61and107_Weight))^2
R2_PP61and107_TESTING1

#-------------------------------------------------------------------------------
#PILOT 61 AND 107 WEIGHTS MSE/MAE/RMSE - MEASURE 1
# Calculate the total amount of records/rows for the training set
n_P61and107_TRAINING1 <- nrow(PILOT_61and107_TRAINING)
n_P61and107_TRAINING1

# Calculate the mean square error for the training set
PILOT_61and107_TRAINING$ERROR_SQ <- PILOT_61and107_TRAINING$ERROR^2
MSE_P61and107_1_TRAINING <- sum(PILOT_61and107_TRAINING$ERROR_SQ)*(1/n_P61and107_TRAINING1) 
MSE_P61and107_1_TRAINING

# Calculate the mean absolute error for the training set
MAE_P61and107_1_TRAINING <- sum(PILOT_61and107_TRAINING$ERROR)*(1/n_P61and107_TRAINING1)
MAE_P61and107_1_TRAINING

# Calculate the root mean square error for the training set
RMSE_P61and107_1_TRAINING <- (sum(PILOT_61and107_TRAINING$ERROR_SQ)/n_P61and107_TRAINING1)^(1/2)
RMSE_P61and107_1_TRAINING

#-------------------------------------------------------------------------------
# Calculate the total amount of records/rows for the test set
n_P61and107_TESTING1 <- nrow(PILOT_61and107_TESTING)
n_P61and107_TESTING1

# Calculate the mean square error for the test set
PILOT_61and107_TESTING$ERROR_SQ <- PILOT_61and107_TESTING$ERROR^2
MSE_P61and107_1_TESTING <- sum(PILOT_61and107_TESTING$ERROR_SQ)*(1/n_P61and107_TESTING1) 
MSE_P61and107_1_TESTING

# Calculate the mean absolute error for the test set
MAE_P61and107_1_TESTING <- sum(PILOT_61and107_TESTING$ERROR)*(1/n_P61and107_TESTING1)
MAE_P61and107_1_TESTING

# Calculate the root mean square error for the test set
RMSE_P61and107_1_TESTING <- (sum(PILOT_61and107_TESTING$ERROR_SQ)/n_P61and107_TESTING1)^(1/2)
RMSE_P61and107_1_TESTING

#-------------------------------------------------------------------------------
#PILOT PART 61 & 107 ACCURACY MEASURE 2 (2015-2020)
# Create the training set
PILOT_61and107_SUBSET_TRAINING_2 <- subset(MERGED_with_WEIGHTS, Year=="2015" | Year=="2016" | Year=="2017" | Year=="2018" | Year=="2019" | Year=="2020",
                                         select=c(Date_Char, Year, Zip.Code, X61and107pilot_weights))
# Create the average target weights by zip code
Mean_P61and107_Weights_By_Zip_TRAINING_2 <- PILOT_61and107_SUBSET_TRAINING_2 %>% group_by(Zip.Code) %>% 
    summarise_at(vars(X61and107pilot_weights), list(Average_P61and107_Weight=mean))
# Merge the average target weights into the training set by zip code
PILOT_61and107_TRAINING_2 <- merge(PILOT_61and107_SUBSET_TRAINING_2, Mean_P61and107_Weights_By_Zip_TRAINING_2, by="Zip.Code") 
# Calculate the error for the training set
PILOT_61and107_TRAINING_2$ERROR <- abs(PILOT_61and107_TRAINING_2$X61and107pilot_weights - PILOT_61and107_TRAINING_2$Average_P61and107_Weight)

# Create the test set
PILOT_61and107_SUBSET_TESTING_2 <- subset(MERGED_with_WEIGHTS, Year=="2021",
                                        select=c(Date_Char, Year, Zip.Code, X61and107pilot_weights))
# Merge the average target weights into the test set by zip code
PILOT_61and107_TESTING_2 <- merge(PILOT_61and107_SUBSET_TESTING_2, Mean_P61and107_Weights_By_Zip_TRAINING_2, by="Zip.Code") 
# Calculate the error for the test set
PILOT_61and107_TESTING_2$ERROR <- abs(PILOT_61and107_TESTING_2$X61and107pilot_weights - PILOT_61and107_TESTING_2$Average_P61and107_Weight)

#-------------------------------------------------------------------------------
#PILOT 61 AND 107 WEIGHTS R SQUARED - MEASURE 2
# Calculate the R-squared for the training set
R2_PP61and107_TRAINING2 <- (cor(PILOT_61and107_TRAINING_2$X61and107pilot_weights, PILOT_61and107_TRAINING_2$Average_P61and107_Weight))^2
R2_PP61and107_TRAINING2

# Calculate the R-squared for the test set
R2_PP61and107_TESTING2 <- (cor(PILOT_61and107_TESTING_2$X61and107pilot_weights, PILOT_61and107_TESTING_2$Average_P61and107_Weight))^2
R2_PP61and107_TESTING2

#-------------------------------------------------------------------------------
#PILOT 61 AND 107 WEIGHTS MSE/MAE/RMSE - MEASURE 1
# Calculate the total amount of records/rows for the training set
n_P61and107_TRAINING2 <- nrow(PILOT_61and107_TRAINING_2)
n_P61and107_TRAINING2

# Calculate the mean square error for the training set
PILOT_61and107_TRAINING_2$ERROR_SQ <- PILOT_61and107_TRAINING_2$ERROR^2
MSE_P61and107_2_TRAINING <- sum(PILOT_61and107_TRAINING_2$ERROR_SQ)*(1/n_P61and107_TRAINING2) 
MSE_P61and107_2_TRAINING

# Calculate the mean absolute error for the training set
MAE_P61and107_2_TRAINING <- sum(PILOT_61and107_TRAINING_2$ERROR)*(1/n_P61and107_TRAINING2)
MAE_P61and107_2_TRAINING

# Calculate the root mean square error for the training set
RMSE_P61and107_2_TRAINING <- (sum(PILOT_61and107_TRAINING_2$ERROR_SQ)/n_P61and107_TRAINING2)^(1/2)
RMSE_P61and107_2_TRAINING

#-------------------------------------------------------------------------------
# Calculate the total amount of records/rows for the test set
n_P61and107_TESTING2 <- nrow(PILOT_61and107_TESTING_2)
n_P61and107_TESTING2

# Calculate the mean square error for the test set
PILOT_61and107_TESTING_2$ERROR_SQ <- PILOT_61and107_TESTING_2$ERROR^2
MSE_P61and107_2_TESTING <- sum(PILOT_61and107_TESTING_2$ERROR_SQ)*(1/n_P61and107_TESTING2) 
MSE_P61and107_2_TESTING

# Calculate the mean absolute error for the test set
MAE_P61and107_2_TESTING <- sum(PILOT_61and107_TESTING_2$ERROR)*(1/n_P61and107_TESTING2)
MAE_P61and107_2_TESTING

# Calculate the root mean square error for the test set
RMSE_P61and107_2_TESTING <- (sum(PILOT_61and107_TESTING_2$ERROR_SQ)/n_P61and107_TESTING2)^(1/2)
RMSE_P61and107_2_TESTING

#-------------------------------------------------------------------------------
#AVERAGE PILOT 61 AND 107 WEIGHTS MSE/MAE/RMSE
# Average R-squared values are calculated from the Measure 1 and 2 test sets
R2_AVERAGE_P61and107_TEST <- (R2_PP61and107_TESTING1 + R2_PP61and107_TESTING2)/2
R2_AVERAGE_P61and107_TEST

# Average mean square error values are calculated from the Measure 1 and 2 test sets
MSE_AVERAGE_P61and107_TEST <- (MSE_P61and107_1_TESTING + MSE_P61and107_2_TESTING)/2
MSE_AVERAGE_P61and107_TEST

# Average mean absolute error values are calculated from the Measure 1 and 2 test sets
MAE_AVERAGE_P61and107_TEST <- (MAE_P61and107_1_TESTING + MAE_P61and107_2_TESTING)/2
MAE_AVERAGE_P61and107_TEST

# Average root mean square error values are calculated from the Measure 1 and 2 test sets
RMSE_AVERAGE_P61and107_TEST <- (RMSE_P61and107_1_TESTING + RMSE_P61and107_2_TESTING)/2
RMSE_AVERAGE_P61and107_TEST

# An array with the average accuracy statistics is created
P61and107Stats <- c(R2_AVERAGE_P61and107_TEST, MSE_AVERAGE_P61and107_TEST, MAE_AVERAGE_P61and107_TEST, RMSE_AVERAGE_P61and107_TEST)
P61and107Stats_Array <- array(P61and107Stats)
dimnames(P61and107Stats_Array)=list( c("Average R-Squared", "Average MSE", "Average MAE", "Average RMSE"))
P61and107Stats_Array

#----------------------------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------------------------
#UAS 107 ACCURACY MEASURE 1 (2015-2019)
# Create the training set
UAS_107_SUBSET_TRAINING <- subset(MERGED_with_WEIGHTS, Year=="2015" | Year=="2016" | Year=="2017" | Year=="2018" | Year=="2019",
                                    select=c(Date_Char, Year, Zip.Code, X107uas_weights))
# Create the average target weights by zip code
Mean_UAS107_Weights_By_Zip_TRAINING <- UAS_107_SUBSET_TRAINING %>% group_by(Zip.Code) %>% 
    summarise_at(vars(X107uas_weights), list(Average_UAS107_Weight=mean))
# Merge the average target weights into the training set by zip code
UAS_107_TRAINING <- merge(UAS_107_SUBSET_TRAINING, Mean_UAS107_Weights_By_Zip_TRAINING, by="Zip.Code") 
# Calculate the error for the training set
UAS_107_TRAINING$ERROR <- abs(UAS_107_TRAINING$X107uas_weights - UAS_107_TRAINING$Average_UAS107_Weight)

# Create the test set
UAS_107_SUBSET_TESTING <- subset(MERGED_with_WEIGHTS, Year=="2020",
                                   select=c(Date_Char, Year, Zip.Code, X107uas_weights))
# Merge the average target weights into the test set by zip code
UAS_107_TESTING <- merge(UAS_107_SUBSET_TESTING, Mean_UAS107_Weights_By_Zip_TRAINING, by="Zip.Code") 
# Calculate the error for the test set
UAS_107_TESTING$ERROR <- abs(UAS_107_TESTING$X107uas_weights - UAS_107_TESTING$Average_UAS107_Weight)

#-------------------------------------------------------------------------------
#UAS107 R SQUARED - MEASURE 1
# Calculate the R-squared for the training set
R2_UAS107_TRAINING1 <- (cor(UAS_107_TRAINING$X107uas_weights, UAS_107_TRAINING$Average_UAS107_Weight))^2
R2_UAS107_TRAINING1

# Calculate the R-squared for the test set
R2_UAS107_TESTING1 <- (cor(UAS_107_TESTING$X107uas_weights, UAS_107_TESTING$Average_UAS107_Weight))^2
R2_UAS107_TESTING1

#-------------------------------------------------------------------------------
#UAS107 MSE/MAE/RMSE - MEASURE 1
# Calculate the total amount of records/rows for the training set
n_UAS107_TRAINING1 <- nrow(UAS_107_TRAINING)
n_UAS107_TRAINING1

# Calculate the mean square error for the training set
UAS_107_TRAINING$ERROR_SQ <- UAS_107_TRAINING$ERROR^2
MSE_UAS107_1_TRAINING <- sum(UAS_107_TRAINING$ERROR_SQ)*(1/n_UAS107_TRAINING1) 
MSE_UAS107_1_TRAINING

# Calculate the mean absolute error for the training set
MAE_UAS107_1_TRAINING <- sum(UAS_107_TRAINING$ERROR)*(1/n_UAS107_TRAINING1)
MAE_UAS107_1_TRAINING

# Calculate the root mean square error for the training set
RMSE_UAS107_1_TRAINING <- (sum(UAS_107_TRAINING$ERROR_SQ)/n_UAS107_TRAINING1)^(1/2)
RMSE_UAS107_1_TRAINING

#-------------------------------------------------------------------------------
# Calculate the total amount of records/rows for the test set
n_UAS107_TESTING1 <- nrow(UAS_107_TESTING)
n_UAS107_TESTING1

# Calculate the mean square error for the test set
UAS_107_TESTING$ERROR_SQ <- UAS_107_TESTING$ERROR^2
MSE_UAS107_1_TESTING <- sum(UAS_107_TESTING$ERROR_SQ)*(1/n_UAS107_TESTING1) 
MSE_UAS107_1_TESTING

# Calculate the mean absolute error for the test set
MAE_UAS107_1_TESTING <- sum(UAS_107_TESTING$ERROR)*(1/n_UAS107_TESTING1)
MAE_UAS107_1_TESTING

# Calculate the root mean square error for the test set
RMSE_UAS107_1_TESTING <- (sum(UAS_107_TESTING$ERROR_SQ)/n_UAS107_TESTING1)^(1/2)
RMSE_UAS107_1_TESTING

#-------------------------------------------------------------------------------
#UAS 107 ACCURACY MEASURE 2 (2015-2020)
# Create the training set
UAS_107_SUBSET_TRAINING_2 <- subset(MERGED_with_WEIGHTS, Year=="2015" | Year=="2016" | Year=="2017" | Year=="2018" | Year=="2019" | Year=="2020",
                                      select=c(Date_Char, Year, Zip.Code, X107uas_weights))
# Create the average target weights by zip code
Mean_UAS107_Weights_By_Zip_TRAINING_2 <- UAS_107_SUBSET_TRAINING_2 %>% group_by(Zip.Code) %>% 
    summarise_at(vars(X107uas_weights), list(Average_UAS107_Weight=mean))
# Merge the average target weights into the training set by zip code
UAS_107_TRAINING_2 <- merge(UAS_107_SUBSET_TRAINING_2, Mean_UAS107_Weights_By_Zip_TRAINING_2, by="Zip.Code") 
# Calculate the error for the training set
UAS_107_TRAINING_2$ERROR <- abs(UAS_107_TRAINING_2$X107uas_weights - UAS_107_TRAINING_2$Average_UAS107_Weight)

# Create the test set
UAS_107_SUBSET_TESTING_2 <- subset(MERGED_with_WEIGHTS, Year=="2021",
                                     select=c(Date_Char, Year, Zip.Code, X107uas_weights))
# Merge the average target weights into the test set by zip code
UAS_107_TESTING_2 <- merge(UAS_107_SUBSET_TESTING_2, Mean_UAS107_Weights_By_Zip_TRAINING_2, by="Zip.Code") 
# Calculate the error for the test set
UAS_107_TESTING_2$ERROR <- abs(UAS_107_TESTING_2$X107uas_weights - UAS_107_TESTING_2$Average_UAS107_Weight)

#-------------------------------------------------------------------------------
#UAS107 R SQUARED - MEASURE 2
# Calculate the R-squared for the training set
R2_UAS107_TRAINING2 <- (cor(UAS_107_TRAINING_2$X107uas_weights, UAS_107_TRAINING_2$Average_UAS107_Weight))^2
R2_UAS107_TRAINING2

# Calculate the R-squared for the test set
R2_UAS107_TESTING2 <- (cor(UAS_107_TESTING_2$X107uas_weights, UAS_107_TESTING_2$Average_UAS107_Weight))^2
R2_UAS107_TESTING2

#-------------------------------------------------------------------------------
#UAS107 MSE/MAE/RMSE - MEASURE 2
# Calculate the total amount of records/rows for the training set
n_UAS107_TRAINING2 <- nrow(UAS_107_TRAINING_2)
n_UAS107_TRAINING2

# Calculate the mean square error for the training set
UAS_107_TRAINING_2$ERROR_SQ <- UAS_107_TRAINING_2$ERROR^2
MSE_UAS107_2_TRAINING <- sum(UAS_107_TRAINING_2$ERROR_SQ)*(1/n_UAS107_TRAINING2) 
MSE_UAS107_2_TRAINING

# Calculate the mean absolute error for the training set
MAE_UAS107_2_TRAINING <- sum(UAS_107_TRAINING_2$ERROR)*(1/n_UAS107_TRAINING2)
MAE_UAS107_2_TRAINING

# Calculate the root mean square error for the training set
RMSE_UAS107_2_TRAINING <- (sum(UAS_107_TRAINING_2$ERROR_SQ)/n_UAS107_TRAINING2)^(1/2)
RMSE_UAS107_2_TRAINING

#-------------------------------------------------------------------------------
# Calculate the total amount of records/rows for the test set
n_UAS107_TESTING2 <- nrow(UAS_107_TESTING_2)
n_UAS107_TESTING2

# Calculate the mean square error for the test set
UAS_107_TESTING_2$ERROR_SQ <- UAS_107_TESTING_2$ERROR^2
MSE_UAS107_2_TESTING <- sum(UAS_107_TESTING_2$ERROR_SQ)*(1/n_UAS107_TESTING2) 
MSE_UAS107_2_TESTING

# Calculate the mean absolute error for the test set
MAE_UAS107_2_TESTING <- sum(UAS_107_TESTING_2$ERROR)*(1/n_UAS107_TESTING2)
MAE_UAS107_2_TESTING

# Calculate the root mean square error for the test set
RMSE_UAS107_2_TESTING <- (sum(UAS_107_TESTING_2$ERROR_SQ)/n_UAS107_TESTING2)^(1/2)
RMSE_UAS107_2_TESTING

#-------------------------------------------------------------------------------
#AVERAGE UAS 107 WEIGHTS MSE/MAE/RMSE
# Average R-squared values are calculated from the Measure 1 and 2 test sets
R2_AVERAGE_UAS107_TEST <- (R2_UAS107_TESTING1 + R2_UAS107_TESTING2)/2
R2_AVERAGE_UAS107_TEST

# Average mean square error values are calculated from the Measure 1 and 2 test sets
MSE_AVERAGE_UAS107_TEST <- (MSE_UAS107_1_TESTING + MSE_UAS107_2_TESTING)/2
MSE_AVERAGE_UAS107_TEST

# Average mean absolute error values are calculated from the Measure 1 and 2 test sets
MAE_AVERAGE_UAS107_TEST <- (MAE_UAS107_1_TESTING + MAE_UAS107_2_TESTING)/2
MAE_AVERAGE_UAS107_TEST

# Average root mean square error values are calculated from the Measure 1 and 2 test sets
RMSE_AVERAGE_UAS107_TEST <- (RMSE_UAS107_1_TESTING + RMSE_UAS107_2_TESTING)/2
RMSE_AVERAGE_UAS107_TEST

# An array with the average accuracy statistics is created
UAS107Stats <- c(R2_AVERAGE_UAS107_TEST, MSE_AVERAGE_UAS107_TEST, MAE_AVERAGE_UAS107_TEST, RMSE_AVERAGE_UAS107_TEST)
UAS107Stats_Array <- array(UAS107Stats)
dimnames(UAS107Stats_Array)=list( c("Average R-Squared", "Average MSE", "Average MAE", "Average RMSE"))
UAS107Stats_Array

#----------------------------------------------------------------------------------------------------------------------
# References for R packages
citation("AppliedPredictiveModeling")
citation("caret")
citation("e1071")
citation("tidymodels")
citation("magrittr")
citation("dplyr")
citation("rsample")
citation("recipes")
citation("corrplot")
citation("readxl")
citation("tidyverse")
citation("vip")
citation("rpart")
citation("rpart.plot")
citation("ranger")
citation("tune")
citation("dials")
citation("parsnip")
citation("workflows")
citation("yardstick")
citation("pROC")
citation("readr")
citation("lubridate")

