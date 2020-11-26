# ************************************************
# PRACTICAL BUSINESS ANALYTICS
# COM3018/COMM053
# Coursework
#
# Data Purgers
# Theodoros, Louai, Valeriy,
# Markos, Yiannis, Nikita
#
# 21 OCTOBER 2019
#
# UPDATE
# 1.00      21/10/2019    Initial Version
# ************************************************

# Clearing all global environment variables
rm(list=ls())

# ************************************************
# Global Environment variables
# ************************************************

DATASET_FILENAME  <- "vehicles.csv"  # Dataset file name
TYPE_DISCREET     <- "DISCREET"      # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"       # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"      # field is a string
TYPE_NUMERIC      <- "NUMERIC"       # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"        # field is not encoded

# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "ggplot2",
               "stats",
               "PerformanceAnalytics",
               "caret",
               "keras",
               "Metrics",
               "sjPlot",
               "dplyr",
               "xgboost",
               "SHAPforxgboost")

# User defined functions are next
# ************************************************


# ************************************************
# This is where R starts execution

# clears the console area
# cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)
# Set scientific notation to a sensible number of digits
options(scipen = 8)
# Load additional R script files provide for this lab
source("dataPrep.R")
set.seed(123)

# Loading the data
carsInitial <- NreadDataset(DATASET_FILENAME)
cars <- carsInitial

# Removing useless columns and 
# id, url, county, regionurl, imageurl, lat, long, description, vin,state,region, titlestatus, fuel, transmission, type, size, drive, paintcolor
cars <- subset(cars, select = -c(id, url, county, regionurl, imageurl, lat, long, description, vin,state,region, titlestatus, fuel, transmission, type, size, drive, paintcolor))

# ---------------------------------------------- Data clean-up ----------------------------------------------
# Remove rows without designated model/manufacturer
cars <- subset(cars, manufacturer != "" & model != "")
# Make sure price column is numeric
cars <- subset(cars, is.numeric(price))
# Remove rows with invalid or "bad" prices, typically < 200
cars <- subset(cars, price >= 200 & price < 45000 & odometer <= 300000 & year >= 1990 & year <=2020)
# Method to remove empty values
removeEmptyVals <- function(dt) {
  colsCar<-colnames(dt)
  for (col in colsCar){
    dt <- dt[!(is.na(dt[,col]) | dt[,col]==""), ]
  }
  return(dt)
}
cars<-removeEmptyVals(cars)

print(dim(cars))
# ---------------------------------------------- Initial plotting ----------------------------------------------

# Plot of number of data before and after clean-up
totalCarsPreCleanup <- nrow(carsInitial)
totalCarsPostCleanup <- nrow(cars)

# Condition of cars
conditionTable <- table(cars$condition)


# Get the top 10 manufacturers
mostCommonManufacturers <- tail(names(sort(table(cars$manufacturer))), 10)

# New data frame constructed with only the data of the top 10 manufacturers included
topTenCarManufacturersDF <- subset(cars, manufacturer %in% mostCommonManufacturers)

# Get the top 50 models
mostCommonModels <- tail(names(sort(table(topTenCarManufacturersDF$model))), 30)
topTenCarManufacturersDF <- subset(topTenCarManufacturersDF, model %in% mostCommonModels)

library(data.table)
library(CatEncoders)
#Get Categorical values
cat_cols <- dplyr::select_if(topTenCarManufacturersDF, is.character)
colsText<-colnames(cat_cols)
#Label Encoder
for (col in colsText){
  fit<-LabelEncoder.fit(topTenCarManufacturersDF[,col])
  toAdd<-paste(col, "encoded")
  topTenCarManufacturersDF[,toAdd]<-transform(fit,topTenCarManufacturersDF[,col])
}
# Clean data - Remove Empty values
topTenCarManufacturersDF <- removeEmptyVals(topTenCarManufacturersDF)
print("Print Dimension of top 10 car manufactures:")
print(dim(topTenCarManufacturersDF))
# Method for Data Visualisation - Needed for diagnosis
#displayDistribution <- function(colname) {
# d <- density(topTenCarManufacturersDF[,colname])
# plot(d, main=paste("Kernel Density of",colname))
# polygon(d, col="red", border="blue")
#}
#x <- list("price", "year", "cylinders", "odometer", "fuel")
#for (item in x){
#  displayDistribution(colname = item)
#}

# Dataframe Ready to train!
carsToTrain <- topTenCarManufacturersDF
shuffledCars <- carsToTrain[sample(nrow(carsToTrain)),]
carsToTrain <- shuffledCars
drops <- c(colsText)


carsToTrain<-topTenCarManufacturersDF[ , !(names(topTenCarManufacturersDF) %in% drops)]
print("Dimension for data ready to train")
print(dim(carsToTrain))


CREATE_NEW_MODEL <- FALSE

k <- 4
indices <- sample(1:nrow(carsToTrain))
folds <- cut(1:length(indices), breaks = k, labels = FALSE)
for(i in 1:k){
  test_indices <- which(folds == i, arr.ind = TRUE)
  test_x <- carsToTrain[test_indices,]
  test_y <- carsToTrain[test_indices,]
  
  # Prepare the training data: data from all other partitions
  train_x <- carsToTrain[-test_indices,]
  train_y <- carsToTrain[-test_indices,]
  
  train_x <- subset(train_x, select = -c(price))
  train_y <- subset(train_y, select = c(price) )
  
  
denormalize <- function(x,y) {
  test_x <- subset(test_x, select = -c(price))
  test_y <- subset(test_y, select = c(price))
}

train_y_notNormalised <- train_y

test_y_notNormalised <- test_y

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

  return (x * (max(y, na.rm=TRUE) - min(y, na.rm=TRUE)) + min(y, na.rm=TRUE))
}

train_x <- normalize(train_x)
train_y <- normalize(train_y)
test_x <- normalize(test_x)
test_y <- normalize(test_y)

train_y$price<-normalize(train_y$price)

dtrain <- xgb.DMatrix(data = as.matrix(train_x), label=as.matrix(train_y))

model <- xgboost(data = dtrain, max.depth = 8, eta = 0.1, nrounds = 6000, objective = "reg:squarederror", verbose = 0)
  
denormalised <- denormalize(train_y,train_y_notNormalised)
  
test_predictions <- model %>% predict(as.matrix(test_x))
  
denormalised <- denormalize(test_predictions,train_y_notNormalised)
 

# Evaluate on test data and labels and find values
mean_abs_error <- mae(test_y_notNormalised$price,denormalised)
sqrt_mean_abs_error <- rmse(test_y_notNormalised$price,denormalised)
# Print the mean absolute error
print(paste("MAE +- $" , mean_abs_error))
print(paste("RMSE +- $" , sqrt_mean_abs_error))


#Adding the predicted prices to the dataframe
test_y_notNormalised$predictedPrice <- denormalised
test_y_notNormalised <- subset(test_y_notNormalised, predictedPrice < 100000)
#Creating a graph with price vs predicted price
#predictedPricesGraph <- ggplot(data = test_y_notNormalised, aes(x = price, y = predictedPrice)) + geom_point()
#print(predictedPricesGraph)
#Removing the minus values from the dataframe
test_y_notNormalised <- subset(test_y_notNormalised, predictedPrice > 0)
#Creating a graph with price vs predicted price
predictedPricesGraph <- ggplot(data = test_y_notNormalised, aes(x = price, y = predictedPrice)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red",linetype="dashed", size=1.5) +
  geom_abline(intercept = -mean_abs_error, slope = 1, color="green",linetype="dashed", size=1.5)+
  geom_abline(intercept = mean_abs_error, slope = 1, color="green",linetype="dashed", size=1.5)

print(predictedPricesGraph)

shap_values <- shap.values(xgb_model = model, X_train = test_x)
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = test_x)


# To prepare the long-format data:
# is the same as: using given shap_contrib
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = as.matrix(test_x))
# (Notice that there will be a data.table warning from `melt.data.table` due to `dayint` coerced from integer to double)

# **SHAP summary plot**
shap.plot.summary(shap_long)

difference <- test_y_notNormalised$price - test_y_notNormalised$predictedPrice
test_y_notNormalised$difference <- difference


print(sum( (-mean_abs_error < test_y_notNormalised$difference) & (test_y_notNormalised$difference < mean_abs_error)))
print(dim(test_y_notNormalised))


}