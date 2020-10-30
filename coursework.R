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
               "caret")

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
# cars <- NPREPROCESSING_prettyDataset(cars)

# Removing the " cylinders" part of the column
# and converting it to numeric
cars$cylinders <- as.numeric(substr(cars$cylinders,1,nchar(cars$cylinders)-10))

# Removing useless columns and 
# id, url, county, regionurl, imageurl, lat, long, description, vin
cars <- subset(cars, select = -c(id, url, county, regionurl, imageurl, lat, long, description, vin))

# ---------------------------------------------- Data clean-up ----------------------------------------------
# Remove rows without designated model/manufacturer
cars <- subset(cars, manufacturer != "" & model != "")
# Make sure price column is numeric
cars <- subset(cars, is.numeric(price))
# Remove rows with invalid or "bad" prices, typically <200
cars <- subset(cars, price >= 200)

# ---------------------------------------------- Initial plotting ----------------------------------------------

# Plot of number of data before and after clean-up
totalCarsPreCleanup <- nrow(carsInitial)
totalCarsPostCleanup <- nrow(cars)
barplot(c(totalCarsPreCleanup, totalCarsPostCleanup), main = "No. of Rows Before/After Cleanup")

# Condition of cars
conditionTable <- table(cars$condition)
barplot(conditionTable, main = "Condition of cars",
        xlab = "Condition", ylab = "No. of Cars")

# Get the top 10 manufacturers
mostCommonManufacturers <- tail(names(sort(table(cars$manufacturer))), 10)

# New data frame constructed with only the data of the top 10 manufacturers included
topTenCarManufacturersDF <- subset(cars, manufacturer %in% mostCommonManufacturers)

# Plot of number of cars for the top 10 manufacturers
manufacturerTop10Table <- table(topTenCarManufacturersDF$manufacturer)
barplot(manufacturerTop10Table, main = "Top 10 Car Manufacturers Distribution",
        xlab = "Manufacturers", ylab = "No. of Cars")

# Scatter plot of car mileage relative to price -- this looks way too bad. perhaps we still have too much unreliable data?
with(cars,plot(odometer,price))



library(data.table)
library(CatEncoders)
num_cols <- dplyr::select_if(topTenCarManufacturersDF, is.character)
s<-colnames(num_cols)
for (i in s){
  fit=LabelEncoder.fit(topTenCarManufacturersDF[,i])
  topTenCarManufacturersDF[,i]=transform(fit,topTenCarManufacturersDF[,i])
  }