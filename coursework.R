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
DISCREET_BINS     <- 8               # Number of empty bins to determine discreet
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.99                 # Confidence p-value for outlier detection
HOLDOUT           <- 70                   # % split to create TRAIN dataset
CUTOFF_OUTLIER    <- 0.99                 # Confidence p-value for outlier detection
CUTOFF_DISCREET   <- 5                    # Number of empty bins to determine discreet
CUTOFF_REDUNDANT  <- 0.95                 # Linear correlation coefficient cut-off
MAX_LITERALS      <- 400                    # Maximum numner of 1-hot-encoding fields
OUTPUT_FIELD      <- "price"

# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4
# ggplot2                3.3.2
# caret                  6.0-86

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "ggplot2",
               "stats",
               "PerformanceAnalytics",
               "caret",
               "data.table",
               "CatEncoders")

# User defined functions are next

# ************************************************
# fieldTypes() :
#
# Printing the type of each field
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   None
#
# ************************************************
fieldTypes<-function(datataset){
  field_types_initial<-NPREPROCESSING_initialFieldType(datataset)
  numeric_fields<-c()
  symbolic_fields<-c()
  for(i in 1:length(field_types_initial)) {
    if(field_types_initial[i]==TYPE_NUMERIC) {
      numeric_fields<-append(numeric_fields,names(datataset)[i])
    } else if(field_types_initial[i]==TYPE_SYMBOLIC) {
      symbolic_fields = append(symbolic_fields,names(datataset)[i])
    }
  }
  field_types<-NPREPROCESSING_discreetNumeric(datataset,field_types_initial,DISCREET_BINS)
  results<-data.frame(field=names(datataset),initial=field_types_initial,final=field_types)
  print(formattable::formattable(results))
} #endof fieldTypes()

# ************************************************
# labelEncode() :
#
# Encodes each label in the dataset using the LabelEncoder.fit function from the
# CatEncoders package
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with encoded labels
#
# ************************************************
labelEncode<-function(dataset){
  cat_cols<-dplyr::select_if(dataset, is.character)
  colsText<-colnames(cat_cols)
  #Label Encoder
  for(col in colsText){
    fit<-LabelEncoder.fit(dataset[,col])
    toAdd<-paste(col,"encoded",sep="")
    dataset[,toAdd]<-transform(fit,dataset[,col])
  }
  return(dataset)
} #endof labelEncode()

# This is where R starts execution

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

# Set scientific notation to a sensible number of digits
options(scipen=8)

# Load additional R script files provide for this lab
source("dataPrep.R")
source("functions.R")
set.seed(123)

# Loading the data
carsInitial<-NreadDataset(DATASET_FILENAME)
cars<-carsInitial

# Removing useless columns and 
# id, url, county, regionurl, imageurl, lat, long, description, vin, state, region, titlestatus
cars<-subset(cars,select=-c(id,url,county,regionurl,imageurl,lat,long,description,vin,state,region,titlestatus))

# ---------------------------------------------- Data clean-up ----------------------------------------------
# Remove rows without designated model/manufacturer
cars<-subset(cars,manufacturer != "" & model != "")
# Make sure price column is numeric
cars<-subset(cars,is.numeric(price))
# Remove rows with invalid or "bad" prices, typically <200
cars<-subset(cars,price>=200)
# Method to remove empty values
removeEmptyVals<-function(dt) {
  colsCar<-colnames(dt)
  for (col in colsCar){
    dt<-dt[!(is.na(dt[,col]) | dt[,col]==""),]
  }
  return(dt)
}

cars<-removeEmptyVals(cars)
print(dim(cars))

# ---------------------------------------------- Field Types ----------------------------------------------
fieldTypes(cars)
# ---------------------------------------------- Initial plotting ----------------------------------------------

# Plot of number of data before and after clean-up
totalCarsPreCleanup<-nrow(carsInitial)
totalCarsPostCleanup<-nrow(cars)
barplot(c(totalCarsPreCleanup, totalCarsPostCleanup),main="No. of Rows Before/After Cleanup")

# Condition of cars
conditionTable<-table(cars$condition)
barplot(conditionTable,
        main="Condition of cars",
        xlab="Condition",
        ylab="No. of Cars")

# Get the top 10 manufacturers
mostCommonManufacturers<-tail(names(sort(table(cars$manufacturer))),10)

# New data frame constructed with only the data of the top 10 manufacturers included
topTenCarManufacturersDF<-subset(cars, manufacturer %in% mostCommonManufacturers)

# Plot of number of cars for the top 10 manufacturers
manufacturerTop10Table<-table(topTenCarManufacturersDF$manufacturer)
barplot(manufacturerTop10Table,
        main="Top 10 Car Manufacturers Distribution",
        xlab="Manufacturers",
        ylab="No. of Cars")

# Scatter plot of car mileage relative to price -- this looks way too bad. perhaps we still have too much unreliable data?
with(cars,plot(odometer,price))

# Encode Labels
topTenCarManufacturersDF<-labelEncode(topTenCarManufacturersDF)

# Clean data - Remove Empty values
topTenCarManufacturersDF<-removeEmptyVals(topTenCarManufacturersDF)
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
cat_cols<-dplyr::select_if(topTenCarManufacturersDF, is.character)
colsText<-colnames(cat_cols)
carsToTrain<-topTenCarManufacturersDF
drops<-c(colsText)

carsToTrain<-topTenCarManufacturersDF[,!(names(topTenCarManufacturersDF) %in% drops)]
print("Dimension for data ready to train")
print(dim(carsToTrain))
