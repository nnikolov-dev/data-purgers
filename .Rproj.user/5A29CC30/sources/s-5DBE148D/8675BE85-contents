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

#Load additional R script files provide for this lab
source("dataPrep.R")
set.seed(123)

# Loading the data
cars <- NreadDataset(DATASET_FILENAME)
# cars <- NPREPROCESSING_prettyDataset(cars)

# Removing the " cylinders" part of the column
# and converting it to numeric
cars$cylinders <- as.numeric(substr(cars$cylinders,1,nchar(cars$cylinders)-10))

# Removing useless columns
# id, url, county, regionurl, imageurl
cars <- subset(cars, select = -c(id,url,county,regionurl,imageurl,lat,long))

# Print Field Types
field_types = NPREPROCESSING_initialFieldType(cars)
print(field_types)
field_types = NPREPROCESSING_initialFieldType(cars)
print(field_types)

print(length(field_types))

# Question 2
numeric_fields = c()
symbolic_fields = c()

for(i in 1:length(field_types)) {
  if(field_types[i] == TYPE_NUMERIC) {
    numeric_fields = append(numeric_fields, names(cars)[i])
  } else if(field_types[i] == TYPE_SYMBOLIC) {
    symbolic_fields = append(symbolic_fields, names(cars)[i])
  }
}


print(paste("NUMERIC FIELDS=", length(numeric_fields)))
print(numeric_fields)
print(paste("SYMBOLIC FIELDS=", length(symbolic_fields)))
print(symbolic_fields)

# Question 3
field_types1 = NPREPROCESSING_discreetNumeric(cars, field_types, DISCREET_BINS)
print(field_types1)
results = data.frame(field=names(cars),initial=field_types,types1=field_types1)
print(formattable::formattable(results))

# Question 4
ordinals = cars[,which(field_types1==TYPE_ORDINAL)]
ordinals = NPREPROCESSING_outlier(ordinals, OUTLIER_CONF)

# Question 6
zscaled = as.data.frame(scale(ordinals, TRUE, TRUE))






