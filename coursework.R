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
               "data.table",
               "CatEncoders",
               "keras",
               "neuralnet",
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
cars <- subset(cars, price <= 50000)
cars <- subset(cars, year >= 1960)

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
topTenCarManufacturersDF <- na.omit(topTenCarManufacturersDF)

# Normalise the data
topTenCarManufacturersDF[] <- lapply(topTenCarManufacturersDF, function(x) if(is.numeric(x)){
  x = (x - min(x))/(max(x) - min(x))
} else x)

# transform feature char_1 to int
fit=LabelEncoder.fit(topTenCarManufacturersDF$manufacturer)
topTenCarManufacturersDF$manufacturer=transform(fit,topTenCarManufacturersDF$manufacturer)

fit=LabelEncoder.fit(topTenCarManufacturersDF$model)
topTenCarManufacturersDF$model=transform(fit,topTenCarManufacturersDF$model)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(topTenCarManufacturersDF))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(topTenCarManufacturersDF)), size = smp_size)

train <- topTenCarManufacturersDF[train_ind, ]
train_x <- subset(train, select = c(year, manufacturer, odometer))
train_y <- subset(train, select = c(price))

test <- topTenCarManufacturersDF[-train_ind, ]
test_x <- subset(test, select = c(year, manufacturer, odometer))
test_y <- subset(test, select = c(price))

# Initialize a sequential model
model <- keras_model_sequential() 

# Add layers to the model
model %>% 
  layer_dense(units = 4, activation = 'relu', input_shape = c(3)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = 'linear')

# Print a summary of a model
summary(model)

# Get model configuration
get_config(model)

# Get layer configuration
get_layer(model, index = 1)

# List the model's layers
model$layers

# List the input tensors
model$inputs

# List the output tensors
model$outputs

# Compile the model
model %>% compile(
  loss = 'mse',
  optimizer = 'adam',
  metrics = c("mse", "accuracy", "mae")
)
xmatrixre <- as.matrix(train_x)
ymatrixre <- data.matrix(train_y)

# Fit the model 
model %>% fit(
  as.matrix(train_x), as.matrix(train_y),
  epochs = 50, 
  batch_size = 64,
  validation_data = list(as.matrix(test_x),as.matrix(test_y))
)

#nn <- neuralnet(price ~ year + odometer + model + manufacturer, data=train, act.fct = "tanh")
#print(nn$result.matrix)
#plot(nn)

#Test the resulting output
#temp_test <- subset(test, select = c("year","odometer", "model", "manufacturer"))
#head(temp_test)
#nn.results <- compute(nn, temp_test)
#results <- data.frame(actual = test$price, prediction = nn.results$net.result)

# Plot of number of cars for the top 10 manufacturers
manufacturerTop10Table <- table(topTenCarManufacturersDF$manufacturer)
barplot(manufacturerTop10Table, main = "Top 10 Car Manufacturers Distribution",
        xlab = "Manufacturers", ylab = "No. of Cars")

# Scatter plot of car mileage relative to price -- this looks way too bad. perhaps we still have too much unreliable data?
with(cars,plot(odometer,price))
