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
               "ggridges",
               "stats",
               "PerformanceAnalytics",
               "caret",
               "keras",
               "Metrics",
               "sjPlot",
               "kerasR",
               "visdat",
               "ggthemes",
               "NeuralNetTools",
               "dplyr")

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
#print(vis_miss(cars, warn_large_data = FALSE))
#print(vis_dat(cars, warn_large_data = FALSE))

# Removing useless columns and 
# id, url, county, regionurl, imageurl, lat, long, description, vin, state, region, titlestatus
cars <- subset(cars, select = -c(id, url, county, regionurl, imageurl, lat, long, description, vin,state,region, titlestatus))
print(vis_miss(cars, warn_large_data = FALSE))
print(vis_dat(cars, warn_large_data = FALSE))
pricesGraph <- ggplot(cars,aes(x=price)) + geom_density(color="darkblue", fill="lightblue")
print(pricesGraph)

pricesGraph2 <- cars %>% 
   filter(price < 80000) %>%
   ggplot(aes(price)) + 
   geom_histogram() +
   scale_x_continuous(breaks = seq(0,80000,10000)) + 
   scale_y_continuous(labels = comma) +
   theme_hc() + 
   labs(x = "Price", y = "Count")

pricesGraph3 <- cars %>% 
   filter(price < 80000) %>%
   ggplot(aes(price)) + 
   stat_ecdf(geom="step") +
   scale_x_continuous(breaks = seq(0,80000,10000)) + 
   scale_y_continuous(breaks = c(0,.25,.5,.75,1), labels = c("0", "25", "50", "75", "100")) + 
   theme_hc() + 
   labs(x = "Price", y = "Percentage of Vehicles under Price")

manufacturersGraph <- cars %>% 
   group_by(manufacturer) %>%
   summarise(Count = n()) %>%
   ggplot(aes(reorder(manufacturer, Count), Count)) +
   geom_bar(stat = "identity") + 
   coord_flip() + 
   scale_y_continuous(labels = comma) + 
   theme_hc() + 
   labs(x = "Vehicle Manufacturer", y = "Count")

distributionOfPricePerYearGraph <- cars %>%
   filter(year>1950 & price < 50000) %>%
   ggplot(aes(x = price, y = year, group = year)) +
   geom_density_ridges(rel_min_height = 0.01) + 
   theme_hc() + 
   labs(x = "Price", y = "Year")


print(pricesGraph2)
print(pricesGraph3)
print(manufacturersGraph)
print(distributionOfPricePerYearGraph)

# ---------------------------------------------- Data clean-up ----------------------------------------------
# Remove rows without designated model/manufacturer
cars <- subset(cars, manufacturer != "" & model != "")
# Make sure price column is numeric
cars <- subset(cars, is.numeric(price))
# Remove rows with invalid or "bad" prices, typically <200 and less than 30000
cars <- subset(cars, price >= 200 & price <= 30000)


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
drops <- c(colsText)


carsToTrain<-topTenCarManufacturersDF[ , !(names(topTenCarManufacturersDF) %in% drops)]
print("Dimension for data ready to train")
print(dim(carsToTrain))

#------------------------------------Initial NN--------------------------------

# CREATE_NEW_MODEL <- TRUE
# 
# #carsToTrain <- subset(carsToTrain, `model encoded` == 1937 | `model encoded` == 3942 |`model encoded` == 15 |`model encoded` == 763 |`model encoded` == 479 |`model encoded` == 398)
# 
# smp_size <- floor(0.975 * nrow(carsToTrain))
# 
# train_ind <- sample(seq_len(nrow(carsToTrain)), size = smp_size)
# 
# 
# train <- carsToTrain[train_ind, ]
# test <- carsToTrain[-train_ind, ]
# 
# 
# #train <- subset(train,  `model encoded` <= 100)
# #test <- subset(test, `model encoded` <= 100)
# 
# 
# train_x <- subset(train, select = -c(price))
# train_y <- subset(train, select = c(price) )
# train_y_notNormalised <- train_y
# 
# 
# test_x <- subset(test, select = -c(price))
# test_y <- subset(test, select = c(price))
# test_y_notNormalised <- test_y
# 
# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# denormalize <- function(x,y) {
#   return (x * (max(y, na.rm=TRUE) - min(y, na.rm=TRUE)) + min(y, na.rm=TRUE))
# }
# 
# 
# train_y$price<-normalize(train_y$price)
# test_y$price<-normalize(test_y$price)
# 
# if(CREATE_NEW_MODEL){
#   # Initialize a sequential model
#   model <- keras_model_sequential() 
#   
#   # Add layers to the model
#   model %>% 
#     layer_dense(units = 64, input_shape = ncol(train_x)) %>%
#     layer_activation_leaky_relu() %>% 
#     layer_dense(units = 64) %>%
#     layer_activation_leaky_relu() %>% 
#     layer_dense(units = 1, activation = "linear")
#     #layer_activation_leaky_relu()
#   
#   
#   # Print a summary of a model
#   summary(model)
#   
#   # Get model configuration
#   get_config(model)
#   
#   # Get layer configuration
#   get_layer(model, index = 1)
#   
#   # List the model's layers
#   model$layers
#   
#   # List the input tensors
#   model$inputs
#   
#   # List the output tensors
#   model$outputs
#   
#   # Compile the model
#   model %>% compile(
#     loss = 'mse',
#     optimizer = optimizer_rmsprop(),
#     metrics = c("mse", "mae")
#   )
#   xmatrixre <- as.matrix(train_x)
#   ymatrixre <- data.matrix(train_y)
#   
#   print(dim(train_x))
#   print(dim(train_y))
#   print(ncol(train_x))
#   
#   #train_x <- sort(table(train_x$`model encoded`),decreasing=TRUE)[1:10]
#   
#   # Fit the model 
#   history <- model %>% fit(
#     x = as.matrix(train_x),
#     y = as.matrix(train_y),
#     epochs = 30,
#     validation_split = 0.3,
#     batch_size = 16,
#     verbose = 1,
#   )
#   #Saving the model so that it can be reused
#   model %>% save_model_tf("model")
#   print(history)
#   
# }else
#   {
#   #Reusing the saved model
#   model <- load_model_tf("model")
#   summary(model)
# }
# 
# denormalised <- denormalize(train_y,train_y_notNormalised)
# 
# test_predictions <- model %>% predict(as.matrix(test_x))
# test_predictions[ , 1]
# 
# denormalised <- denormalize(test_predictions,train_y_notNormalised)
# 
# # Evaluate on test data and labels and find values
# score = model %>% evaluate(as.matrix(test_x), as.matrix(test_y))
# mean_abs_error <- mae(test_y_notNormalised$price,denormalised)
# # Print the mean absolute error
# print(paste("The model is off by +- $" , mean_abs_error))
# 
# #Adding the predicted prices to the dataframe
# test_y_notNormalised$predictedPrice <- denormalised
# #Creating a graph with price vs predicted price
# predictedPricesGraph <- ggplot(data = test_y_notNormalised, aes(x = price, y = predictedPrice)) + geom_point()
# print(predictedPricesGraph)
# #Removing the minus values from the dataframe
# test_y_notNormalised <- subset(test_y_notNormalised, predictedPrice > 0)
# #Creating a graph with price vs predicted price
# predictedPricesGraph <- ggplot(data = test_y_notNormalised, aes(x = price, y = predictedPrice)) + geom_point() +
#   geom_abline(intercept = 0, slope = 1, color="red",linetype="dashed", size=1.5)
# print(predictedPricesGraph)


#Graph of Neural Network
#NNGraph <- plot_model(model, to_file = "model.png", show_shapes = TRUE,
#                      show_layer_names = TRUE)

#print(NNGraph)

#nn <- neuralnet(price ~ year + odometer + model + manufacturer, data=train, act.fct = "tanh")
#print(nn$result.matrix)
#plot(nn)

#Test the resulting output
#temp_test <- subset(test, select = c("year","odometer", "model", "manufacturer"))
#head(temp_test)
#nn.results <- compute(nn, temp_test)
#results <- data.frame(actual = test$price, prediction = nn.results$net.result)

# Plot of number of cars for the top 10 manufacturers
#manufacturerTop10Table <- table(topTenCarManufacturersDF$manufacturer)


#------------------------------------ SPRINT #2 --------------------------------

# carsToTrain <- subset(carsToTrain, select = -c(`fuel encoded`,`transmission encoded`,`type encoded`,`size encoded`,`cylinders encoded`,`drive encoded`))
# carsToTrain <- subset(carsToTrain, odometer <= 300000 & odometer >= 1000)
# 
# carsToTrain <- carsToTrain[sample(1:nrow(carsToTrain)),]
# testData <- carsToTrain[sample(1:1000),]
# test_x <- subset(testData, select = -c(price))
# test_y <- subset(testData, select = c(price) )
# test_y_notNormalised <- test_y
# test_y$price<-normalize(test_y$price)
# 
# CREATE_NEW_MODEL <- TRUE
# 
# k <- 5
# indices <- sample(1:nrow(carsToTrain))
# folds <- cut(1:length(indices), breaks = k, labels = FALSE)
# scoresList <- list()
# 
# for(i in 1:k){
#   print(paste("Fold number", i))
#   val_indices <- which(folds == i, arr.ind = TRUE)
#   val_x <- carsToTrain[val_indices,]
#   val_y <- carsToTrain[val_indices,]
#   
#   # Prepare the training data: data from all other partitions
#   train_x <- carsToTrain[-val_indices,]
#   train_y <- carsToTrain[-val_indices,]
#   
#   train_x <- subset(train_x, select = -c(price))
#   train_y <- subset(train_y, select = c(price) )
#   train_y_notNormalised <- train_y
#   
#   val_x <- subset(val_x, select = -c(price))
#   val_y <- subset(val_y, select = c(price))
#   val_y_notNormalised <- val_y
#   
#   normalize <- function(x) {
#     return ((x - min(x)) / (max(x) - min(x)))
#   }
#   
#   denormalize <- function(x,y) {
#     return (x * (max(y, na.rm=TRUE) - min(y, na.rm=TRUE)) + min(y, na.rm=TRUE))
#   }
#   
#   
#   train_y$price<-normalize(train_y$price)
#   val_y$price<-normalize(val_y$price)
#   
#   if(CREATE_NEW_MODEL){
#     # Initialize a sequential model
#     model <- keras_model_sequential() 
#     
#     # Add layers to the model
#     model %>% 
#       layer_dense(units = 64, input_shape = ncol(train_x)) %>%
#       layer_activation_leaky_relu() %>% 
#       layer_dense(units = 64) %>%
#       layer_activation_leaky_relu() %>% 
#       layer_dense(units = 1, activation = "linear")
#     #layer_activation_leaky_relu()
#     
#     
#     # Print a summary of a model
#     summary(model)
#     
#     # Get model configuration
#     get_config(model)
#     
#     # Get layer configuration
#     get_layer(model, index = 1)
#     
#     # List the model's layers
#     model$layers
#     
#     # List the input tensors
#     model$inputs
#     
#     # List the output tensors
#     model$outputs
#     
#     # Compile the model
#     model %>% compile(
#       loss = 'mse',
#       optimizer = optimizer_rmsprop(),
#       metrics = c("mse", "mae")
#     )
#     xmatrixre <- as.matrix(train_x)
#     ymatrixre <- data.matrix(train_y)
#     
#     print(dim(train_x))
#     print(dim(train_y))
#     print(ncol(train_x))
#     
#     #train_x <- sort(table(train_x$`model encoded`),decreasing=TRUE)[1:10]
#     
#     # Fit the model 
#     history <- model %>% fit(
#       x = as.matrix(train_x),
#       y = as.matrix(train_y),
#       epochs = 30,
#       validation_data = list(as.matrix(val_x),as.matrix(val_y)),
#       batch_size = 16,
#       verbose = 1,
#     )
#     #Saving the model so that it can be reused
#     model %>% save_model_tf("model")
#     print(history)
#     
#   }else
#   {
#     #Reusing the saved model
#     model <- load_model_tf("model")
#     summary(model)
#   }
#   
#   denormalised <- denormalize(train_y,train_y_notNormalised)
#   
#   test_predictions <- model %>% predict(as.matrix(test_x))
#   test_predictions[ , 1]
#   
#   denormalised <- denormalize(test_predictions,train_y_notNormalised)
#   
#   # Evaluate on test data and labels and find values
#   score = model %>% evaluate(as.matrix(test_x), as.matrix(test_y))
#   mean_abs_error <- mae(test_y_notNormalised$price,denormalised)
#   # Print the mean absolute error
#   print(paste("The model in the",i,"fold is off by +- $" , mean_abs_error))
#   #Saving the scores
#   scoresList <- c(scoresList, score, mean_abs_error)
#   #Adding the predicted prices to the dataframe
#   test_y_notNormalisedCopy <- test_y_notNormalised
#   test_y_notNormalisedCopy$predictedPrice <- denormalised
#   #Creating a graph with price vs predicted price
#   #predictedPricesGraph <- ggplot(data = test_y_notNormalisedCopy, aes(x = price, y = predictedPrice)) + geom_point()
#   #print(predictedPricesGraph)
#   #Removing the minus values from the dataframe
#   test_y_notNormalisedCopy <- subset(test_y_notNormalisedCopy, predictedPrice > 0)
#   #Creating a graph with price vs predicted price
#   predictedPricesGraph <- ggplot(data = test_y_notNormalisedCopy, aes(x = price, y = predictedPrice)) + geom_point() +
#     geom_abline(intercept = 0, slope = 1, color="red",linetype="dashed", size=1.5)
#   print(predictedPricesGraph)
# }

#------------------------------------ SPRINT #3 --------------------------------
CREATE_NEW_MODEL <- TRUE
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

denormalize <- function(x,y) {
  return (x * (max(y, na.rm=TRUE) - min(y, na.rm=TRUE)) + min(y, na.rm=TRUE))
}


carsToTrain <- subset(carsToTrain, select = -c(`fuel encoded`,`transmission encoded`,`type encoded`,`size encoded`,`cylinders encoded`,`drive encoded`, `paintcolor encoded`))
carsToTrain <- subset(carsToTrain, odometer <= 300000 & odometer >= 1000)
carsToTrain <- subset(carsToTrain, year >= 1990)
#carsToTrain$odometer <- round(carsToTrain$odometer, -4)
#carsToTrain$price <- round(carsToTrain$price, -2)

mostCommonModels <- tail(names(sort(table(carsToTrain$`model encoded`))), 1)
carsToTrain <- subset(carsToTrain, `model encoded` %in% mostCommonModels)

smp_size <- floor(0.9 * nrow(carsToTrain))
train_ind <- sample(seq_len(nrow(carsToTrain)), size = smp_size)

train <- carsToTrain[train_ind, ]
test <- carsToTrain[-train_ind, ]

train_y_notNormalised <- subset(train, select = c(price) )
train <- carsToTrain[train_ind, ]

# Prepare the training data
train_x <- subset(train, select = -c(price))
train_y <- subset(train, select = c(price) )

test_x <- subset(test, select = -c(price))
test_y <- subset(test, select = c(price) )

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

denormalize <- function(x,y) {
  return (x * (max(y, na.rm=TRUE) - min(y, na.rm=TRUE)) + min(y, na.rm=TRUE))
}


test_y_notNormalised <- test_y
test_y_normalised <- test_y

train_y$price<-normalize(train_y$price)
test_y$price<-normalize(test_y$price)

   if(CREATE_NEW_MODEL){
     # Initialize a sequential model
     model <- keras_model_sequential() 
     
     # Add layers to the model
     model %>% 
       layer_dense(units = 64, input_shape = ncol(train_x)) %>%
       layer_activation_leaky_relu() %>% 
       layer_dense(units = 64) %>%
       layer_activation_leaky_relu() %>% 
       layer_dense(units = 1, activation = "linear")
     #layer_activation_leaky_relu()
     
     
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
       optimizer = optimizer_rmsprop(),
       metrics = c("mse", "mae")
     )
     xmatrixre <- as.matrix(train_x)
     ymatrixre <- data.matrix(train_y)
     
     print(dim(train_x))
     print(dim(train_y))
     print(ncol(train_x))
     
     #train_x <- sort(table(train_x$`model encoded`),decreasing=TRUE)[1:10]
     
     # Fit the model 
        history <- model %>% fit(
          x = as.matrix(train_x),
          y = as.matrix(train_y),
          epochs = 100,
          validation_split = 0.2,
          batch_size = 16,
          callbacks = EarlyStopping(monitor='mae', mode = 'min', patience = 10 ,verbose = 1),
          verbose = 1,
        )
     #Saving the model so that it can be reused
     model %>% save_model_tf("model")
     print(history)
     
   }else
   {
     #Reusing the saved model
     model <- load_model_tf("model")
     summary(model)
   }
   
   denormalised <- denormalize(train_y,train_y_notNormalised)
   
   test_predictions <- model %>% predict(as.matrix(test_x))
   test_predictions[ , 1]
   
   denormalised <- denormalize(test_predictions,train_y_notNormalised)
   
   # Evaluate on test data and labels and find values
   score = model %>% evaluate(as.matrix(test_x), as.matrix(test_y))
   mean_abs_error <- mae(test_y_notNormalised$price,denormalised)
   # Print the mean absolute error
   print(paste("The model in the is off by +- $" , mean_abs_error))
   #Saving the scores
   #Adding the predicted prices to the dataframe
   test_y_notNormalisedCopy <- test_y_notNormalised
   test_y_notNormalisedCopy$predictedPrice <- denormalised
   #Creating a graph with price vs predicted price
   #predictedPricesGraph <- ggplot(data = test_y_notNormalisedCopy, aes(x = price, y = predictedPrice)) + geom_point()
   #print(predictedPricesGraph)
   #Removing the minus values from the dataframe
   test_y_notNormalisedCopy <- subset(test_y_notNormalisedCopy, predictedPrice > 0)
   #Creating a graph with price vs predicted price
   predictedPricesGraph <- ggplot(data = test_y_notNormalisedCopy, aes(x = price, y = predictedPrice)) + geom_point() +
     geom_abline(intercept = 0, slope = 1, color="red",linetype="dashed", size=1.5)+
     geom_abline(intercept = 2000, slope = 1, color="green",linetype="dashed", size=1.5)+
     geom_abline(intercept = -2000, slope = 1, color="green",linetype="dashed", size=1.5)
   print(predictedPricesGraph)
   
   difference <- test_y_notNormalisedCopy$price - test_y_notNormalisedCopy$predictedPrice
   test_y_notNormalisedCopy$difference <- difference
   
   
   print(sum( (-2000 < test_y_notNormalisedCopy$difference) & (test_y_notNormalisedCopy$difference < 2000)))
   print(dim(test_y_notNormalisedCopy))
   
#}

