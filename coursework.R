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
K_FOLDS           <- 5
TRAIN_MODEL       <- TRUE


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
               "CatEncoders",
               "dplyr",
               "xgboost",
               "SHAPforxgboost",
               "Metrics",
               "NeuralNetTools",
               "keras",
               "kerasR",
               "visdat",
               "ggthemes",
               "sjPlot")

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

# ************************************************
# nromalize() :
#
# Normalizes a dataframe
#
# INPUT   :   data frame         - x              - dataset
#
# OUTPUT  :   data frame         - dataset        - normalized dataset
#
# ************************************************
normalize<-function(x){
  return ((x - min(x)) / (max(x) - min(x)))
} #endof normalize()

# ************************************************
# denormalize() :
#
# Converts all data from normalized to original values a dataframe
#
# INPUT   :   data frame         - x              - dataset
#
# OUTPUT  :   data frame         - dataset        - normalized dataset
#
# ************************************************
denormalize<-function(x,y){
  return (x * (max(y, na.rm=TRUE) - min(y, na.rm=TRUE)) + min(y, na.rm=TRUE))
} #endof denormalize()

# ************************************************
# evalNN() :
#
# Evaluates the performance of a NN model
#
# INPUT:
#        model      - model      - model to be evaluated
#        data frame - data frame - train labels
#        data frame - data frame - test data
#        data frame - data frame - test labels
#        data frame - data frame - train labels normalized
#        data frame - data frame - train labels not normalized
#
# ************************************************
evalNN<-function(model,train_y,test_x,test_y,train_y_notNormalised,test_y_notNormalised){
  denormalised<-denormalize(train_y,train_y_notNormalised)
  
  test_predictions<-model%>%predict(as.matrix(test_x))
  test_predictions[,1]
  denormalised<-denormalize(test_predictions,train_y_notNormalised)
  
  # Evaluate on test data and labels and find values
  score<-model%>%evaluate(as.matrix(test_x), as.matrix(test_y))
  mean_abs_error_3<-mae(test_y_notNormalised$price,denormalised)
  mean_square_error_3<-mse(test_y_notNormalised$price,denormalised)
  
  # Print the mean absolute error
  print(paste("The model in the is off by +- $",mean_abs_error_3))
  #Saving the scores
  #Adding the predicted prices to the dataframe
  test_y_notNormalisedCopy<-test_y_notNormalised
  test_y_notNormalisedCopy$predictedPrice<-denormalised
  #Creating a graph with price vs predicted price
  #predictedPricesGraph <- ggplot(data = test_y_notNormalisedCopy, aes(x = price, y = predictedPrice)) + geom_point()
  #print(predictedPricesGraph)
  #Removing the minus values from the dataframe
  test_y_notNormalisedCopy<-subset(test_y_notNormalisedCopy,predictedPrice > 0)
  #Creating a graph with price vs predicted price
  predictedPricesGraph <- ggplot(data=test_y_notNormalisedCopy,aes(x=price,y =predictedPrice)) + geom_point() +
    geom_abline(intercept=0, slope= 1, color="red",linetype="dashed", size=1.5)+
    geom_abline(intercept=2000, slope=1, color="green",linetype="dashed", size=1.5)+
    geom_abline(intercept=-2000, slope=1, color="green",linetype="dashed", size=1.5)
  print(predictedPricesGraph)
  
  difference<-test_y_notNormalisedCopy$price - test_y_notNormalisedCopy$predictedPrice
  test_y_notNormalisedCopy$difference<-difference
  
  print(sum((-2000 < test_y_notNormalisedCopy$difference) & (test_y_notNormalisedCopy$difference < 2000)))
  print(dim(test_y_notNormalisedCopy))
} #endof evalNN()

# ************************************************
# trainGradientBoost() :
#
# Trains a model using the Gradient Boost algorithm
#
# INPUT:
#       data frame - train_x               - train data
#       data frame - train_y               - train labels
#       data frame - test_x                - test data
#       data frame - test_y                - test labels
#       data frame - train_y_notNormalised - train labels (not normalized)
#       data frame - test_y_notNormalised  - test labels (not normalized)
#
# ************************************************
trainGradientBoost<-function(train_x,train_y,test_x,test_y,train_y_notNormalised,test_y_notNormalised){
  dtrain<-xgb.DMatrix(data=as.matrix(train_x),label=as.matrix(train_y))
  model<-xgboost(data=dtrain,max.depth=8,eta=0.1,nrounds=6000,objective="reg:squarederror",verbose=0)
  xgb.save(model, 'models/xgb.model')
  denormalised<-denormalize(train_y,train_y_notNormalised)
  test_predictions<-model %>% predict(as.matrix(test_x))
  denormalised<-denormalize(test_predictions,train_y_notNormalised)
  
  # Evaluate on test data and labels and find values
  mean_abs_error<-mae(test_y_notNormalised$price,denormalised)
  sqrt_mean_abs_error<-rmse(test_y_notNormalised$price,denormalised)
  
  # Print the mean absolute error
  print(paste("MAE +- $" , mean_abs_error))
  print(paste("RMSE +- $" , sqrt_mean_abs_error))
  
  #Adding the predicted prices to the dataframe
  test_y_notNormalised$predictedPrice<-denormalised
  test_y_notNormalised<-subset(test_y_notNormalised,predictedPrice < 100000)
  
  #Creating a graph with price vs predicted price
  #predictedPricesGraph <- ggplot(data = test_y_notNormalised, aes(x = price, y = predictedPrice)) + geom_point()
  #print(predictedPricesGraph)
  #Removing the minus values from the dataframe
  test_y_notNormalised<-subset(test_y_notNormalised,predictedPrice > 0)
  
  #Creating a graph with price vs predicted price
  predictedPricesGraph<-ggplot(data=test_y_notNormalised,aes(x=price,y=predictedPrice)) + geom_point() +
    geom_abline(intercept=0,slope=1,color="red",linetype="dashed", size=1.5) +
    geom_abline(intercept=-mean_abs_error,slope=1,color="green",linetype="dashed",size=1.5) +
    geom_abline(intercept=mean_abs_error,slope=1,color="green",linetype="dashed",size=1.5)
  plot(predictedPricesGraph)
  
  shap_values<-shap.values(xgb_model=model,X_train=test_x)
  shap_long<-shap.prep(shap_contrib=shap_values$shap_score,X_train=test_x)
  
  # To prepare the long-format data:
  # is the same as: using given shap_contrib
  shap_long<-shap.prep(shap_contrib=shap_values$shap_score,X_train=as.matrix(test_x))
  # (Notice that there will be a data.table warning from `melt.data.table` due to `dayint` coerced from integer to double)
  
  # **SHAP summary plot**
  shap.plot.summary(shap_long)
  
  difference<-test_y_notNormalised$price - test_y_notNormalised$predictedPrice
  test_y_notNormalised$difference<-difference
  
  print(sum((-mean_abs_error < test_y_notNormalised$difference) & (test_y_notNormalised$difference < mean_abs_error)))
  print(dim(test_y_notNormalised))
} #endof trainGradientBoost()

# ************************************************
# trainNN() :
#
# Trains a model using the Neural Network
#
# INPUT:
#       data frame - train_x               - train data
#       data frame - train_y               - train labels
#       data frame - test_x                - test data
#       data frame - test_y                - test labels
#       data frame - train_y_notNormalised - train labels (not normalized)
#       data frame - test_y_notNormalised  - test labels (not normalized)
#
# ************************************************
trainNN<-function(train_x,train_y,test_x,test_y,train_y_notNormalised,test_y_notNormalised){
  
  # Initialize a sequential model
  model <- keras_model_sequential() 
  
  # Add layers to the model
  model %>% 
    layer_dense(units=64,input_shape=ncol(train_x)) %>%
    layer_activation_leaky_relu() %>% 
    layer_dense(units=64) %>%
    layer_dropout(0.2) %>%
    layer_activation_leaky_relu() %>%
    layer_dropout(0.2) %>%
    layer_dense(units=1,activation="linear")
  #layer_activation_leaky_relu()
  
  # Print a summary of a model
  summary(model)
  
  # Get model configuration
  get_config(model)
  
  # Get layer configuration
  get_layer(model,index=1)
  
  # List the model's layers
  model$layers
  
  # List the input tensors
  model$inputs
  
  # List the output tensors
  model$outputs
  
  # Compile the model
  model %>% compile(
    loss='mse',
    optimizer=optimizer_rmsprop(),
    metrics=c("mse", "mae")
  )
  
  xmatrixre<-as.matrix(train_x)
  ymatrixre<-data.matrix(train_y)
  
  print(dim(train_x))
  print(dim(train_y))
  print(ncol(train_x))
  
  #train_x <- sort(table(train_x$`model encoded`),decreasing=TRUE)[1:10]
  
  # Fit the model 
  history<-model %>% fit(
    x=as.matrix(train_x),
    y=as.matrix(train_y),
    epochs=100,
    validation_split=0.2,
    batch_size=16,
    #callbacks = EarlyStopping(monitor='mae', mode = 'min', patience = 10 ,verbose = 1),
    verbose=1
  )
  #Saving the model so that it can be reused
  model %>% save_model_tf("models/nn")
  print(history)
  evalNN(model,train_y,test_x,test_y,train_y_notNormalised,test_y_notNormalised)
  return(model)
} #endof trainNN()


# ---------------------------------------------- Code execution ----------------------------------------------

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

#carsToTrain <- subset(carsToTrain, select = -c(`fuel encoded`,`transmission encoded`,`type encoded`,`size encoded`,`cylinders encoded`,`drive encoded`, `paintcolor encoded`))
carsToTrain<-subset(carsToTrain,odometer <= 300000 & odometer >= 1000)
carsToTrain<-subset(carsToTrain,year >= 2000)
#carsToTrain$odometer <- round(carsToTrain$odometer, -4)
#carsToTrain$price <- round(carsToTrain$price, -2)

mostCommonModels<-tail(names(sort(table(carsToTrain$`modelencoded`))),30)
carsToTrain<-subset(carsToTrain, `modelencoded` %in% mostCommonModels)

smp_size<-floor(0.9*nrow(carsToTrain))

indices<-sample(1:nrow(carsToTrain))
folds<-cut(1:length(indices),
           breaks=K_FOLDS,
           labels=FALSE)

if(TRAIN_MODEL==TRUE){
  for(k in 1:K_FOLDS){
    test_indices<-which(folds==k,arr.ind=TRUE)
    test_x<- carsToTrain[test_indices,]
    test_y<- carsToTrain[test_indices,]
    train_x<-carsToTrain[-test_indices,]
    train_y<-carsToTrain[-test_indices,]
    train_x<-subset(train_x,select=-c(price))
    train_y<-subset(train_y,select=c(price) )
    test_x<-subset(test_x,select=-c(price))
    test_y<-subset(test_y,select=c(price))
    train_y_notNormalised<-train_y
    test_y_notNormalised<-test_y
    train_x<-normalize(train_x)
    train_y<-normalize(train_y)
    test_x<-normalize(test_x)
    test_y<-normalize(test_y)
    train_y$price<-normalize(train_y$price)
    trainNN(train_x,train_y,test_x,test_y,train_y_notNormalised,test_y_notNormalised)
    trainGradientBoost(train_x,train_y,test_x,test_y,train_y_notNormalised,test_y_notNormalised)
  }
}


