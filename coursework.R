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

# Forest
NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 1000                 # Number of trees in the forest
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage

KFOLDS            <- 6                 # Number of folded experiments

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
# C50                    0.1.3.1

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "ggplot2",
               "stats",
               "PerformanceAnalytics",
               "caret",
               "C50")

# User defined functions are next
# ************************************************
# getTreeClassifications() :
#
# Put in test dataset and get out class predictions of the decision tree
# Determine the threshold, plot the results and calculate metrics
#
# INPUT : object - myTree - tree
# : Data Frame - testDataset - dataset to evaluate
# : string - title - string to plot as the chart title
# : int - classLabel - lable given to the positive (TRUE)class
# : boolean - plot - TRUE to output results/charts
#
# OUTPUT : List - Named evaluation measures
#
# ************************************************
getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(myTree,test_inputs,type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE)
    NprintMeasures(results=measures,title=title)
  
  return(measures)
} #endof getTreeClassifications()

# ************************************************
# fullDT() :
#
# Create C5 Decision Tree on pre-processed dataset
#
# INPUT   :
#             Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             int            - boost       - number of trees to boost
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
fullDT<-function(train,test,boost=1,plot=TRUE){
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  # ************************************************
  # Create a standard Decision Tree using the C5.0 algorithm
  # Uses library C50
  # Outputs the tree in the format of rules
  
  myTitle<-"Preprocessed Dataset. DT C5.0"
  if (boost>1)
    myTitle<-paste(myTitle,"BOOSTED=",boost)
  
  print(myTitle)
  
  tree<-C50::C5.0(x=train_inputs,
                  factor(train_expected),
                  rules=TRUE,
                  trials=boost)
  
  # Use the created decision tree with the test dataset
  # to determine best classification threshold & calculate metrics
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    
    print(summary(tree))
    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=myTitle)
    
    # ************************************************
    # We can visualise the tree
    
    #Function to output the tree as rules to a file
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
    
    # ************************************************
    # Creates C5.0 decision tree & plot as a tree structure
    # The "partykit" library requires the variables (wrongly) to be global
    print("Plot decision tree to file called tree.pdf")
    
    Global_train_inputs<<-train_inputs
    Global_train_expected<<-train_expected
    
    # :: is used to specify a function within the named package to avoid confusion
    tree<-C50::C5.0(x=Global_train_inputs,
                    factor(Global_train_expected),
                    trials=boost)
    
    # ::: is used to directly access a member of a package that is internal
    graphtree<-C50:::as.party.C5.0(tree)
    
    # The plot is large - so print to a big PDF file
    pdf(PDF_FILENAME, width=100, height=50, paper="special", onefile=F)
    
    # The number is the node level of the tree to print
    plot(graphtree[NODE_LEVEL])
    
    #This closes the PDF file
    dev.off()
    
  }
  return(measures)
} #endof fullDT()

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
source("functions.R")
set.seed(123)

# Loading the data
carsInitial <- NreadDataset(DATASET_FILENAME)
cars <- carsInitial
# cars <- NPREPROCESSING_prettyDataset(cars)



# Removing useless columns and 
# id, url, county, regionurl, imageurl, lat, long, description, vin, state, region, titlestatus
cars <- subset(cars, select = -c(id, url, county, regionurl, imageurl, lat, long, description, vin,state,region, titlestatus))

# ---------------------------------------------- Data clean-up ----------------------------------------------
# Remove rows without designated model/manufacturer
cars <- subset(cars, manufacturer != "" & model != "")
# Make sure price column is numeric
cars <- subset(cars, is.numeric(price))
# Remove rows with invalid or "bad" prices, typically <200
cars <- subset(cars, price >= 200)
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
