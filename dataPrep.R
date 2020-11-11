# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  COM3018/COMM053
#
# Prof. Nick F Ryman-Tubb
# Department of Computer Science
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# UPDATE
# 1.00      1/2/2017    Initial Version
# 1.01      14/2/2018   NPREPROCESSING_discreetNumeric - changed for total of ten bins
#                       Added bin number as x-axis label
# 1.02      26/2/2018   Added NcalcMeasures(), updated NcalcConfusion() to use this.
#                       Updated NPREPROCESSING_splitdataset() with descripition
#                       Updated Npreprocessdataset() with scaleFlag
# 1.03      2/3/2018    Renamed Npreprocessdataset() to NPREPROCESSING_dataset()
#                       Added library(neuralnet) inside NLEARN_BasicNeural()
# 1.04      5/3/2018    Added global constants - although these values should be PASSED
#                       NPREPROCESSING_dataset() added vector manualTypes
#                       Updated all functions to include manualTypes where required
#                       Added NPREPROCESSING_setInitialFieldType()
#                       Added MAX_LITERALS as a constant
#                       Added NPREPROCESSING_removeAllSameValueField()
#                       NPREPROCESSING_categorical() does not convert very sparse fields
#                       Removed N_LEARN_BasicNeural() and put in lab functions source
# 1.05      7/3/2018    NPREPROCESSING_discreetNumeric() updated to show plot for all numeric fields
#                       & title on plot shows if field type manually set or determined by preprocessing
# 1.06      18/2/2019   For lab 3
# 1.07      19/2/2019   Fixed NPREPROCESSING_discreetNumeric() with scale
# 1.08      24/2/2019   Updated NcalcConfusion()
#                       Added NROCgraph(),NPREPROCESSING_prettyDataset()
#           9/3/2019    Updated manual setting of field types
#           6/8/2019    NPREPROCESSING_categorical() added "_" to make field name easier to read
#                       NPREPROCESSING_dataset() fixed scaling to do each column seperatley
#                       pass confidence as parameter for outlier detection
#           7/8/2019    NPREPROCESSING_outlier() - if negative confidence then do not replace outlier
#                       NROCgraph() - updated as the pROC library had changed
#                       NcalcConfusion() - convert values to doubles to avoid integers overflowing
#                       NprintMeasures() - output to viewer only
#           8/8/2019    NplotConfusion() - added new function for pretty confusion chart
#           21/8/2019   Removed border from the plot
# 1.09      17/10/2019  Uddates for PBA lab 3
# 1.10      22/10/2019  Nrescaleentireframe() return the scaled values in an object
#                       NPLOT_correlagram() plot abs() value correlations only
# 1.11                  For PBA lab 4, added NPREPROCESSING_dataset()
# 1.12      11/5/2020   NPREPROCESSING_outlier() Fixed bug to either replace with MEAN or REMOVE records
# 1.13      23/5/2020   NPREPROCESSING_redundantFields() bug fix for multiple correlations
# 1.14      2/6/2020    NPREPROCESSING_outlier() - Fixed bug to process all identified outlier records
# ************************************************

# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()

# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){

  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}

# ************************************************
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){

  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}

# ************************************************
# NPREPROCESSING_removePunctuation()
#
# INPUT: String - fieldName - name of field
#
# OUTPUT : String - name of field with punctuation removed
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}

# ************************************************
# NreadDataset() :
#
# Read a CSV file from working directory
#
# INPUT: string - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset<-function(csvFilename){

  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)

  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))

  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# NPREPROCESSING_setInitialFieldType() :
#
# Set  each field for NUMERIC or SYNBOLIC
#
# INPUT:
#        String - name - name of the field to manually set
#        String - type - manual type
#
# OUTPUT : None
# ************************************************
NPREPROCESSING_setInitialFieldType<-function(name,type){

  #Sets in the global environment
  manualTypes<<-rbind(manualTypes,data.frame(name=name,type=type,stringsAsFactors = FALSE))
}

# ************************************************
# NPREPROCESSING_initialFieldType() :
#
# Test each field for NUMERIC or SYNBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
NPREPROCESSING_initialFieldType<-function(dataset){

  field_types<-vector()
  for(field in 1:(ncol(dataset))){

    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }

    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}

# ************************************************
# NPREPROCESSING_discreetNumeric() :
#
# Test NUMERIC field if DISCREET or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discreet (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCREET, ORDINAL}
# ************************************************
# Uses histogram
# Plots histogram for visulisation
# ************************************************
NPREPROCESSING_discreetNumeric<-function(dataset,field_types,cutoff){

  #For every field in our dataset
  for(field in 1:(ncol(dataset))){

    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {

      #Scale the whole field (column) to between 0 and 1
      scaled_column<-Nrescale(dataset[,field])

      #Generate the "cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)

      #This creates an empty vector that will hold the counts of ther numbers in the bin range
      bins<-vector()

      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }

      # the 10 bins will have a % value of the count (i.e. density)
      bins<-(bins/length(scaled_column))*100.0

      graphTitle<-"AUTO:"

      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discreet value

      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCREET
      else
        field_types[field]<-TYPE_ORDINAL

      #Bar chart helps visulisation. Type of field is the chart name
      barplot(bins, main=paste(graphTitle,field_types[field]),
              xlab=names(dataset[field]),
              names.arg = 1:10,bty="n")

    } #endif numeric types
  } #endof for
  return(field_types)
}

# ************************************************
# NPREPROCESSING_categorical() :
#
# Transform SYMBOLIC or DISCREET fields using 1-hot-encoding
#
# INPUT: data frame    - dataset      - symbolic fields
#        vector string - field_types  - types per field {ORDINAL, SYMBOLIC, DISCREET}
#
# OUTPUT : data frame    - transformed dataset
# ************************************************
# Small number of literals only otherwise too many dimensions
# Uses 1-hot-encoding if more than 2 unique literals in the field
# Otherwise converts the 2 literals into one field of {0,1}
# ************************************************
NPREPROCESSING_categorical<-function(dataset,field_types){

  #This is a dataframe of the transformed categorical fields
  catagorical<-data.frame(first=rep(NA,nrow(dataset)),stringsAsFactors=FALSE)

  #For every field in our dataset
  for(field in 1:(ncol(dataset))){

    #Only for fields marked SYMBOLIC or DISCREET
    if ((field_types[field]==TYPE_SYMBOLIC)||(field_types[field]==TYPE_DISCREET)) {

      #Create a list of unique values in the field (each is a literal)
      literals<-as.vector(unique(dataset[,field]))
      numberLiterals<-length(literals)

      #if there are just two literals in the field we can convert to 0 and 1
      if (numberLiterals==2){
        transformed<-ifelse (dataset[,field]==literals[1],0.0,1.0)
        catagorical<-cbind(catagorical,transformed)
        colnames(catagorical)[ncol(catagorical)]<-colnames(dataset)[field]

      } else
      {
        #We have now to one-hot encoding FOR SMALL NUMBER of literals
        if (numberLiterals<=MAX_LITERALS){
          for(num in 1:numberLiterals){
            nameOfLiteral<-literals[num]
            hotEncoding<-ifelse (dataset[,field]==nameOfLiteral,1.0,0.0)

            # 5/3/2018 - do not convert the field if their are too few literals
            # Use log of number of recrods as the measure
            literalsActive<-sum(hotEncoding==1)
            if (literalsActive>log(length(hotEncoding))) {
              catagorical<-cbind(catagorical,hotEncoding)
              #060819 field name has the "_" seperator to make easier to read
              colnames(catagorical)[ncol(catagorical)]<-paste(colnames(dataset)[field],
                                                              "_",
                                                              NPREPROCESSING_removePunctuation(nameOfLiteral),
                                                              sep="")
            }
            else {
              warning(paste("Prof Nick says: Ignoring in field:",
                            names(dataset)[field],
                          "Literal:",nameOfLiteral,
                          "Too few=",literalsActive))
            }
          }
        } else {
          stop(paste("Prof Nick says: Error - too many literals in:",
                     names(dataset)[field],
                     numberLiterals))
        }

      }
    }
  }

  return(catagorical[,-1]) #Remove that first column that was full of NA due to R
}

# ************************************************
# NplotOutliers() :
#
# Scatter plot of field values and colours outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
#
# OUTPUT : None
# ************************************************
NplotOutliers<-function(sorted,outliers,fieldName){

  plot(1:length(sorted),sorted,
       pch=1,
       xlab="Unique records",
       ylab=paste("Sorted values",fieldName),
       bty="n")

  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}

# ************************************************
# NPLOT_correlagram() :
#
# Plots PLOT_correlagram
#
# INPUT: data frame - cr - n x n frame of correlation coefficients
#
# OUTPUT : None
# 221019 - plot absolute values only
# ************************************************
NPLOT_correlagram<-function(cr){

  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))

  #To fir on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)

  corrplot::corrplot(abs(cr),method="square",
                     order="FPC",
                     cl.ratio=0.2,
                     cl.align="r",
                     tl.cex = 0.6,cl.cex = 0.6,
                     cl.lim = c(0, 1),
                     mar=c(1,1,1,1),bty="n")
}

# ************************************************
# NPREPROCESSING_redundantFields() :
#
# Determine if an entire field is redundant
# Uses LINEAR correlation,
# so use with care as information will be lost
#
# INPUT: data frame - dataset - numeric values only
#        double     - cutoff  - Value above which is determined redundant [0,1]
#
# OUTPUT : Frame - dataset with any fields removed
#
# Updated: 230529NRT c
# ************************************************
NPREPROCESSING_redundantFields<-function(dataset,cutoff){

  print(paste("Before redundancy check Fields=",ncol(dataset)))

  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1

  if (length(xx)>0L)
    dataset<-dataset[,-xx]

  # "Kendall" is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")

  NPLOT_correlagram(cr)

  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]

  if (length(list_fields_correlated)>0){

    print("Following fields are correlated")
    print(list_fields_correlated)

    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    v<-unique(v)  #230529NRT fields might repeat as correlated with more than one
    print(paste("Removing the", length(v),"named fields"))
    print(names(dataset)[v])

    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}

# ************************************************
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame    - dataset   - complete data set
#          vector string - field_types  - types per field {ORDINAL, SYMBOLIC, DISCREET}
#          double        - confidence - Confidence above which is determined an outlier [0,1]
#          string        - operation  = "ignore" = make no changes
#                                     = "mean"   = replace with field mean
#                                     = "remove" = delete the entire record
#
# OUTPUT : data frame - data set with outlier values: ignored, replaced or deleted
#
# 110520NRT - Fixed bug to either replace with MEAN or DELETE records
# 020620NRT - Fixed bug to process all identified outlier records
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdfß

NPREPROCESSING_outlier<-function(dataset,field_types, confidence, operation="remove"){

  #For everyfield in our dataset
  for(field in 1:(ncol(dataset))){

    #Only for fields that are all numeric
    if (field_types[field]==TYPE_ORDINAL) {

      #020620NRT  Assign data frame for just this field with the values and TRUE/FALSE  based on confidence level
      justField<-data.frame(v=dataset[,field],outlier=outliers::scores(dataset[,field],type="chisq",prob=abs(confidence)))
      indexToOutliers<-which(justField$outlier)
      numberOfOutliers<-length(indexToOutliers)

      # 020620NRT This sorts the entire dataframe from low values to high
      # and then plot
      sortedData<-justField[order(justField$v),]

      plot(1:nrow(sortedData),sortedData$v,
           pch=1,
           xlab="Records",
           ylab=paste("Sorted values",colnames(dataset)[field]),
           bty="n")

      # If outlier(s) detected then show on the plot and process
      if (numberOfOutliers>0){

        # Highlight outliers as red plots
        indexToSortedOutliers<-which(sortedData$outlier)
        points(indexToSortedOutliers,sortedData$v[indexToSortedOutliers],col="red",pch=19)

        #If found records with outlier values
        switch(operation,

               "mean"= {
                       dataset[indexToOutliers,field]<-mean(dataset[,field])
                       print(paste("REPLACED WITH MEAN: Outlier field=",names(dataset)[field],"#Records=",numberOfOutliers))
                       },
               "remove"={
                          dataset<-dataset[-indexToOutliers,]
                          print(paste("DELETED RECORDS: Outlier field=",names(dataset)[field],"#Records=",numberOfOutliers))
                         },
              "ignore"=  {
                         print(paste("NO REPLACEMENT: Outlier field=",names(dataset)[field],"#Records=",numberOfOutliers))
                         }
               )
      } #endof if any outliers found
    } #endof if ordinal
  } #endof for() each field

  return(dataset)
}

# ************************************************
# NprintMeasures()
#
# Output measures to the Viewer
#
# INPUT:    list -   results - results from NcalcConfusion()
#           string - title   - title of the table
#
# OUTPUT :  NONE
#
# 070819NRT updated to output table to viewer only
# 171019NRT added column name "Metric"
# 241019NRT added title
# ************************************************
NprintMeasures<-function(results,title){

  #This outputs our results into the "Viewer" in RStudio
  tidyTable<-data.frame(t(t(results)))
  names(tidyTable)[1]<-title

  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}

# ************************************************
# NplotConfusion()
#
# Plot confusion matrix
#
# INPUT:    list - results - results from NcalcConfusion()
#
# OUTPUT :  NONE
#
# 070819NRT Plots confusion matrix
# ************************************************
NplotConfusion<-function(results){

  aa<-matrix(c(round(results$TP,digits=0),
               round(results$FN,digits=0),
               round(results$FP,digits=0),
               round(results$TN,digits=0)),
             nrow=2)
  row.names(aa)<-c("Fraud","Genuine")
  colnames(aa)<-c("Fraud","Genuine")
  fourfoldplot(aa,color=c("#cc6666","#99cc99"),
               conf.level=0,
               margin=2,
               main="TP  FP / FN   TN")
} #endof NplotConfusion()

# ************************************************
# Nrmse() :
#
# Calculate the RMSE statistic
#
# INPUT: vector - actual_y     -  numbers indicating the known class
#        vector - y_predicted  - numbers indicating the predicted class
#
# OUTPUT : double - calculated RMSE
# ************************************************
Nrmse<-function(actual_y,y_predicted){

  return(sqrt(mean((actual_y-y_predicted)^2)))
}
# ************************************************
# NcalcMeasures() :
#
# Evaluation measures for a confusion matrix
#
# INPUT: numeric  - TP, FN, FP, TN
#
# OUTPUT: A list with the following entries:
#        TP        - double - True Positive records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        FN        - double - False Negative records
#        accuracy  - double - accuracy measure
#        pgood     - double - precision for "good" (values are 1) measure
#        pbad      - double - precision for "bad" (values are 1) measure
#        FPR       - double - FPR measure
#        TPR       - double - FPR measure
#        TNR       - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
#
# 080819NRT added TNR measure
# ************************************************
NcalcMeasures<-function(TP,FN,FP,TN){

  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  "pgood"=   100.0*(TP/(TP+FP)),
                  "pbad"=    100.0*(TN/(FN+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  )
  return(retList)
}

# ************************************************
# NcalcConfusion() :
#
# Calculate a confusion matrix for 2-class classifier
# INPUT: vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#        vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#
# OUTPUT: A list with the  entries from NcalcMeasures()
#
# 070819NRT convert values to doubles to avoid integers overflowing
# Updated to the following definition of the confusion matrix
#
# A good loan is indicated when $Status=1 and bad when $Status=0

#                    ACTUAL
#               ------------------
# PREDICTED     GOOD=1   |  BAD=0
#               ------------------
#     GOOD=1      TP     |    FP
#               ==================
#     BAD=0       FN     |    TN
#
#
# ************************************************
NcalcConfusion<-function(expectedClass,predictedClass){

  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))

  # This "converts" the above into our preferred format

  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])

  return(NcalcMeasures(TP,FN,FP,TN))

} #endof NcalcConfusion()

# ************************************************
# NPREPROCESSING_splitdataset() :
#
# Randomise and split entire data set
#
# INPUT: data Frame - combinedML - dataset
#
# OUTPUT : data Frame - test dataset
#          data Frame - train dataset
# 241019 use the global HOLDOUT
# ************************************************
NPREPROCESSING_splitdataset<-function(combinedML){

  # **** Create a TRAINING dataset using HOLDOUT % of the records

  combinedML<-combinedML[order(runif(nrow(combinedML))),]
  training_records<-round(nrow(combinedML)*(HOLDOUT/100))

  train <- 1:training_records
  test <- -train

  training_data <- combinedML[train,]
  testing_data = combinedML[test,]

  retList<-list("train"=training_data,
                "test"=testing_data)
  return(retList)
}

# ************************************************
# NPREPROCESSING_prettyDataset()
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
NPREPROCESSING_prettyDataset<-function(dataset,...){

  params <- list(...)

  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)

  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }

  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }

  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}

# ************************************************
# preprocessdataset() :
#
# Run the steps discussed to pre-process a dataset
#
# INPUT: data frame - dataset    - original (raw) dataset
#        Bool       - scaleFlag  - true to scale dataset
#
# OUTPUT : Frame - dataset
# ************************************************
NPREPROCESSING_dataset<-function(dataset, scaleFlag=FALSE){

  NPREPROCESSING_prettyDataset(dataset)

  # ************************************************
  # Determine initial field types: NUMERIC or SYMBOLIC
  field_types<-NPREPROCESSING_initialFieldType(dataset)

  numeric_fields<-names(dataset)[field_types==TYPE_NUMERIC]
  print(numeric_fields)

  symbolic_fields<-names(dataset)[field_types==TYPE_SYMBOLIC]
  print(symbolic_fields)

  # ************************************************
  # Determine if the numeric fields might be discreet numeric
  # If there are over 3 bins with less than 1% of the values, then the field is
  # marked as a discreet numeric
  field_types<-NPREPROCESSING_discreetNumeric(dataset=dataset,
                                              field_types=field_types,
                                              cutoff=CUTOFF_DISCREET)

  # ************************************************
  # FOR ORDINAL TYPES:

  # ************************************************
  # Outlier detection
  #
  # If the p-value<significance (e.g. p=0.05, confidence=95%)
  # Operation can be: "ignore", "mean" or "remove"
  # NRT110520 updated parameters to pass whole dataset andoperation
  dataset<-NPREPROCESSING_outlier(dataset=dataset,
                                   field_types=field_types,
                                   confidence=CUTOFF_OUTLIER,
                                   operation="remove")

  # The entire dataset is returned, as some records may have been removed

  ordinals<-dataset[,field_types==TYPE_ORDINAL]

  if (scaleFlag==TRUE){
    # ************************************************
    # Now z-scale
    zscaled<-apply(ordinals, MARGIN = 2,
                   FUN = function(X) (scale(X,center=TRUE,
                                            scale=TRUE)))

    # ************************************************
    # Scale in this case to be [0.0,1.0]
    ordinalReadyforML<-Nrescaleentireframe(as.data.frame(zscaled))

  } else
  {
    ordinalReadyforML<-ordinals
  }
  # We now have a frame called ordinalReadyforML of
  # just the numeric fields, nice and ready for the ML

  # ************************************************
  # IF SYMBOLIC TYPES:
  # This function undertakes 1-hot-encoding

  catagoricalReadyforML<-NPREPROCESSING_categorical(dataset = dataset,
                                                    field_types=field_types)

  # ************************************************
  # Are any of the fields of both the numeric and symbolic pre-processed datasets redundant?

  #Combine the two sets of data that are read for ML
  combinedML<-cbind(ordinalReadyforML,catagoricalReadyforML)

  # Are any of the fields redundant?
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=CUTOFF_REDUNDANT)

  #The dataset for ML information
  print(paste("Fields=",ncol(combinedML)))

  # ************************************************
  # If teh names of the fields contain spaces then they
  # "confuse" some of the library algorithms
  # This removes the spaces.
  names(combinedML)<-gsub(" ", "", names(combinedML), fixed = TRUE)

  # ************************************************
  # Returns the pre-processed dataset
  return(combinedML)
}
