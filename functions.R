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
# 15th February 2019
#
# v1.01
#
# UPDATE
# 1.00      1/2/2017    Initial Version
# 1.10      24/2/2019   Only code for rule output now inlcuded
# 1.11      8/8/2018    N_DEEP_TrainClassifier(), N_DEEP_Initialise reproducable parameter for h2o learning
# 1.12      27/9/2019   N_DEEP_Initialise() reproducable is optional parameter
# 1.13      22/10/2019  Added NROCgraph() which is modified myPerformancePlot() from lab3.R
# 1.14                  Created NdetermineThreshold()
#                       NDT5RuleOutput() now outputs class label as BAD or GOOD loan
#           27/10/2019  Added NConvertClass() top reduce clutter in lab code
#           31/10/2019  NdetermineThrehold(), added axis bound checks in abline plots
#           29/5/2020   NdetermineThreshold() added quiet parameter & use mindist as threshold
#           19/10/2020  NdetermineThreshold() use won ROC chart, as lab 3
# ************************************************

# ************************************************
# NConvertClass() :
#
# In original dataset, $Status is the classification label
# We need to convert this to give the minority class a value of 0
# this just makes it easiert to define the confusioon matrix!
# for the UCI-G this is a class of {0,1} being {bad, good}
#
# INPUT   :
#             Data Frame        - dataset
#
# OUTPUT  :
#             Data Frame        - dataset
#
# ************************************************
NConvertClass<-function(dataset){
  positionClassOutput<-which(names(dataset)==OUTPUT_FIELD)
  classes<-sort(table(dataset[,positionClassOutput])) #smallest class will be first
  minority<-names(classes[1])
  indexToStatus2<-which(dataset[positionClassOutput]==minority)
  dataset[positionClassOutput][indexToStatus2,]<-0
  dataset[positionClassOutput][-indexToStatus2,]<-1
  return(dataset)
}

# ************************************************
# NEvaluateClassifier() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
NEvaluateClassifier<-function(test_predicted,test_expected,threshold) {

  predictedClass<-ifelse(test_predicted<threshold,0,1)

  results<-NcalcConfusion(expectedClass=test_expected,
                          predictedClass=predictedClass)

  return(results)
} #endof NEvaluateClassifier()

# ************************************************
# NdetermineThreshold() :
#
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Deterime "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - probs        - probability of being class 1
#         :   Data Frame     - testing_data - dataset to evaluate
#         :   boolean        - plot         - TRUE=create charts otherwise don't
#         :   string         - title        - string to plot as the chart title
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# 241019NRT - added plot flag and title for charts
# 311019NRT - added axis bound checks in abline plots
# 191020NRT - Updated to use own ROC plot
# ************************************************
NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){

  # Helper local scope function
  getFirst<-function(values){
    if (length(values)>1){
      return(values[1])
    } else
      return(values)
  }

  toPlot<-data.frame()

  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-NEvaluateClassifier(test_predicted=test_predicted,
                                  test_expected=test_expected,
                                  threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }

  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  # Youdan = sensitivty + specificity -1
  #        = TPR + (1-FPR) -1

  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1

  # 121020NRT - max Youdan
  # use which.max() to return a single index to the higest value in the vector
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]

  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))

  # 121020NRT - Euclidean distance to "perfect" classifier (smallest the best)
  # use which.min() to return a single index to the lowest value in the vector
  minEuclidean<-toPlot$x[which.min(toPlot$distance)]

  # ************************************************
  # Plot threshold graph

  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))

    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)

    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]

    if (!is.na(crosspoint)){
      if ((crosspoint<1) & (crosspoint>0))
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }

    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)

    # Plot the min distance, as might be more (311019NRT check it is within range)
    if ((minEuclidean<1) & (minEuclidean>0))
      abline(v=minEuclidean,col="green",lty=3,lwd=2)

    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)

    if ((maxYoudan<1) & (maxYoudan>0))
      abline(v=maxYoudan,col="purple",lty=3,lwd=2)

    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
    text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nEuclidean=",minEuclidean,"\nYoudan=",maxYoudan))

    # ************************************************
    # 121020NRT ROC graph

    plot(100-toPlot$fpr,toPlot$tpr,type="l",lwd=3, col="blue",
         main=paste("ROC:",title),
         xlab="Specificity (1-FPR) %",
         ylab="Sensitivity (TPR) %",
         xlim=(c(100,0)))

    sensitivityROC<-toPlot$tpr[which.min(toPlot$distance)]
    specificityROC<-100-toPlot$fpr[which.min(toPlot$distance)]

    #Add crosshairs to the graph
    abline(h=sensitivityROC,col="red",lty=3,lwd=2)
    abline(v=specificityROC,col="red",lty=3,lwd=2)

    annotate<-paste("Threshold: ",round(minEuclidean,digits=4L),
                    "\nTPR: ",round(sensitivityROC,digits=2L),
                    "%\n1-FPR: ",round(specificityROC,digits=2L),"%",sep="")

    text(x=specificityROC, y=sensitivityROC, adj = c(-0.2,1.2),cex=1, col="red",annotate)

  } # endof if plotting

  # Select the threshold - I have choosen distance

  myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]

  #Use the "best" distance threshold to evaluate classifier
  results<-NEvaluateClassifier(test_predicted=test_predicted,
                                test_expected=test_expected,
                                threshold=myThreshold)

  results$threshold<-myThreshold

  return(results)
} #endof myPerformancePlot()

# ************************************************
# NprintDTRules() :
#
# INPUT: text - filename
#
# OUTPUT : Frame - dataset
# ************************************************
NprintDTRules<-function(dtrules, filename){

  sink(filename)

  for (r in 1:nrow(dtrules)){
    print(paste("[",r,"]",dtrules$Rule[r],"==",(ifelse(dtrules$Class[r]==0,"BAD","GOOD"))))
  }
  sink()
}
# ************************************************
# DECISION TREE CONVERT DT RULES TO ASCII FORMATTED RULES
#
# <anticedent 1> AND <anticedent 2> ...
# Each anticedent is: [field][comparision][value]
#
# INPUT: Object - tree - Trained tree
#
# OUTPUT: data frame of rules, class and anticedents
# ************************************************
NDT5RuleOutput<-function(tree){
  #library(stringr)
  x<-summary(tree)[1]
  x<-substr(x,regexpr("Rules:",x)[1]+8,nchar(x))
  x<-substr(x,1,regexpr("Evaluation on training data",x)[1]-1)
  x<-gsub("[\n\t]", "*", x)
  df_of_rules<-data.frame(matrix(ncol=3,nrow=tree$size),stringsAsFactors = FALSE)
  df_of_rules<-setNames(df_of_rules,c("Rule","Class","Anti"))

  numberofrules<-tree$size
  # 271019 allow for multiple trees (i.e. boosted)
  if (length(numberofrules)>1){
    numberofrules<-numberofrules[1]
    warning("Prof Nick says: More than one tree found. Extracting rules for just the first")
  }

  totalAnticedents<-0
  for (ruleNumber in 1:numberofrules){
    start<-regexpr("\\*\\*",x)[1]+2
    end<-regexpr("->",x)[1]-3
    onerule<-substr(x,start,end) #Single rule, anticedents seperated by '**'
    onerule<-gsub("\\*\\*"," AND ",onerule) #Rule now has "AND" between anticedents
    #onerule<-convertNormalisedDTRuleToRealWorld(onerule)
    NumAnticedents<-str_count(onerule,"AND")+1
    totalAnticedents=totalAnticedents+NumAnticedents
    classpos<-regexpr("class ",x)+6
    classID<-as.numeric(substr(x,classpos,classpos))  #This has the class of the rule, i.e. {0,1}
    df_of_rules$Rule[ruleNumber]<-onerule
    df_of_rules$Class[ruleNumber]<-ifelse(classID==0,"BAD","GOOD") # Convert class to label
    df_of_rules$Anti[ruleNumber]<-NumAnticedents
    x<-substr(x,classpos,nchar(x))
    st<-regexpr("\\*\\*",x)[1]+2 #move past the rule ID
    x<-substr(x,st,nchar(x))
  }
  return(df_of_rules)
}

# ************************************************
# N_DEEP_Initialise()
# Initialise the H2O server
#
# INPUT:
#         Bool       - reproducible       - TRUE if model must be reproducable each run
#
# OUTPUT : none
# ************************************************
N_DEEP_Initialise<-function(reproducible=TRUE){

  library(h2o)

  print("Initialise the H2O server")
  #Initialise the external h20 deep learning local server if needed
  #130517NRT - set nthreads to -1 to use maximum so fast, but set to 1 to get reproducable results
  #080819NRT - use reproducable parameter
  if (reproducible==TRUE)
    nthreads<-1
  else
    nthreads<- -1

  h2o.init(max_mem_size = "5g",nthreads = nthreads)

  h2o.removeAll() # 261019NRT clean slate - just in case the cluster was already running
  #h2o.no_progress()
}

# ************************************************
# N_DEEP_TrainClassifier()
#
# h2O NEURAL NETWORK : DEEP LEARNING CLASSIFIER TRAIN
#
# INPUT:  Frame      - train              - scaled [0.0,1.0], fields & rows
#         String     - fieldNameOutput    - Name of the field to classify
#         Int Vector - hidden             - Number of hidden layer neurons for each layer
#         int        - stopping_rounds    - Number of times no improvement before stop
#         double     - stopping_tolerance - Error threshold
#         String     - activation         - Name of activation function
#         Bool       - reproducible       - TRUE if model must be reproducable each run
#
# OUTPUT: object     - trained neural network
# ************************************************
N_DEEP_TrainClassifier<- function(train,
                                  fieldNameOutput,
                                  hidden,
                                  stopping_rounds,
                                  stopping_tolerance,
                                  activation,
                                  reproducible){

  #positionOutput<-which(names(train)==fieldNameOutput)

  #Creates the h2o training dataset
  train[fieldNameOutput] <- lapply(train[fieldNameOutput] , factor) #Output class has to be a R "factor"

  train_h2o <- as.h2o(train, destination_frame = "traindata")

  # Create validation dataset for early stopping
  splits <- h2o.splitFrame(train_h2o, 0.9, seed=1234)
  nntrain  <- h2o.assign(splits[[1]], "nntrain.hex") # 90%
  nnvalid  <- h2o.assign(splits[[2]], "nnvalid.hex") # 10%

  #This lists all the input field names ignoring the fieldNameOutput
  predictors <- setdiff(names(train_h2o), fieldNameOutput)

  # Deep training neural network
  # Updated 13/5/17 - set reproducible = TRUE so that the same random numbers are used to initalise
  # 281019NRT - added validation dataset for early stopping

  deep<-h2o::h2o.deeplearning(x=predictors,
                              y=fieldNameOutput,
                              training_frame = nntrain,
                              validation_frame=nnvalid,
                              epochs=BASICNN_EPOCHS,
                              hidden=hidden,
                              adaptive_rate=TRUE,
                              stopping_rounds=stopping_rounds,
                              stopping_tolerance=stopping_tolerance,
                              stopping_metric = "misclassification",
                              fast_mode=FALSE,
                              activation=activation,
                              seed=1234,
                              l1 = 1e-2,
                              l2 = 1e-2,
                              variable_importances = TRUE,
                              reproducible = TRUE)
  return(deep)
}

# ************************************************
# N_EVALUATE_DeepNeural() :
#
# Evaluate Deep Neural Network classifier
# Generates probabilities from the classifier
#
# INPUT: Data Frame    -  test             - scaled [0.0,1.0], fields & rows
#        String        -  fieldNameOutput  - Name of the field that we are training on (i.e.Status)
#        Object         - deep             - trained NN including the learn weights, etc.
#         boolean      - plot              - TRUE = output charts/results
#         string       - myTitle           - title on results
#
# OUTPUT :
#         list - metrics from confusion matrix
# ************************************************
# Uses   library(h2o)

N_EVALUATE_DeepNeural<-function(test,fieldNameOutput, deep,plot,myTitle){

  #Creates the h2o test dataset
  test[fieldNameOutput] <- lapply(test[fieldNameOutput] , factor) #Output class has to be a R "factor"
  test_h2o <- as.h2o(test, destination_frame = "testdata")

  pred <- h2o::h2o.predict(deep, test_h2o)

  test_predicted<-as.vector(pred$p1)  #Returns the probabilities of class 1

  positionClassOutput<-which(names(test)==fieldNameOutput)

  # train data: vector with the expedcted output
  test_expected<-test[,positionClassOutput]

  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predicted,
                                plot=plot,
                                title=myTitle)

  if (plot==TRUE)
    NprintMeasures(results=measures,title=myTitle)

  return(measures)
}

# ************************************************
# N_MLP_TrainClassifier()
#
# MLP NEURAL NETWORK
#
# INPUT:  Frame      - train              - scaled [0.0,1.0], fields & rows
#         String     - fieldNameOutput    - Name of the field to classify
#         Int Vector - hidden             - Number of hidden layer neurons for each layer
#         boolean    - plot               - TRUE = output charts/results
#
# OUTPUT: object     - trained neural network
# ************************************************
N_MLP_TrainClassifier<- function(train,
                                  fieldNameOutput,
                                  hidden,
                                  plot
                                  ){

  positionClassOutput<-which(names(train)==fieldNameOutput)

  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]

  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]

  x<-as.matrix(train_inputs)
  y<-keras::to_categorical(train_expected,num_classes = 2)

  mlp_classifier = keras_model_sequential()

  # add layers, first layer needs input dimension
  mlp_classifier %>%
    keras::layer_dense(input_shape = ncol(x), units=ncol(x), activation = "relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = hidden, activation = "relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = 2, activation = "softmax")

  # add a loss function and optimizer
  mlp_classifier %>%
    keras::compile(
      loss = "categorical_crossentropy",
      optimizer = "adagrad",
      metrics = "accuracy"
    )

  # train model with our training data set
  fit = mlp_classifier %>%
    keras::fit(
      x = x,
      y = y,
      shuffle = T,
      batch_size = 5,
      validation_split = 0.2,
      epochs = BASICNN_EPOCHS,
      callbacks = c(
        callback_early_stopping(monitor = "val_loss", patience = 8, mode = "auto")),
      verbose=0, view_metrics=0
    )

  # Plot the neural network error (loss) udring training
  if (plot==TRUE)
    print(plot(fit))

  return(mlp_classifier)
}

# ************************************************
# N_EVALUATE_MLP() :
#
# Evaluate MLP Neural Network classifier
# Generates probabilities from the classifier
#
# INPUT: Data Frame    -  testing_data     - scaled [0.0,1.0], fields & rows
#        String        -  fieldNameOutput  - Name of the field that we are training on (i.e.Status)
#        Object        - mlp_classifier    - trained NN including the learn weights, etc.
#         boolean      - plot              - TRUE = output charts/results
#         string       - myTitle           - title on results
#
# OUTPUT :
#         list - metrics from confusion matrix
# ************************************************

N_evaluate_MLP<-function(test,fieldNameOutput,mlp_classifier,plot,myTitle){

  positionClassOutput<-which(names(test)==fieldNameOutput)

  # test data: dataframe with with just input fields
  test_inputs<-test[-positionClassOutput]

  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)

  testPredictedAllClassProbs<-predict(mlp_classifier,as.matrix(test_inputs))

  # Probabilities for just class 1
  testPredictedClassProbs<-testPredictedAllClassProbs[,2]

  # train data: vector with the expedcted output
  test_expected<-test[,positionClassOutput]

  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=testPredictedClassProbs,
                                plot=plot,
                                title=myTitle)
  if (plot==TRUE)
    NprintMeasures(results=measures,title=myTitle)

  return(measures)
}

