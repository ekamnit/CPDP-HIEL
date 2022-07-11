# Load all the projects
data <- rbind(Eclipse,Equinox,Lucene,Mylyn)
#########################################
defect <- replicate(nrow(data),0)
for (i in 1:nrow(data)) {
  if (data$bugs[i] >= 1)
  {
    defect[i] = 1
  }
  else
  {
    defect[i] = 0
  }
}
data = data[,c(-1,-19,-20,-21,-22,-23,-24)]
data = cbind(data,defect)
data = na.omit(data)
sampledDataForTraining <- c((nrow(unique(data)))*10)
PopulationData <- unique(data)
testData <- PDE[,c(-1,-20,-21,-22,-23,-24)]
TestDepVar <- ifelse(testData$bug >= 1, "1", "0")
TestDepVar <- as.integer(TestDepVar)
###############################################
# Networks = 10
###############################################
for (i in c(1:10)) {
  samplingData <- PopulationData[sample(nrow(PopulationData), replace = TRUE),]
  sampledDataForTraining <- rbind(sampledDataForTraining,samplingData)
}
sampledData <- sampledDataForTraining[c(2:nrow(sampledDataForTraining)),]
###############################################
# Macros: LR Stuff
PredsLR <- replicate(10*nrow(testData),0)
LR_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testData)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  TrainDepVar <- as.integer(trainSet$defect)
  ########################################################
  # Training the Logistic regression model
  modelLR <- glm(TrainDepVar~ ., data = trainSet[,-18], family = binomial(link <- "logit"))
  # Testing using LR model on TestSet
  pred_probsLR <- predict(object = modelLR, newdata = testData[,c(1:17)], type = "response")
  pred_LR <- ifelse(pred_probsLR > 0.5, "1", "0")
  PredsLR[c(d:e)] <- as.integer(pred_LR)
  CM_LR <- table(TestDepVar, PredsLR[c(d:e)])
  LR_Accuracies[i] <- accuracy(CM_LR)
  j <- b
  k <- e
}
max(LR_Accuracies)
min(LR_Accuracies)
LR_Accuracies
##############################################
# Macros: K-NN Stuff
PredsKNN <- replicate(10*nrow(testData),0)
KNN_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testData)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  TrainDepVar <- as.integer(trainSet$defect)
  ########################################################
  # Training the K-NN model
  model_KNN <- knn(train = trainSet[,c(1:17)], test = testData[,c(1:17)], cl = trainSet[,18], k =11)
  model_KNN
  ########################################################
  PredsKNN[c(d:e)] <- model_KNN # Need to get clarity why the values gets incremented by 1
  CM_KNN <- table(model_KNN ,TestDepVar)
  KNN_Accuracies[i] <- accuracy(CM_KNN)
  ########################################################
  j <- b
  k <- e
}
max(KNN_Accuracies)
min(KNN_Accuracies)
KNN_Accuracies
PredsKNN <- PredsKNN - 1
##############################################
# Macros: SVM Stuff
PredsSVM <- replicate(10*nrow(testData),0)
SVM_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method Using SVM
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testData)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  TrainDepVar <- as.integer(trainSet$defect)
  ########################################################
  # Training the SVM Model
  model_SVM = svm(formula = TrainDepVar ~ ., data = trainSet[,-18], type = 'C-classification', kernel = 'linear') 
  ########################################################
  # Prediction using Test Set 
  pred_SVM = predict(model_SVM, newdata = testData[-18])
  PredsSVM[c(d:e)] <- pred_SVM
  CM_SVM <- table(pred_SVM ,TestDepVar)
  SVM_Accuracies[i] <- accuracy(CM_SVM)
  ########################################################
  j <- b
  k <- e
}
beep()
PredsSVM <- PredsSVM - 1
max(SVM_Accuracies)
min(SVM_Accuracies)
SVM_Accuracies
##############################################
# Macros: Naive Bayes Stuff
PredsNB <- replicate(10*nrow(testData),0)
NB_Accuracies <- replicate(10,0)
testDataNB <- testData
TestDepVar <- factor(TestDepVar, levels = c(0,1), labels = c("False", "True"))
##############################################
# Building Ensemble Learning Method Using NB
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testData)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  #Setting outcome variables as categorical
  trainSetNB <- trainSet
  TrainDepVar <- as.integer(trainSet$defect)
  TrainDepVar <- factor(TrainDepVar, levels = c(0,1), labels = c("False", "True"))
  ########################################################
  # Training the NB Model
  model_NB <- naiveBayes(TrainDepVar~., data=trainSetNB, laplace = 1)
  ########################################################
  # Prediction using Test Set
  Predict_NB <- predict(model_NB, newdata = testDataNB[,-18], type = "class")
  #Get the confusion matrix to see accuracy value and other parameter values
  Predict_NB <- as.integer(as.logical(Predict_NB))
  PredsNB[c(d:e)] <- Predict_NB
  CM_NB <- table(Predict_NB ,as.integer(as.logical(TestDepVar)))
  NB_Accuracies[i] <- accuracy(CM_NB)
  ########################################################
  j <- b
  k <- e
}
max(NB_Accuracies)
min(NB_Accuracies)
NB_Accuracies
TestDepVar <- as.integer(as.logical(TestDepVar))
##############################################
# Macros: NN Stuff
PredsNN <- replicate(10*nrow(testData),0)
NN_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testData)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  TrainDepVar <- as.integer(trainSet$defect)
  ########################################################
  # Training the Neural Network model
  model_NN <- neuralnet(TrainDepVar~ ., data=trainSet[,-18], algorithm = "rprop+", 
                        threshold = 0.5, hidden=2,lifesign = "minimal",
                        stepmax = 1e+05,rep=5)
  ########################################################
  # Testing using NN model on TestSet
  pred_probsNN <- predict(object = model_NN, newdata = testData[,c(1:17)], type = "response")
  pred_NN <- ifelse(pred_probsNN > 0.5, "1", "0")
  PredsNN[c(d:e)] <- pred_NN
  CM_NN <- table(TestDepVar, PredsNN[c(d:e)])
  NN_Accuracies[i] <- accuracy(CM_NN)
  j <- b
  k <- e
}
max(NN_Accuracies)
min(NN_Accuracies)
NN_Accuracies
beep()
##############################################
# DT Stuff
##############################################
PredsDT <- replicate(10*nrow(testData),0)
DT_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testData)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  TrainDepVar <- as.integer(trainSet$defect)
  ########################################################
  # Building the classification tree with rpart
  model_DT <- rpart(TrainDepVar ~ ., data = trainSet[,-18], method = "class")
  ########################################################
  # Prediction
  PredsDT[c(d:e)] <- predict(object = model_DT, testData[,-18], type = "class")
  CM_DT <- table(TestDepVar, PredsDT[c(d:e)])
  DT_Accuracies[i] <- accuracy(CM_DT)
  j <- b
  k <- e
}
max(DT_Accuracies)
min(DT_Accuracies)
DT_Accuracies
PredsDT <- PredsDT - 1
beep()
##############################################
# Classification Stage
##############################################
# HIEL
########################################################
# Randomized Weighted Majority Voting implementaion
########################################################
RWMVHIELFinalPredictions <- replicate(nrow(testData),0)
RMVPredictions <- rbind(as.numeric(PredsLR),as.numeric(PredsDT),as.numeric(PredsKNN),
                        as.numeric(PredsNB),as.numeric(PredsSVM),as.numeric(PredsNN))
RWMVWeights <- replicate(60,1)
tempProbs <- replicate(60,0)
tempPreds <- replicate(60,0)
WeightSum <- sum(RWMVWeights)
Beta <- 0.1
for (i in c(1:nrow(testData))) {
  #i<- 1
  k <- i
  temp <- TestDepVar[i]
  for (j in c(1:60)) {
    x1 <- RMVPredictions[k]
    x1 <- as.numeric(x1)
    tempPreds[j] <- x1
    if(x1==temp){
      tempProbs[j] <- RWMVWeights[j]/WeightSum
    }
    else{
      RWMVWeights[j] <- RWMVWeights[j]*Beta
      tempProbs[j] <- RWMVWeights[j]/WeightSum
    }
    k <- k + nrow(testData)
  }
  if(WeightSum==0){
    RWMVHIELFinalPredictions <- replicate(nrow(testData),0)
    RWMVWeights <- replicate(60,1)
    tempProbs <- replicate(60,0)
    tempPreds <- replicate(60,0)
    WeightSum <- sum(RWMVWeights)
  }
  maxVal <- tempPreds[which.max(tempProbs)]
  print(maxVal)
  WeightSum <- sum(RWMVWeights)
  RWMVHIELFinalPredictions[i] <- maxVal
}
HIEL_CM_Final <- table(TestDepVar, RWMVHIELFinalPredictions)
HIEL_CM_Final
##############################################
# Performance Measures
##############################################
# 1. F-measure, AUC, Accuracy
##############################################
F_meas(data = HIEL_CM_Final)
RoC <- roc(RWMVHIELFinalPredictions, TestDepVar)
auc(RoC)
accuracy(HIEL_CM_Final)
##############################################
# 2. Percent of Perfect Cleans
##############################################
CountLoC <- 0
TotalLoC <- 0
for(i in c(1:nrow(testData))) {
  TotalLoC <- TotalLoC + testData$lcom[i]
}
for(i in c(1:nrow(testData))) {
  if(TestDepVar[i]==0 && RWMVHIELFinalPredictions[i]==0){
    CountLoC <- CountLoC + testData$lcom[i]
  }
}
PPC <- HIEL_CM_Final[1]/length(TestDepVar)
PPC
PSC <- CountLoC/TotalLoC
PSC
SavedCost <- CountLoC
SavedCost
##############################################
# 3. Percent of Non-Perfect Cleans
##############################################
PNPC <- 1- (HIEL_CM_Final[1]/length(TestDepVar))
PNPC
PRE <- (TotalLoC-CountLoC)/TotalLoC
PRE
RemainingEdits <- TotalLoC-CountLoC
RemainingEdits
##############################################
# 4. False Omission Rate
##############################################
FOR <- HIEL_CM_Final[2]/(HIEL_CM_Final[1]+HIEL_CM_Final[2])
FOR*100
Failures <- HIEL_CM_Final[2]
Failures
##############################################
# Ploting
##############################################
diverse10 <- c(0.8241,0.8159,0.8151,0.8049,0.8049,0.8049,0.8049,0.8049,0.8049)
diverse20 <- c(0.8285,0.8239,0.8263,0.8235,0.8263,0.8263,0.8263,0.8263,0.8263)
diverse30 <- c(0.8512,0.8505,0.8369,0.8313,0.8369,0.8369,0.8369,0.8369,0.8369)
diverse40 <- c(0.8437,0.8432,0.8358,0.8363,0.8371,0.8299,0.8348,0.8223,0.8215)
diverse50 <- c(0.8566,0.8509,0.8334,0.8556,0.8334,0.8322,0.8322,0.8322,0.8322)
diverse60 <- c(0.8589,0.8586,0.8439,0.8391,0.8311,0.8309,0.8307,0.8307,0.8307)

png(file = "Eclipse.jpg")
# Plot the bar chart.
plot(diverse10, type = "o",col = "red", xlab = "β Values", ylab = "F-Measure Values",
     ylim=c(0.8,0.86), lwd = 2)
lines(diverse20, type = "o", col = "blue", lwd = 2)
lines(diverse30, type = "o", col = "orange", lwd = 2)
lines(diverse40, type = "o", col = "darkgreen", lwd = 2)
lines(diverse50, type = "o", col = "brown", lwd = 2)
lines(diverse60, type = "o", col = "black", lwd = 2)
# Save the file.
legend("topright", 
       legend = c("Classifiers = 10", "Classifiers = 20", "Classifiers = 30",
                  "Classifiers = 40", "Classifiers = 50", "Classifiers = 60"), 
       col = c("red", "blue", "orange", "darkgreen", "brown", "black"), 
       pch = c(16, 16, 16, 16, 16, 16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F)
dev.off()

