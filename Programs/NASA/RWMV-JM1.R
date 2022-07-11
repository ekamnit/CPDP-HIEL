# Data collection: Merging all individual files (Which contain class level metrics), totalling to 30.
#setwd('/media/pramod/6E88CA0788C9CE31/Research/SixthSem/NASADefectDataset-master/Cleaned/Data1/')
CM1 <- read.csv('CM1.csv')
JM1 <- read.csv('JM1.csv')
KC1 <- read.csv('KC1.csv')
KC3 <- read.csv('KC3.csv')
MC1 <- read.csv('MC1.csv')
MC2 <- read.csv('MC2.csv')
MW1 <- read.csv('MW1.csv')
PC1 <- read.csv('PC1.csv')
PC2 <- read.csv('PC2.csv')
PC3 <- read.csv('PC3.csv')
PC4 <- read.csv('PC4.csv')
PC5 <- read.csv('PC5.csv')
# Collecting the attributes from CM1, MW1, PC3 and PC4
CM1 <- CM1[,c(3,5,6,8,12,15,17,19,20,21,22,23,24,25,26,32,33,34,35,38,39)]
JM1 <- JM1[,c(3:23)]
KC1 <- KC1[,c(3:23)]
KC3 <- KC3[,c(3,5,6,8,12,15,17,21,22,23,24,25,26,27,28,34,35,36,37,40,41)]
MC1 <- MC1[,c(3,5,6,8,11,14,16,20,21,22,23,24,25,26,27,33,34,35,36,39,40)]
MC2 <- MC2[,c(3,5,6,8,12,15,17,21,22,23,24,25,26,27,28,34,35,36,37,40,41)]
MW1 <- MW1[,c(3,5,6,8,12,15,17,19,20,21,22,23,24,25,26,32,33,34,35,38,39)]
PC1 <- PC1[,c(3,5,6,8,12,15,17,19,20,21,22,23,24,25,26,32,33,34,35,38,39)]
PC2 <- PC2[,c(2,4,5,7,11,14,16,18,19,20,21,22,23,24,25,31,32,33,34,37,38)]
PC3 <- PC3[,c(3,5,6,8,12,15,17,19,20,21,22,23,24,25,26,32,33,34,35,38,39)]
PC4 <- PC4[,c(3,5,6,8,12,15,17,19,20,21,22,23,24,25,26,32,33,34,35,38,39)]
PC5 <- PC5[,c(3,5,6,8,11,14,16,20,21,22,23,24,25,26,27,33,34,35,36,40,41)]
#############################################
# Getting attributes
attributes(CM1)$names
attributes(JM1)$names
attributes(KC1)$names
attributes(KC3)$names
attributes(MC1)$names
attributes(MC2)$names
attributes(MW1)$names
attributes(PC1)$names
attributes(PC2)$names
attributes(PC3)$names
attributes(PC4)$names
attributes(PC5)$names
#############################################
Defective <- JM1$label
JM1 <- JM1[,-21]
JM1 <- cbind(JM1,Defective)
#############################################
# Data Fusion:
#############################################
NASA <- rbind(CM1,KC1,KC3,MC1,MC2,MW1,PC1,PC2,PC3,PC4,PC5)
NASA <- na.omit(NASA)
sampledDataForTraining <- c((nrow(unique(NASA)))*10)
PopulationData <- unique(NASA)
testSet <- JM1
TestDepVar <- testSet$Defective
TestDepVar <- ifelse(TestDepVar == "Y", "1", "0")
TestDepVar <- as.integer(TestDepVar)
PopulationData <- PopulationData[c(1:nrow(unique(NASA))),]
###############################################
# Networks = 10
###############################################
for (i in c(1:10)) {
  #set.seed(runif(1))
  samplingData <- PopulationData[sample(nrow(PopulationData), replace = TRUE),]
  sampledDataForTraining <- rbind(sampledDataForTraining,samplingData)
}
sampledData <- sampledDataForTraining[c(2:nrow(sampledDataForTraining)),]
##############################################
# Macros: LR Stuff
PredsLR <- replicate(10*nrow(testSet),0)
LR_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Logistic Regression Classifier
  ########################################################
  # Partition data training - 90%, test - 10%
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testSet)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  trainSet$Defective <- ifelse(trainSet$Defective == "Y", "1", "0")
  TrainDepVar <- as.integer(trainSet$Defective)
  ########################################################
  # Training the Logistic regression model
  modelLR <- glm(TrainDepVar~ ., data = trainSet[,-21], family = binomial(link <- "logit"))
  # Testing using LR model on TestSet
  pred_probsLR <- predict(object = modelLR, newdata = testSet[,c(1:20)], type = "response")
  pred_LR <- ifelse(pred_probsLR > 0.5, "1", "0")
  PredsLR[c(d:e)] <- pred_LR
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
##############################################
TestDepVar <- testSet$Defective
TestDepVar <- ifelse(TestDepVar == "Y", "1", "0")
TestDepVar <- as.integer(TestDepVar)
##############################################
PredsKNN <- replicate(10*nrow(testSet),0)
KNN_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Logistic Regression Classifier
  ########################################################
  # Partition data training - 90%, test - 10%
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testSet)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  trainSet$Defective <- ifelse(trainSet$Defective == "Y", "1", "0")
  TrainDepVar <- as.integer(trainSet$Defective)
  ########################################################
  # Training the K-NN model
  model_KNN <- knn(train = trainSet[,c(1:20)], test = testSet[,c(1:20)], cl = trainSet[,21], k =12)
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
##############################################
TestDepVar <- testSet$Defective
TestDepVar <- ifelse(TestDepVar == "Y", "1", "0")
TestDepVar <- as.integer(TestDepVar)
##############################################
PredsSVM <- replicate(10*nrow(testSet),0)
SVM_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method Using SVM
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # SVM Classifier
  ########################################################
  # Partition data training - 90%, test - 10%
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testSet)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  trainSet$Defective <- ifelse(trainSet$Defective == "Y", "1", "0")
  TrainDepVar <- as.integer(trainSet$Defective)
  ########################################################
  # Training the SVM Model
  model_SVM = svm(formula = TrainDepVar ~ ., data = trainSet[,-21], type = 'C-classification', kernel = 'linear') 
  ########################################################
  # Prediction using Test Set 
  pred_SVM = predict(model_SVM, newdata = testSet[-21])
  PredsSVM[c(d:e)] <- pred_SVM
  CM_SVM <- table(pred_SVM ,TestDepVar)
  SVM_Accuracies[i] <- accuracy(CM_SVM)
  ########################################################
  j <- b
  k <- e
}
#PredsSVM <- PredsSVM + 1
max(SVM_Accuracies)
min(SVM_Accuracies)
SVM_Accuracies
##############################################
# Macros: Naive Bayes Stuff
##############################################
TestDepVar <- testSet$Defective
TestDepVar <- ifelse(TestDepVar == "Y", "1", "0")
TestDepVar <- as.integer(TestDepVar)
##############################################
PredsNB <- replicate(10*nrow(testSet),0)
NB_Accuracies <- replicate(10,0)
testSetNB <- testSet
TestDepVar <- factor(TestDepVar, levels = c(0,1), labels = c("False", "True"))
##############################################
# Building Ensemble Learning Method Using NB
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # NB Classifier
  ########################################################
  # Partition data training - 90%, test - 10%
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testSet)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  #Setting outcome variables as categorical
  trainSetNB <- trainSet
  trainSet$Defective <- ifelse(trainSet$Defective == "Y", "1", "0")
  TrainDepVar <- as.integer(trainSet$Defective)
  TrainDepVar <- factor(TrainDepVar, levels = c(0,1), labels = c("False", "True"))
  ########################################################
  # Training the NB Model
  model_NB <- naiveBayes(TrainDepVar~., data=trainSetNB, laplace = 1)
  ########################################################
  # Prediction using Test Set
  Predict_NB <- predict(model_NB, newdata = testSetNB[,-21], type = "class")
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
##############################################
# DT Stuff
##############################################
TestDepVar <- testSet$Defective
TestDepVar <- ifelse(TestDepVar == "Y", "1", "0")
TestDepVar <- as.integer(TestDepVar)
##############################################
PredsDT <- replicate(10*nrow(testSet),0)
DT_Accuracies <- replicate(10,0)
##############################################
# Building Ensemble Learning Method
##############################################
j <- 0
k <- 0
for (i in c(1:10)) {
  ########################################################
  # Logistic Regression Classifier
  ########################################################
  # Partition data training - 90%, test - 10%
  # Creating Bootstrap Samples
  ########################################################
  # Declarations Section
  set.seed(runif(1))
  a <- j+1
  b <- i*nrow(PopulationData)
  d <- k+1
  e <- i*nrow(testSet)
  ########################################################
  trainSet <- sampledData[c(a:b),]
  trainSet$Defective <- ifelse(trainSet$Defective == "Y", "1", "0")
  TrainDepVar <- as.integer(trainSet$Defective)
  ########################################################
  # Building the classification tree with rpart
  model_DT <- rpart(TrainDepVar ~ ., data = trainSet[,-21], method = "class")
  ########################################################
  # Prediction
  PredsDT[c(d:e)] <- predict(object = model_DT, testSet[,-21], type = "class")
  CM_DT <- table(TestDepVar, PredsDT[c(d:e)])
  DT_Accuracies[i] <- accuracy(CM_DT)
  #print(CM_DT)
  #print(DT_Accuracies[i])
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
RWMVHIELFinalPredictions <- replicate(nrow(testSet),0)
RMVPredictions <- rbind(as.numeric(PredsLR),as.numeric(PredsDT),as.numeric(PredsKNN),
                        as.numeric(PredsNB),as.numeric(PredsSVM))
RWMVWeights <- replicate(50,1)
tempProbs <- replicate(50,0)
tempPreds <- replicate(50,0)
WeightSum <- sum(RWMVWeights)
Beta <- 0.1
for (i in c(1:nrow(testSet))) {
  #i<- 1
  k <- i
  temp <- TestDepVar[i]
  for (j in c(1:50)) {
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
    k <- k + nrow(testSet)
  }
  if(WeightSum==0){
    RWMVHIELFinalPredictions <- replicate(nrow(testSet),0)
    RWMVWeights <- replicate(50,1)
    tempProbs <- replicate(50,0)
    tempPreds <- replicate(50,0)
    WeightSum <- sum(RWMVWeights)
  }
  maxVal <- tempPreds[which.max(tempProbs)]
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
for(i in c(1:nrow(testSet))) {
  TotalLoC <- TotalLoC + testSet$LOC_TOTAL[i]
}
for(i in c(1:nrow(testSet))) {
  if(TestDepVar[i]==0 && RWMVHIELFinalPredictions[i]==0){
    CountLoC <- CountLoC + testSet$LOC_TOTAL[i]
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
diverse10 <- c(0.9081,0.9076,0.9075,0.9075,0.9066,0.9066,0.9066,0.9066,0.9066)
diverse20 <- c(0.9132,0.9132,0.9118,0.9118,0.9118,0.9118,0.9118,0.9118,0.9118)
diverse30 <- c(0.9149,0.9086,0.9086,0.9086,0.9086,0.9086,0.9086,0.9086,0.9086)
diverse40 <- c(0.9207,0.9207,0.9207,0.9207,0.9207,0.9207,0.9207,0.9207,0.9207)
diverse50 <- c(0.9224,0.9217,0.9217,0.9217,0.9217,0.9217,0.9217,0.9217,0.9217)
diverse60 <- c(0.9231,0.9231,0.9231,0.9231,0.9231,0.9231,0.9231,0.9231,0.9231)

png(file = "JM.jpg")
# Plot the bar chart.
plot(diverse10, type = "o",col = "red", xlab = "β Values", ylab = "F-Measure Values",
     ylim=c(0.9,0.94), lwd = 2)
lines(diverse20, type = "o", col = "blue", lwd = 2)
lines(diverse30, type = "o", col = "orange", lwd = 2)
lines(diverse40, type = "o", col = "darkgreen", lwd = 2)
lines(diverse50, type = "o", col = "brown", lwd = 2)
lines(diverse60, type = "o", col = "black", lwd = 2)

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
