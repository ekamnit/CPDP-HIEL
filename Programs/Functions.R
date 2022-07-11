########################################################
# Function Declarations Section
# Declaring the training controls for multiple models
# Adaptive Bootstrap Validation technique is used
########################################################
fitControl <- trainControl(method = "cv", 
                           number = 10, 
                           savePredictions = 'final', 
                           classProbs = T)
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
########################################################
rfControl <- trainControl(method = "repeatedcv",
                           number = 10, repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random")
## Declaring the normalization function
########################################################
nor <-function(x) {(x -min(x))/(max(x)-min(x))}
########################################################
## Declaring the accuracy function
########################################################
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
###################################
# Precision, Recall and F1-Measure
###################################
measurePrecisionRecall <- function(predict, actual_labels){
precision <- sum(predict & actual_labels) / sum(predict)
recall <- sum(predict & actual_labels) / sum(actual_labels)
fmeasure <- 2 * precision * recall / (precision + recall)
  
cat('precision:  ')
cat(precision * 100)
cat('%')
cat('\n')
  
cat('recall:     ')
cat(recall * 100)
cat('%')
cat('\n')

cat('f-measure:  ')
cat(fmeasure * 100)
cat('%')
cat('\n')
}