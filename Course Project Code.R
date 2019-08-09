# PACKAGES
library(caret)
library(randomForest)
library(dplyr)

# FILES
training<-read.csv("pml-training.csv")
testing<-read.csv("pml-testing.csv")

# PREPROCESSING
## REMOVE UNNECESSARY COLUMNS
training<-training[,-c(1,2)]
testing<-testing[,-c(1,2)]
str(testing)
colnames(training)
str(training)
View(head(training))

## REMOVE ZERO VARIANCE - NO DIFFERENCE BETWEEN SAMPLES
near <- nearZeroVar(training, saveMetrics = T)
training_clean <- training[, !near$nzv] 
testing_clean <- testing[, !near$nzv]

## REMOVE COLUMNS COMPLETELY ZERO
training_clean <- training_clean[, colSums(is.na(training_clean)) != nrow(training_clean)]

## REPLACING MISSING VALUES WITH COLUMN MEAN
for(i in 1:ncol(training_clean)){
  training_clean[is.na(training_clean[,i]), i] <- mean(training_clean[,i], 
                                                       na.rm = TRUE)
}
for(i in 1:ncol(testing_clean)){
  testing_clean[is.na(testing_clean[,i]), i] <- mean(testing_clean[,i], na.rm = TRUE)
}


# TRAINING AND VALIDATION SPLIT
set.seed(1234)
inTrain <- createDataPartition(training_clean$classe,p=0.8,list=FALSE)
train <- training_clean[inTrain,]
validate <- training_clean[-inTrain,]

# TESTING DATA SET
for_test<-select(testing,which(colnames(testing) %in% colnames(train)))

# RANDOM FOREST MODEL
model1<-randomForest(classe~.,data=train)
model1
## PREDICT
pred1<-predict(model1,validate[,-99],type="class")
mean(pred1 == validate$classe)  
table(pred1,validate$classe)
pred_test<-predict(model1,testing_clean[-98],type="class")
View(pred_test)
colnames_train<-colnames(train)
colnames_test<-colnames(testing_clean)
columnn<-cbind(colnames_train,colnames_test)
View(columnn)
str(testing_clean)
testing_clean[testing_clean=='NaN']<-0
str(train)
str(testing_clean)
levels(testing_clean$cvtd_timestamp) <- levels(train$cvtd_timestamp)
str(training)
