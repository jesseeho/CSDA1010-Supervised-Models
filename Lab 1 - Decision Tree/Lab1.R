#### LIBRARIES #################
library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(Amelia)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
#### LOAD FILES ################
rm(list = ls())
mush=read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA","?"))

#View data structure with HEAD(), STR(), and SUMMARY()
head(mush,10)
str(mush)
summary(mush)

#################Determine Missing Values####################################
# Checking missing values (missing values or empty values)
colSums(is.na(mush)|mush=='')

#missmap allows us to explore how much missing data we have.

missmap(mush, main="Mushroom Data - Missings Map",
        col=c("yellow", "black"), legend=FALSE)


#remove Veil.type = Null values impacting corrplot
mush_Veil=mush$veil.type
mush$veil.type=NULL

#convert factor categories to numeric categories
mush_matrix=data.matrix(mush)
str(mush_matrix)

#################################### CORRELATOIN MATRIX ##################################
#Advanced Corr matrix
cor_result=rcorr(mush_matrix)

# Extract the correlation coefficients
cor_result$r

# flattenCorrMatrix - identified that veil.type causing error.
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Simple method to flatten (if that how you want to look at it)
cor_result_flat = flattenCorrMatrix(cor_result$r, cor_result$P)
head(cor_result_flat,1000)

library(corrplot)
corrplot(cor_result$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


###################    IMPUTING MISSING DATA     ###########################


####### BONUS: KNN TO IMPUTE  ####
#make sure to remove target variable when doing imputation - when you push model to produciton you want have this!
#also KNN is based on a distance metric so requires normalization/scaling
knn_input=as.data.frame(mush_matrix[, !names(mush_matrix) %in% c("poisonous.")]) #removed target
str(knn_input)
sample_knn_input=sample_n(knn_input, 8124)
nrow(sample_knn_input)

#normalize/stardardize everything
#option 1: create your own apply() function to normalize
#normalize = function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}
#sample_knn_input_normal = as.data.frame(lapply(sample_knn_input, normalize))
#summary(sample_knn_input_normal)


#Run KNN imputation for this sample - warning might take a while....?knnimputation

#separate mush data into mush_select (only data with correlation to stalk.root per correlation matrix)
mush_select=mush[ ,c("stalk.color.above.ring","stalk.color.below.ring","cap.surface","gill.size","gill.spacing","stalk.root")]
mush_other = mush[,-which(names(mush)%in% names(mush_select))]

#KNN Imputation on mush_select
library(VIM)
knnoutput = kNN(mush_select, k = 5)
summary (knnoutput)

#combine cleaned data and 
mush_imputed=knnoutput[ ,c("stalk.color.above.ring","stalk.color.below.ring","cap.surface","gill.size","gill.spacing","stalk.root")]
mush_cleaned=cbind(mush_other, mush_imputed)
summary(mush_cleaned)




#### MODEL #####################
#split cleaned data into training and test sets
set.seed(8124)
training = createDataPartition(mush_cleaned$poisonous., p=0.6,list = FALSE)
train_cleaned = mush_cleaned[training,]
test_cleaned = mush_cleaned[-training,]
test_poisonous = test_cleaned$poisonous.#remove poisounous (target) from test
test_cleaned$poisonous.=NULL #remove poisounous (target) from test

head(test_cleaned,10)

# Supervised Learning: Decision Tree
DT <- rpart(poisonous. ~., train_cleaned, method = "class", cp=0)
summary(DT)
printcp(DT)
rpart.plot(DT, type=1, extra = 102)

#### OUTPUT ####################

#training accuracy
t_pred <- predict(DT, train_cleaned, type = "class")
confMat <- table(train_cleaned$poisonous.,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

predict_dt <- predict(DT, test_cleaned, type = "class")
result <- data.frame(poisonous. = predict_dt)
result_test = cbind(result, Actuals = test_poisonous)
#result <- data.frame(PassengerID = test_cleaned$PassengerID., Survived = predict_dt)
write.csv(result_test, file=file.choose(), row.names = FALSE)

#k fold cross validation

# load the library
library(caret)
# load the iris dataset
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
set.seed(7)
# train the model
train_cleaned$poisonous.=as.factor(train_cleaned$poisonous.)
rpart.grid <- expand.grid(.cp=0.0)
kf_DT <- train(poisonous.~., train_cleaned, method="rpart", trControl=train_control,tuneGrid=rpart.grid)
# summarize results
print(kf_DT)
t_pred <- predict(kf_DT, train_cleaned, type="raw")
confMat <- table(train_cleaned$poisonous.,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

#random forest
library(randomForest)
rf.grid <- expand.grid(.mtry=5.0)
kf_rf <- train(poisonous.~., train_cleaned, method="rf", trControl=train_control,tuneGrid=rf.grid)
#NOTE: you can use "." to have the decision tree select for you. I.e. "Survived~."

# summarize results
print(kf_rf)
t_pred <- predict(kf_rf, train_cleaned, type="raw")
confMat <- table(train_cleaned$poisonous.,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

