library(ggplot2)
library(reshape2)
library(plyr)
library(car)
library(rattle)
library(gvlma)
library (caret)

rm(list = ls())
datawhite.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
datawhite<- read.csv(datawhite.url, header = TRUE, sep = ";")

############################################################
#######################White Analysis#######################
############################################################

####Box Cox Transformation - Normalize Data####
#we are scaling, centering and transformting data
datawhite_raw = preProcess(datawhite[,1:11],c("BoxCox","center","scale"))
datawhite_normalized = data.frame(trans = predict(datawhite_raw, datawhite))

#Visualize data pre-transformation - note that the values have crazy range and distribution
str(datawhite)
ggplot(data = datawhite, mapping = aes(x = fixed.acidity)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = volatile.acidity)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = citric.acid)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = residual.sugar)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = chlorides)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = free.sulfur.dioxide)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = total.sulfur.dioxide)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = density)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = pH)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = sulphates)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = alcohol)) + geom_bar()
ggplot(data = datawhite, mapping = aes(x = quality)) + geom_bar()
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(datawhite$fixed.acidity, h=0.5, col = "slategray3", main="Fixed Acidity")
truehist(datawhite_normalized$trans.fixed.acidity, h=0.5, col = "slategray3", main="Fixed Acidity")
#Visualize data post transformation (Box Cox): normalized distribution - normalized with scale -5 to 5
str(datawhite_normalized)
ggplot(data = datawhite_normalized, mapping = aes(x = trans.fixed.acidity)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.volatile.acidity)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.citric.acid)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.residual.sugar)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.chlorides)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.free.sulfur.dioxide)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.total.sulfur.dioxide)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.density)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.pH)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.sulphates)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.alcohol)) + geom_bar()
ggplot(data = datawhite_normalized, mapping = aes(x = trans.quality)) + geom_bar()

#checking for outliers using box plots
boxplot(datawhite_normalized$trans.fixed.acidity) #outliers
boxplot(datawhite_normalized$trans.volatile.acidity) #outliers
boxplot(datawhite_normalized$trans.citric.acid) #outliers
boxplot(datawhite_normalized$trans.residual.sugar) #no outliers
boxplot(datawhite_normalized$trans.chlorides) #outliers
boxplot(datawhite_normalized$trans.free.sulfur.dioxide) #outliers
boxplot(datawhite_normalized$trans.total.sulfur.dioxide) #outliers
boxplot(datawhite_normalized$trans.density) #outliers
boxplot(datawhite_normalized$trans.pH) #outliers
boxplot(datawhite_normalized$trans.sulphates) #outliers
boxplot(datawhite_normalized$trans.alcohol)#no outliers
boxplot(datawhite_normalized$trans.quality) #outliers

#removing outliers
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.fixed.acidity)>3,]
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.volatile.acidity)>3,]
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.citric.acid)>3,]
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.chlorides)>3,]
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.free.sulfur.dioxide)>3,]
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.total.sulfur.dioxide)>3,]
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.density)>3,]
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.pH)>3,]
datawhite_norm = datawhite_normalized[!abs(datawhite_normalized$trans.sulphates)>3,]


datawhite_select = data.frame(datawhite_norm$trans.fixed.acidity,datawhite_norm$trans.volatile.acidity, datawhite_norm$trans.citric.acid, datawhite_norm$trans.residual.sugar,datawhite_norm$trans.chlorides,datawhite_norm$trans.free.sulfur.dioxide, datawhite_norm$trans.total.sulfur.dioxide, datawhite_norm$trans.pH,datawhite_norm$trans.sulphates, datawhite_norm$trans.alcohol, datawhite_norm$trans.quality)

####automated feature selection####
#uses Recursive Feature Elimination (random forrest algorithm on each iteration to evaluate the model)
control = rfeControl(method = "repeatedcv", repeats = 5, verbose = TRUE, functions = lmFuncs) #define looping fucntions (5 iterations)
whitewineRFE = rfe(x = datawhite_select[,1:10], y = datawhite_select[,11], sizes = c(1:10), metrix = "RMSE", rfeControl = control)
print(whitewineRFE)
importance <- varImp(whitewineRFE, scale=FALSE)
print(importance)

###create new dataframe using top 5 predictors identified by random forest###
datawhite_predict= data.frame(datawhite_norm$trans.alcohol, datawhite_norm$trans.volatile.acidity, datawhite_norm$trans.residual.sugar,datawhite_norm$trans.free.sulfur.dioxide,datawhite_norm$trans.total.sulfur.dioxide,datawhite_norm$trans.quality)
head(datawhite_predict)

#*******USE datawhite_predict in your model ********




