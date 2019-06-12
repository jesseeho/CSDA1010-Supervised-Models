## LAB 2: Regression Model - Wine Quality
## Model wine quality based on physicochemical characteristics

## Load Library
library("ggplot2")
library("Amelia")
library("corrplot")
library("hexbin")
library("descr")
library("reshape2")
library("plyr")
library("car")
library("MASS")
library("rattle")
library("gvlma")
library("psych")

## datasets URL
dataRed.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
dataWhite.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"

## datasets
dataRed <- read.csv(dataRed.url, header = TRUE, sep = ";")
dataWhite <- read.csv(dataWhite.url, header = TRUE, sep = ";")


##############################################################################
## RED ANALYSIS
##############################################################################
head(dataRed)
summary(dataRed)

str(dataRed)


describe(dataRed)
cor(dataRed[, -12])
cor(dataRed[, -12], method = "spearman")
pairs(dataRed[, -12], gap=0, pch=19, cex=0.4, col="darkblue")
title(sub="Scatterplot of Chemical Attributes", cex=0.8)

# Correlation
par(mfrow = c(1,1))
corrplot(cor(dataRed), method = "ellipse")


## Check for missing/empty values
colSums(is.na(dataRed) | dataRed=="" | dataRed==0)

## missmap explores how much missiong data there is
missmap(dataRed, main="Wine Quality: Red - MIssings Map",
        col=c("yellow", "black"), legend = FALSE)

## Distribution data by QUality
ggplot(data = dataRed, mapping = aes(x = quality)) +
  geom_bar()


## Relation of physicochemical characteristics vs quality
par(mfrow = c(2,2))

boxplot(fixed.acidity~quality, data = dataRed, xlab = "Quality", ylab = "Fixed Acidity")
boxplot(volatile.acidity~quality, data = dataRed, xlab = "Quality", ylab = "Volatile Acidity")
boxplot(citric.acid~quality, data = dataRed, xlab = "Quality", ylab = "Citric Acid")
boxplot(residual.sugar~quality, data = dataRed, xlab = "Quality", ylab = "Residual Sugar")
boxplot(chlorides~quality, data = dataRed, xlab = "Quality", ylab = "Chlorides")
boxplot(free.sulfur.dioxide~quality, data = dataRed, xlab = "Quality", ylab = "Free Sulfur Dioxide")
boxplot(total.sulfur.dioxide~quality, data = dataRed, xlab = "Quality", ylab = "Total Sulfur Dioxide")
boxplot(density~quality, data = dataRed, xlab = "Quality", ylab = "Density")
boxplot(pH~quality, data = dataRed, xlab = "Quality", ylab = "pH")
boxplot(sulphates~quality, data = dataRed, xlab = "Quality", ylab = "Sulphates")
boxplot(alcohol~quality, data = dataRed, xlab = "Quality", ylab = "Alcohol")


par(mfrow = c(1,1))

mod <- lm(quality ~ ., data = dataRed)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm = T),
                   names(cooksd), ""), col = "red")

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm = T))])
head(dataRed[influential, ])


car::outlierTest(mod)

## Histograms
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(dataRed$fixed.acidity, h=0.5, col = "slategray3", main="Fixed Acidity")
truehist(dataRed$volatile.acidity, h=0.5, col = "slategray3", main="Volatile Acidity")
truehist(dataRed$citric.acid, h=0.5, col = "slategray3", main="Citric Acid")
truehist(dataRed$residual.sugar, h=0.5, col = "slategray3", main="Residual Sugar")
truehist(dataRed$chlorides, h=0.5, col = "slategray3", main="Chlorides")
truehist(dataRed$free.sulfur.dioxide, h=0.5, col = "slategray3", main="Free Sulfur Dioxide")
truehist(dataRed$total.sulfur.dioxide, h=0.5, col = "slategray3", main="Total Sulfur Dioxide")
truehist(dataRed$density, h=0.5, col = "slategray3", main="Density")
truehist(dataRed$pH, h=0.5, col = "slategray3", main="pH")
truehist(dataRed$sulphates, h=0.5, col = "slategray3", main="Sulphates")
truehist(dataRed$alcohol, h=0.5, col = "slategray3", main="Sulphates")


##########################################################################
## REMOVE OUTLIERS
#########################################################################

## A predictor value is considered to be an outlier only if it is greater then
## Q3 + 1.5IQR
## The rationale behind this rule is that the extreme outliers are all on the 
## higher end of the values and the distributions are all positively skewed.
## Application of this rule reduces the data size from 1599 to x
limout <- rep(0,11)
for (i in 1:11) {
  t1 <- quantile(dataRed[, i], 0.75)
  t2 <- IQR(dataRed[, i], 0.75)
  limout[i] <- t1 + 1.5*t2
}

dataRedIndex <- matrix(0, 1599, 11)
for (i in 1:1599) {
  for (j in 1:11) {
    if (dataRed[i, j] > limout[j]) dataRedIndex[i,j] <- 1
  }
}

dRInd <- apply(dataRedIndex, 1, sum)
dataRedTemp <- cbind(dRInd, dataRed)
Indexes <- rep(0, 208)

j <- 1
for (i in 1:1599){
  if (dRInd[i] > 0) {Indexes[j]<- i
  j <- j + 1}
  else j <- j
}

dataRedLib <- dataRed[-Indexes,]


########################################
## REVIEW OUTLIERS REMOVED DATASET
#######################################

summary(dataRedLib)

str(dataRedLib)

## Distribution data by QUality
ggplot(data = dataRedLib, mapping = aes(x = quality)) +
  geom_bar()


## Relation of physicochemical characteristics vs quality
par(mfrow = c(2,2))

boxplot(fixed.acidity~quality, data = dataRedLib, main = "Fixed Acidity by Quality")
boxplot(volatile.acidity~quality, data = dataRedLib, main = "Volatile Acidity by Quality")
boxplot(citric.acid~quality, data = dataRedLib, main = "Citric Acid by Quality")
boxplot(residual.sugar~quality, data = dataRedLib, main = "Residual Sugar by Quality")
boxplot(chlorides~quality, data = dataRedLib, main = "Chlorides by Quality")
boxplot(free.sulfur.dioxide~quality, data = dataRedLib, main = "Free Sulfur Dioxide by Quality")
boxplot(total.sulfur.dioxide~quality, data = dataRedLib, main = "Total Sulfur Dioxide by Quality")
boxplot(density~quality, data = dataRedLib, main = "Density by Quality")
boxplot(pH~quality, data = dataRedLib, main = "pH by Quality")
boxplot(sulphates~quality, data = dataRedLib, main = "Sulphates by Quality")
boxplot(alcohol~quality, data = dataRedLib, main = "Alcohol by Quality")


par(mfrow = c(1,1))

mod <- lm(quality ~ ., data = dataRedLib)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm = T),
                   names(cooksd), ""), col = "red")

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm = T))])
head(dataRedLib[influential, ])


car::outlierTest(mod)

## Histograms
par(mfrow=c(2,2), oma = c(1,1,0,0) + 0.1, mar = c(3,3,1,1) + 0.1)

truehist(dataRedLib$fixed.acidity, h=0.5, col = "slategray3", main="Fixed Acidity")
truehist(dataRedLib$volatile.acidity, h=0.5, col = "slategray3", main="Volatile Acidity")
truehist(dataRedLib$citric.acid, h=0.5, col = "slategray3", main="Citric Acid")
truehist(dataRedLib$residual.sugar, h=0.5, col = "slategray3", main="Residual Sugar")
truehist(dataRedLib$chlorides, h=0.5, col = "slategray3", main="Chlorides")
truehist(dataRedLib$free.sulfur.dioxide, h=0.5, col = "slategray3", main="Free Sulfur Dioxide")
truehist(dataRedLib$total.sulfur.dioxide, h=0.5, col = "slategray3", main="Total Sulfur Dioxide")
truehist(dataRedLib$density, h=0.5, col = "slategray3", main="Density")
truehist(dataRedLib$pH, h=0.5, col = "slategray3", main="pH")
truehist(dataRedLib$sulphates, h=0.5, col = "slategray3", main="Sulphates")
truehist(dataRedLib$alcohol, h=0.5, col = "slategray3", main="Sulphates")

#########################################


describe(dataRedLib)
cor(dataRedLib[, -12])
cor(dataRedLib[, -12], method = "spearman")
pairs(dataRedLib[, -12], gap=0, pch=19, cex=0.4, col="darkblue")
title(sub="Scatterplot of Chemical Attributes", cex=0.8)

# Correlation
par(mfrow = c(1,1))
corrplot(cor(dataRedLib), method = "ellipse")





############ Linear Regression #####################

### MODEL 1: FULL
red_lm_full <- lm(quality ~ .-quality,
                  data = dataRedLib, na.action = na.omit)
summary(red_lm_full)

# Evaluate Collinearity
sqrt(vif(red_lm_full)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(red_lm_full) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(red_lm_full) # positive autocorrelation 

gvmodel=gvlma(red_lm_full) 
summary(gvmodel)


### MODEL 2: ALL WITH *
red_lm_2 <- lm(quality ~ volatile.acidity + total.sulfur.dioxide + pH +
                    sulphates + alcohol,
                  data = dataRedLib, na.action = na.omit)
summary(red_lm_2)

# Evaluate Collinearity
sqrt(vif(red_lm_2)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(red_lm_2) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(red_lm_2) # positive autocorrelation 

gvmodel=gvlma(red_lm_2) 
summary(gvmodel)


### MODEL 3: ALL WITH ***
red_lm_3 <- lm(quality ~ volatile.acidity + sulphates + alcohol,
               data = dataRedLib, na.action = na.omit)
summary(red_lm_3)

# Evaluate Collinearity
sqrt(vif(red_lm_3)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(red_lm_3) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(red_lm_3) # positive autocorrelation 

gvmodel=gvlma(red_lm_3) 
summary(gvmodel)


### MODEL 4: Step-wise - Forward
red_lm_4 <- stepAIC(red_lm_full, direction = "forward")
summary(red_lm_4)

# Evaluate Collinearity
sqrt(vif(red_lm_4)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(red_lm_4) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(red_lm_4) # positive autocorrelation 

gvmodel=gvlma(red_lm_4) 
summary(gvmodel)


### MODEL 5: Step-wise - Backward
red_lm_5 <- stepAIC(red_lm_full, direction = "backward")
summary(red_lm_5)

# Evaluate Collinearity
sqrt(vif(red_lm_5)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(red_lm_5) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(red_lm_5) # positive autocorrelation 

gvmodel=gvlma(red_lm_5) 
summary(gvmodel)


### MODEL 6: Step-wise - Both
red_lm_6 <- stepAIC(red_lm_full, direction = "both")
summary(red_lm_6)

# Evaluate Collinearity
sqrt(vif(red_lm_6)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(red_lm_6) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(red_lm_6) # positive autocorrelation 

gvmodel=gvlma(red_lm_6) 
summary(gvmodel)



### MODEL 7: quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + sulphates + alcohol
red_lm_7 <- lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + sulphates + alcohol,
                  data = dataRedLib)
summary(red_lm_7)

# Evaluate Collinearity
sqrt(vif(red_lm_7)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(red_lm_7) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(red_lm_7) # positive autocorrelation 

gvmodel=gvlma(red_lm_7) 
summary(gvmodel)



### MODEL 8: quality ~ volatile_acidity + sulphates + alcohol , data=wine
red_lm_8 <- lm(quality ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol,
               data = dataRedLib)
summary(red_lm_8)

# Evaluate Collinearity
sqrt(vif(red_lm_8)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(red_lm_8) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(red_lm_8) # positive autocorrelation 

gvmodel=gvlma(red_lm_8) 
summary(gvmodel)




newdata <- dataRed[1:2,1:11]

predict(red_lm_8, newdata, interval = "confidence")
predict(red_lm_7, newdata, interval = "confidence")
predict(red_lm_6, newdata, interval = "confidence")
predict(red_lm_5, newdata, interval = "confidence")
predict(red_lm_4, newdata, interval = "confidence")
predict(red_lm_3, newdata, interval = "confidence")
predict(red_lm_2, newdata, interval = "confidence")


