library(ggplot2)
library(reshape2)
library(plyr)
library(car)
install.packages('rattle')
library(rattle)
library(gvlma)


wine <- read.csv(file.choose())
colnames(wine) <- c("fixed_acidity","volatile_acidity","citric_acid",
                    "residual_sugar","chlorides","free_sulfur_dioxide",
                    'total_sulfur_dioxide','density','pH','sulphates','alcohol','quality')

str(wine)

wine <- na.omit(wine)

pairs(wine)

attach(wine)

plot(density(quality))


Wine.fit=lm(quality ~ . -quality,data=wine)
summary(Wine.fit)

Wine.fit2=lm(quality ~ density + sulphates + alcohol, data=wine)
summary(Wine.fit2)

Wine.fit3=lm(quality ~ volatile_acidity + chlorides + total_sulfur_dioxide + sulphates + alcohol , data=wine)
summary(Wine.fit3)

Wine.fit4=lm(quality ~ volatile_acidity + sulphates + alcohol , data=wine)
summary(Wine.fit4)



newdata <- wine[1:2,1:11]
predict(Wine.fit4, newdata, interval = "confidence")


# Evaluate Collinearity
sqrt(vif(Wine.fit4)) #interpret VIF http://www.statisticshowto.com/variance-inflation-factor/

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(Wine.fit4) # the variance is unchanging in the residual

# Test for Autocorrelated Errors
durbinWatsonTest(Wine.fit4) # positive autocorrelation 

gvmodel=gvlma(Wine.fit4) 
summary(gvmodel)
