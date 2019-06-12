library(rattle)
library(plyr)
library(dplyr)  
library(mice)
library(VIM)    #KNN
library(corrplot)
library(caret)
library(ggplot2)
library(rpart.plot)
library(klaR) #classification
library(readr)
library(rpart)
library(Amelia)
library(reshape2)
library(Hmisc)
library(pROC)
library(sqldf)
library(cluster)

rm(list = ls())
mice = read.csv(file.choose(), header = TRUE, na.strings = c("NA","","#NA","?"))
str(mice)

#########################
####Feature Selection####
#########################

#mice$class = as.factor(mice$class) #convert Class feature into factor
mice.scale <- mice[,-1]
mice$Genotype <- as.integer(mice$Genotype)
mice$Treatment <- as.integer(mice$Treatment)
mice$Behavior <- as.integer(mice$Behavior)

mice$MouseID = NULL #remove MouseID as this does not predict anything
mice$Genotype = NULL
mice$Treatment= NULL
mice$Behavior = NULL

#missmap
aggr(mice,sortVars=T, combined=T,bars=F,numbers=F,prop=F,sortCombs=T)

#Plot missing values visually
missmap(mice, main="Mice Data - Missings Map",
        col=c("yellow", "black"), legend=FALSE)
#count missig values per column
sort(colSums(is.na(mice)),decreasing=T)[1:10]

#KNN - impute missing values
knnoutput = kNN(mice, k = 5)
summary (knnoutput)

sort(colSums(is.na(knnoutput)),decreasing=T)[1:10] #check for N/A

#only take original columns, exclude KNN TRUE/FALSE result columnds
mice.select=knnoutput[ ,c("DYRK1A_N","BDNF_N","NR1_N","NR2A_N","pAKT_N","pBRAF_N","pCAMKII_N","pCREB_N","pELK_N","pERK_N","pJNK_N","PKCA_N","pMEK_N","pNR1_N","pNR2A_N","pNR2B_N","pPKCAB_N","pRSK_N","AKT_N","BRAF_N","CAMKII_N","CREB_N","ELK_N","ERK_N","GSK3B_N","JNK_N","MEK_N","TRKA_N","RSK_N","APP_N","Bcatenin_N","SOD1_N","MTOR_N","P38_N","pMTOR_N","DSCR1_N","AMPKA_N","NR2B_N","pNUMB_N","RAPTOR_N","TIAM1_N","pP70S6_N","NUMB_N","P70S6_N","pGSK3B_N","pPKCG_N","CDK5_N","S6_N","ADARB1_N","AcetylH3K9_N","RRP1_N","BAX_N","ARC_N","ERBB4_N","nNOS_N","Tau_N","GFAP_N","GluR3_N","GluR4_N","IL1B_N","P3525_N","pCASP9_N","PSD95_N","SNCA_N","Ubiquitin_N","pGSK3B_Tyr216_N","SHH_N","BAD_N","BCL2_N","pCFOS_N","SYP_N","H3AcK18_N","EGR1_N","H3MeK4_N","CaNA_N","class")]

mice.select.top=knnoutput[ ,c("pGSK3B_N","class","BAX_N","ERBB4_N", "GFAP_N", "ARC_N", "pMEK_N")]




control = rfeControl(method = "repeatedcv", repeats = 5, verbose = TRUE, functions = lmFuncs) #define looping fucntions (5 iterations)
miceRFE = rfe(x = mice.select[,1:77], y = mice.select[,78], sizes = c(1:77), metrix = "RMSE", rfeControl = control)
print(miceRFE)
importance <- varImp(miceRFE, scale=FALSE)
print(importance)

#ITSN1_N.1 and pS6_N have no importance to our target
mice.imp = mice.select[ ,c("DYRK1A_N","BDNF_N","NR1_N","NR2A_N","pAKT_N","pBRAF_N","pCAMKII_N","pCREB_N","pELK_N","pERK_N","pJNK_N","PKCA_N","pMEK_N","pNR1_N","pNR2A_N","pNR2B_N","pPKCAB_N","pRSK_N","AKT_N","BRAF_N","CAMKII_N","CREB_N","ELK_N","ERK_N","GSK3B_N","JNK_N","MEK_N","TRKA_N","RSK_N","APP_N","Bcatenin_N","SOD1_N","MTOR_N","P38_N","pMTOR_N","DSCR1_N","AMPKA_N","NR2B_N","pNUMB_N","RAPTOR_N","TIAM1_N","pP70S6_N","NUMB_N","P70S6_N","pGSK3B_N","pPKCG_N","CDK5_N","S6_N","ADARB1_N","AcetylH3K9_N","RRP1_N","BAX_N","ARC_N","ERBB4_N","nNOS_N","Tau_N","GFAP_N","GluR3_N","GluR4_N","IL1B_N","P3525_N","pCASP9_N","PSD95_N","SNCA_N","Ubiquitin_N","pGSK3B_Tyr216_N","SHH_N","BAD_N","BCL2_N","pCFOS_N","SYP_N","H3AcK18_N","EGR1_N","H3MeK4_N","CaNA_N","class")]


#### HOW MANY CLUSTERS?! #####

#separate mush data into mush_select (only data with correlation to stalk.root per correlation matrix)
mice.target= mice.select$class
mice.select$class = NULL

mice.target.top= mice.select.top$class
mice.select.top$class = NULL


mice.stand <- scale(mice.select[-1])
mice.stand.top <- scale(mice.select.top[-1]) 
#How many K's?
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(mice.stand, nc=15) #visually, 3 clusters explains 80% of the observations (look for the elbow in the graph

#rejoin target variable with dataset.
mice.select=cbind(mice.select, mice.target)
mice.select.top=cbind(mice.select.top, mice.target.top)
#USE mice.select

#**********************IGNORE MODEL*****************************************

#**********************IGNORE MODEL*****************************************

#**********************IGNORE MODEL*****************************************

#**********************IGNORE MODEL*****************************************

#**********************IGNORE MODEL*****************************************

#**********************IGNORE MODEL*****************************************

#**********************IGNORE MODEL*****************************************


####################################
############ MODEL TIME ############
####################################

#******************K - means Cluster*****************
results = kmeans(mice.stand,8)
results

results$size

table(mice.select$class, results$cluster)

plot(mice.select[c("pGSK3B_N","class")], col = mice.select$Class)

clusplot(mice.stand, kclust$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Mice Protein Expression')


clusplot(mice.stand, mice.select$class, color=TRUE, shade=TRUE,
         labels=2, lines=0, main= 'Mice Protein Expression')

plot(kc)


attributes(kclust)

# Centroids:
kclust$centers

# Clusters:
kclust$cluster

# Cluster size:
kclust$size

table(mice[,1],kclust$cluster)


