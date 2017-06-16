# install.packages("RCurl")
# install.packages("knitr")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("corrplot")
# install.packages("kernlab")
# install.packages("randomForest")
# install.packages("doParallel")
# install.packages("gbm")
# install.packages("survival")
# install.packages("splines")
# install.packages("xtable")
# install.packages("stargazer")
# install.packages("stringr")
# install.packages("reshape2")
# install.packages("plyr")
# install.packages("adabag")
# install.packages("rpart")
# 
# install.packages("e1071")# I dunno what is this...


library(RCurl)
library(knitr)
library(caret)
library(ggplot2)
library(corrplot)
library(kernlab)
library(randomForest)
library(doParallel)
library(gbm)
library(survival)
library(splines)
library(xtable)
library(stargazer)
library(stringr)
library(reshape2)
library(plyr)
library(adabag)
library(rpart)


# Downloading Training Data
##if(!file.exists("./data")){dir.create("./data")}

#trainDataURL <- "https://inclass.kaggle.com/c/deloitte-tackles-titanic/download/train.csv"
##trainDataURL <- "https://inclass.kaggle.com/c/deloitte-tackles-titanic/download/train2.csv"
##trainFile <- paste("./data/", basename(trainDataURL), sep = "")

#download.file(trainDataURL, trainFile, method = "curl")
# Downloading Testing Data
#if(!file.exists("./data")){dir.create("./data")}
##testDataURL <- "https://inclass.kaggle.com/c/deloitte-tackles-titanic/download/test.csv"
##testFile <- paste("./data/", basename(testDataURL), sep = "")
# Need username and password; do not use this link
#download.file(testDataURL, testFile, method = "curl")

trainData <- read.csv("C:/Users/Vojta/Desktop/kaggle challenge/train.csv")
testData <- read.csv("C:/Users/Vojta/Desktop/kaggle challenge/test.csv")



trainData$Type <- "Training"
testData$Type <- "Testing"

# $Survived initialization
testData$Survived <- NA
combi <- rbind(trainData, testData)


#Factor conversion
combi$Survived <- as.factor(combi$Survived)
combi$Pclass <- as.factor(combi$Pclass)


################################################################################
#TITLE ANALYSIS

combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]') [[1]][2]})

#Cleaning white spaces
combi$Title <- sapply(combi$Title, FUN = function(x){ final <- "_" 
                                                      if(str_sub(x,end = -nchar(x))== " "){ 
                                                        final <- str_sub(x,start = -nchar(x)+1) }
                                                      final })


#table(combi$Title)
#table(combi$Title, combi$Sex)

#########################################################################



#Grouping titles
titleCleaner <- function(title, sex){
  
  finalTitle = title
  menTitles <- c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir")
  womenTitles <- c("Dona", "Lady", "Mme", "the Countess")
  girlTitles <- c("Mlle", "Mme")
  if(title %in% menTitles ){
    finalTitle = "Mr"
  }
  if(title %in% womenTitles){
    finalTitle = "Mrs"
  }
  if(title %in% girlTitles){
    finalTitle = "Ms"
  }
  if(title == "Dr"){
    if(sex == "male"){
      finalTitle = "Mr"
    }else{
      finalTitle = "Mrs"
    }
  }
  finalTitle
}





combi$TitleCombined <- mapply(titleCleaner, combi$Title, combi$Sex)
#table(combi$Title, combi$TitleCombined)
combi$TitleCombined <- as.factor(combi$TitleCombined)



#FAMILY SIZE ANALYSIS
##############################################################################3
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split ='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
#combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
combi$FamilyID <- as.factor(combi$FamilyID)
combi$SmallFamily <- ifelse(combi$FamilySize <=2, 1, 0)


###############################################################################
#PARENT ANALYSIS

#isParent
isParent <- function(FamilySize, Parch, SibSp, TitleCombined){
  isParent <- "No"
  if(FamilySize > 1){
    if(Parch > 1){
      #if(SibSp > 1){
      if(TitleCombined == "Mr"){
        isParent <- "Father"
      }
      if(TitleCombined == "Mrs"){
        isParent <- "Mother"
      }
      #}
    }
  }
  isParent
}

combi$isParent <- mapply(isParent, FamilySize = combi$FamilySize, Parch = combi$Parch, SibSp = combi$SibSp, TitleCombined = combi$TitleCombined)
combi$isParent <- as.factor(combi$isParent)




#################################################################################
#CABIN ANALYSIS

combi$Cabin <- as.character(combi$Cabin)
combi$CabinLetter <- colsplit(combi$Cabin,"",names = c("a", "b", "c"))[,2]
combi[which(combi$CabinLetter==""), grep("CabinLetter", colnames(combi))] <-  "Unknown"
combi$CabinLetter <- as.factor(combi$CabinLetter)


##########################################################################
#NUMBER OF CABIN ANALYSIS

combi$NumberOfCabins <- lapply(strsplit(combi$Cabin, split='[ ]'),length)
combi$NumberOfCabins <- as.numeric(combi$NumberOfCabins)
combi <- combi[,-which( colnames(combi)=="Cabin" )]


##############################################################################
# FARE ANALYSIS

combi$FarePerPerson <- mapply(x=combi$Fare, y=combi$FamilySize, FUN = function(x,y){
                                if(is.na(x)){x<-0};
                                if(is.na(y)){y<-1};
                                x/y
                              })

combi$TotalFare <- sapply(combi$Fare, FUN = function(x){
  if(is.na(x)){x<-0};
  x
})

combi$FareDifference <- combi$TotalFare - combi$FarePerPerson
combi$FareDifference_scaled <- scale(combi$FareDifference)
combi$FareDifference_scaled_centered <- scale(combi$FareDifference, center = TRUE,
                                              scale = max(combi$FareDifference, na.rm = TRUE)/ 100)
combi$Dependency <- as.factor(ifelse(combi$FareDifference==0, 1,0))


####################################################################################
# FARE BIN

fareBreaks <- c(-1,7.75, 8, 13, 14.50, 15.10, 26,30.65,60,Inf)
combi$FareBin <- cut(combi$FarePerPerson,fareBreaks)


#######################################################################33
# AGE ANALYSIS

#combi$Age <- round(combi$Age,0)

#linear fitting model
AgeModel <- lm(Age ~ TitleCombined+Sex, data = combi[-which(is.na(combi$Age)),])
combi$Age2 <- predict(AgeModel, combi)

## Warning: prediction from a rank-deficient fit may be misleading
combi$Age2 <- ifelse(is.na(combi$Age), combi$Age2, combi$Age )
ageBreaks <- c(0,5,10,15,20,30,40,50,60,70,80,90,Inf)
#ageBreaks <- c(0,5,10,15,20,30,40,Inf)
combi$AgeBin <- cut(combi$Age2,ageBreaks)


####################################################################
# TITLE ANALYSIS

isDeadlyMr <- function(TitleCombined, Parch, FareDifference){
  DeadlyMr <- 0
  if(TitleCombined=="Mr"){
    if(Parch <=1){
      if(FareDifference <=0){
        DeadlyMr <- 1
      }
    }
  }
  DeadlyMr
}
#combi$MrAnalysis <- mapply(isDeadlyMr, TitleCombined=combi$TitleCombined, Parch=combi$Parch, FareDifference=combi$FareDifference)
#combi$MrAnalysis <- as.factor(combi$MrAnalysis)

#write.table(combi, file = "C:/Users/Vojta/Desktop/K3/combi.csv", sep=",",row.names = FALSE)


##########################################################################

##########################################################################
# PRE PROCESSING
##########################################################################

#Removing Blank Embarked data
combi <- combi[-which(combi$Embarked == ""),]

#Dropping the column
combi <- combi[,-which( colnames(combi)=="Title" )]
combi <- combi[,-which( colnames(combi)=="Name" )]
combi <- combi[,-which( colnames(combi)=="Ticket" )]
combi <- combi[,-which( colnames(combi)=="Fare" )] #Removed as there was 1 missing value
#combi <- combi[,-which( colnames(combi)=="FarePerPerson" )]
#combi <- combi[,-which( colnames(combi)=="SibSp" )]
#combi <- combi[,-which( colnames(combi)=="Parch" )]
combi <- combi[,-which( colnames(combi)=="FamilyID" )]
combi <- combi[,-which( colnames(combi)=="Surname" )]
combi <- combi[,-which( colnames(combi)=="Age")]
combi <- combi[,-which( colnames(combi)=="Age2")]


####################################################################
# SPLITTING TEST AND TRAIN DATA

testingData <- combi[which(combi$Type=="Testing"),]
trainingData <- combi[which(combi$Type=="Training"),]
#Removing Type column
testingData <- testingData[,-which( colnames(combi)=="Type" )]
trainingData <- trainingData[,-which( colnames(combi)=="Type" )]


##################################################################
# BREAKING TO CROSS VALIDATION

set.seed(112)
inTest <- createDataPartition(y=trainingData$Survived, p=0.8, list=FALSE)
training <- trainingData[inTest,]
crossVal <- trainingData[-inTest,]


rm(trainData)
rm(testData)
rm(trainingData)
#rm(combi)


#############################################################################
#############################################################################
#############################################################################
#############################################################################
# MODEL BUILDING 
#############################################################################

#rm(trainData)


cl <- makeCluster(detectCores())
registerDoParallel(cl)
# Random Forest

set.seed(112)
training_Mr <- training[which(training$TitleCombined == "Mr"),]
training_P3 <- training[which(training$Pclass == 3),]
training_P1 <- training[which(training$Pclass == 1),]
training_FareDiff <- training[which(training$FareDifference >= 14.5),]
training_FarePerson <- training[which(training$FarePerPerson <= 20),]

modelFit_rf <- train(Survived ~., data=training[,-1], method="rf", prox=TRUE, trControl = trainControl(method="cv"),number=5)
modelFit_rf_Mr <- train(Survived ~., data=training_Mr[,-1], method="rf",prox=TRUE, trControl = trainControl(method="cv"),number=5)
modelFit_rf_P3 <- train(Survived ~., data=training_P3[,-1], method="rf",prox=TRUE, trControl = trainControl(method="cv"),number=5)

modelFit_rf_FareDiff <- train(Survived ~., data=training_FareDiff[,-1],prox=TRUE, method="rf", trControl = trainControl(method="cv"),number=5)
modelFit_rf_P1 <- train(Survived ~., data=training_P1[,-1], method="rf",prox=TRUE, trControl = trainControl(method="cv"),number=5)
modelFit_rf_FarePerson <- train(Survived ~., data=training_FarePerson[,-1], method="rf", trControl = trainControl(method="cv"),number=5)


###################################################################################
# PREDICTING THE RESULTS

testingData$Survived<- predict(modelFit_rf, newdata=testingData[,-1])
testingData[which(testingData$TitleCombined == "Mr"),2] <- predict(modelFit_rf_Mr, newdata=testingData[which(testingData$TitleCombined == "Mr"),-1])
testingData[which(testingData$Pclass == 3),2] <- predict(modelFit_rf_P3, newdata=testingData[which(testingData$Pclass == 3),-1])
testingData[which(testingData$FareDifference >= 14.5),2] <- predict(modelFit_rf_FareDiff, newdata=testingData[which(testingData$FareDifference >= 14.5),-1])
testingData[which(testingData$Pclass == 1),2] <- predict(modelFit_rf_P1, newdata=testingData[which(testingData$Pclass == 1),-1])##Put at end
testingData[which(testingData$FarePerPerson <= 20),2] <- predict(modelFit_rf_FarePerson, newdata=testingData[which(testingData$FarePerPerson <= 20),-1])


##################################################################################3
# IDENTIFICATION TAB FOR DEBUGGING

testingData$Tag <- ""
Tag <- grep("Tag", colnames(testingData))
##Identification
testingData[which(testingData$TitleCombined == "Mr"), Tag] <- "Mr"
testingData[which(testingData$Pclass == 3), Tag] <- "P3"
testingData[which(testingData$Pclass == 1), Tag] <- "P1"
testingData[which(testingData$FareDifference >= 14.5), Tag] <- "FareDiff"
testingData[which(testingData$FarePerPerson <= 20), Tag] <- "FarePerson"

#################################################################################
# SUBMISSION PREPARATION

finalResults <- testingData[,1:2]
write.table(finalResults, file = "C:/Users/Vojta/Desktop/Ktest/finalResults.csv", sep = ",", row.names = FALSE)

#################################################################################
# POST ANALYSIS

plot(varImp(modelFit_rf, scale = TRUE))

plot(varImp(modelFit_rf_Mr, scale = TRUE))

plot(varImp(modelFit_rf_P3, scale = TRUE))

plot(varImp(modelFit_rf_FareDiff, scale = TRUE))

plot(varImp(modelFit_rf_P1, scale = TRUE))


########################################################################
# WRITING OUT RESULTS FOR ANALYSIS

crossVal$Predicted2 <- predict(modelFit_rf, crossVal)
write.table(crossVal, file = "C:/Users/Vojta/Desktop/Ktest/crossVal.csv", sep=",",row.names = FALSE)
write.table(testingData, file = "C:/Users/Vojta/Desktop/Ktest/testing.csv", sep=",",row.names = FALSE)

