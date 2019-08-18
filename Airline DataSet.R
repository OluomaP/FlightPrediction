getwd()

#load the data into data frame with coulms and rows
origData<-read.csv2('.\\airline.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE)
rm(list = ls())
#we specify the file path, separator, whether the csv file's 1st row is column

nrow(origData)
ncol(origData)

#that is a lot of rows to process so to speed things up lets restrict data to only flight between certain large airports
airports<- c('ATL','LAX', 'ORD', 'DFW', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')

origData<- subset(origData, DEST %in% airports & ORIGIN %in% airports)
nrow(origData)

#inspecting and cleaning Data

#lets visually inspect data to find obvious issues
head(origData,2)
tail(origData,2)

origData$X <- NULL #the x column is gone
#in general we wanmt to eliminate any colum that we do not need

head(origData,10)
cor(origData[c("ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_AIRPORT_ID")])#if they are equal to 1 then they are perfectly correlated and one should be removed
cor(origData[c("DEST_AIRPORT_ID","DEST_AIRPORT_SEQ_ID")])
cor(origData[c("OP_UNIQUE_CARRIER","OP_CARRIER")])#it can only be done with data

origData$ORIGIN_AIRPORT_ID<- NULL
origData$DEST_AIRPORT_SEQ_ID<- NULL

mismatched<- origData[origData$OP_CARRIER != origData$OP_UNIQUE_CARRIER,]
nrow(mismatched)

origData$OP_UNIQUE_CARRIER<- NULL

onTimeData <- origData[!is.na(origData$ARR_DEL15) & origData$ARR_DEL15 !="" & !is.na(origData$DEP_DEL15) & origData$DEP_DEL15 !="",]
nrow(origData)
nrow(onTimeData)

onTimeData$DISTANCE<- as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED<- as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED<- as.integer(onTimeData$CANCELLED)

head(onTimeData,10)

onTimeData$ARR_DEL15<- as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15<- as.factor(onTimeData$DEP_DEL15)
onTimeData$DEST_AIRPORT_ID<- as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_SEQ_ID<- as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$DAY_OF_WEEK<- as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DEST<-as.factor(onTimeData$DEST)
onTimeData$ORIGIN<-as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK<- as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$OP_CARRIER<- as.factor(onTimeData$ OP_CARRIER)

tapply(onTimeData$ARR_DEL15, onTimeData$ARR_DEL15,length)
tapply(onTimeData$DEP_DEL15, onTimeData$DEP_DEL15,length)
onTimeData

#Training the Algorithm

install.packages('caret')
library(caret)
set.seed(122515) #set random number seed for reproducability

#set the columns we are going to use train algorithm
featurecols<- c("ARR_DEL15", "DAY_OF_WEEK","OP_CARRIER", "DEST", "ORIGIN", "DEP_TIME_BLK")
onTimeDataFiltered<- onTimeData[,featurecols]

inTrainRows<- createDataPartition(onTimeDataFiltered$ARR_DEL15, p=0.70, list = FALSE)

head(inTrainRows)

trainDataFiltered<- onTimeDataFiltered[inTrainRows,]
testDataFiltered<- onTimeDataFiltered[-inTrainRows,]

nrow(trainDataFiltered)/(nrow(testDataFiltered)+ nrow(trainDataFiltered))
nrow(testDataFiltered)/(nrow(testDataFiltered)+ nrow(trainDataFiltered))

install.packages('e1071', dependencies = TRUE)
library(e1071)

logisticRegModel <- train(ARR_DEL15 ~ ., data=trainDataFiltered, method="glm", family="binomial",
                          trControl=trainControl(method="repeatedcv", number=10, repeats=10))
 ;p# Output model

logRegPrediction <- predict(logisticRegModel, testDataFiltered)
logRegPrediction

logRegConfMat <- confusionMatrix(logRegPrediction, testDataFiltered[,"ARR_DEL15"])
logRegConfMat

summary(logRegPrediction)
my_solution <- data.frame(Carrier = testDataFiltered$OP_CARRIER, Arrival_Delay = logRegPrediction)
summary(my_solution)
write.csv(my_solution,file="my_solution.csv",row.names=FALSE)

install.packages('randomForest')

#  load the random forest library into the current session
library(randomForest)

# This code will run for a while!  It ran for 8 minutes on a system with a i7-4790K, 16 GB of memory, and a 500 GB SSD.
rfModel <- randomForest(trainDataFiltered[-1], trainDataFiltered$ARR_DEL15, proximity = TRUE, importance = TRUE)
rfModel

#   Random Forest
rfValidation <- predict(rfModel, testDataFiltered)
#    Get detailed statistics of prediction versus actual via Confusion Matrix 
rfConfMat <- confusionMatrix(rfValidation, testDataFiltered[,"ARR_DEL15"])
rfConfMat


