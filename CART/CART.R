#Assignment for the 3rd 
#voting 

gerber <- read.csv("gerber.csv")
str(gerber)
table(gerber$voting)

108696/nrow(gerber)

table(gerber$neighbors,gerber$voting)
tapply(gerber$voting, gerber$civicduty, mean)
#gerber -> 12021
#hawthorne -> 12316
#self-> 13191
#neighbors ->14438


#logistic regression
gerberlm <- glm( voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family=binomial)
summary(gerberlm)

#accurracy with threshold 0.3

predict = predict(gerberlm, type="response")

#Confusion matrix with threshold of 0.3
table(gerber$voting,predict > 0.5)

(133413 + 51966)/(133413 + 51966 + 100875 + 56730)

235388/(235388+108696)

library(ROCR)

ROCRpred = prediction(predict, gerber$voting)

as.numeric(performance(ROCRpred, "auc")@y.values)


#Trees ########################

#build regression tree without using class
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)


CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
CARTmodel5 = rpart(voting ~ control + sex, data = gerber, cp=0.0)
prp(CARTmodel5, digits=6)

abs(0.34-0.296638)


#logistic regression again
LogModelSex <- glm (voting ~ sex + control, data = gerber, family = binomial)
summary(LogModelSex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")


#Assignment2-letters#########################

letters <- read.csv("letters_ABPR.csv")
str(letters)
letters$isB <- as.factor(letters$letter=="B")

set.seed(1000)

library(caTools)
split<-sample.split(letters$isB, SplitRatio = 0.5)

lettersTrain <- subset(letters, split==TRUE)
lettersTest <- subset(letters, split==FALSE)

table(lettersTest$isB)

#building class tree to predct whether letter is a B ot not
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=lettersTrain, method="class")
prp(CARTb)

pred<- predict(CARTb, newdata=lettersTest, type="class")

table(lettersTest$isB, pred)
#        FALSE TRUE
# FALSE  1118   57
# TRUE     43  340

#accuracy
(1118+340)/nrow(lettersTest)
0.9358151

#Random forest
library(randomForest)
set.seed(1000)
lettersForest <- randomForest(isB ~ . - letter, data=lettersTrain)

predForest <- predict(lettersForest,newdata=lettersTest, type="class")

table(lettersTest$isB,predForest)
(1165+374)/nrow(lettersTest)

#multiclass classification
letters$letter <- as.factor(letters$letter)

set.seed(2000)
spl <- sample.split(letters$letter, SplitRatio = 0.5)

letTrain <- subset(letters, spl==T)
letTest <- subset(letters, spl==F)

table(letTest$letter)

CARTc <- rpart(letter ~ . - isB, data = letTrain, method="class")

predb <- predict(CARTc, newdata = letTest, type = "class")

table(letTest$letter, predb)
#     A   B   P   R
# A 348   4   0  43
# B   8 318  12  45
# P   2  21 363  15
# R  10  24   5 340

(348+318+363+340)/nrow(letTest)

set.seed(1000)
letForest <- randomForest( letter ~ . - isB, data=letTrain)
predFor <- predict(letForest, newdata = letTest, type = "class")

table(letTest$letter, predFor)

(390+380+393+364)/nrow(letTest)


#Assignment 4 census data#################

census <- read.csv("census.csv")

set.seed(2000)

split <- sample.split(census$over50k, SplitRatio = 0.6)

censusTrain <- subset(census, split==T)
censusTest <- subset(census, split==F)
str(censusTrain)

logModel <- glm(over50k ~ ., data = censusTrain, family=binomial)
summary(logModel)

pred <- predict(logModel, newdata = censusTest, type = "response")

table(censusTest$over50k,pred > 0.5)
(9051+1888)/nrow(censusTest)

(9051+662)/nrow(censusTest)

library(ROCR)
ROCRpred = prediction(pred, censusTest$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

library(rpart)
library(rpart.plot)

censusTree <- rpart(over50k ~ ., data=censusTrain, method="class")

prp(censusTree)
predTree <- predict(censusTree, newdata = censusTest, type="class")

table(censusTest$over50k, predTree)
(9243+1596)/nrow(censusTest)

PredictROC = predict(censusTree, newdata = censusTest)
PredictROC

pred = prediction(PredictROC[,2], censusTest$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

set.seed(1)

trainSmall = censusTrain[sample(nrow(censusTrain), 2000), ]

library(randomForest)
forest <- randomForest(over50k ~ ., data = trainSmall)

forestPred <- predict(forest, newdata = censusTest)

table(censusTest$over50k, forestPred)

(9641+851)/nrow(censusTest)

#to see which variables are used maximum number of splits in the forest

vu = varUsed(forest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))
varImpPlot(forest)

library(caret)
library(e1071)

# Define cross-validation experiment
set.seed(2)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

train(over50k ~ ., data = censusTrain, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

censusTreeCV <- rpart(over50k ~ ., data=censusTrain,cp=0.002)

predictCV <- predict(censusTreeCV, newdata=censusTest,type="class")
  
table(censusTest$over50k,predictCV)
(9178+1838)/nrow(censusTest)

prp(censusTreeCV)




