#Movies##################################
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
library(e1071)


#read movies data
movies<- read.csv("Movies.csv")
str(movies)

#split into train and test set
movieTrain <- subset(movies, Year < 2010)
movieTest <- subset(movies, Year >= 2010)

#build a linear reg model
movieslm <- lm(Worldwide ~ ., movieTrain[,3:ncol(movieTrain)])
summary(movieslm)

#find correlation b/w Worldwide and production budget
cor(movieTrain$Worldwide,movieTrain$Production.Budget)

#recreating model with significant variables
movieslm <- lm(Worldwide ~ Runtime + Crime + Horror + Animation + History + Nominations + Production.Budget, movieTrain)
summary(movieslm)

#Making predictions on test set
moviePred <- predict(movieslm, newdata= movieTest)

#calculate SSE, SST and Rsquared
movieSSE <- sum((movieTest$Worldwide-moviePred)^2)
movieSSE

movieSST <- sum((movieTest$Worldwide-mean(moviePred))^2)
movieSST

movieRsquared <- 1 - movieSSE/movieSST

#classification problem
movies$Performance = factor(ifelse(movies$Worldwide > quantile(movies$Worldwide, .75), "Excellent", ifelse(movies$Worldwide > quantile(movies$Worldwide, .25), "Average", "Poor")))
table(movies$Performance)
movies$Worldwide <- NULL

#split into train and test set
movieTrain <- subset(movies, Year < 2010)
movieTest <- subset(movies, Year >= 2010)

movieCART <- rpart(Performance ~ ., movieTrain[,3:ncol(movieTrain)])
prp(movieCART)

predMovieCart <- predict(movieCART,newdata = movieTrain, type="class")
table(movieTrain$Performance,predMovieCart)
(107+39+41)/nrow(movieTrain)
(28+10+18)/(28+4+6+13+10+1+6+18)
(107)/(107+20+19)

#predict on test set
#accuracy
#baseline accuracy

#Conclusion
#both linear and CART models are best for prediction in this case

#Interest rate hikes#################################
fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors = F)
table(fedFunds$RaisedFedFunds)
294/(291+294)

fedFunds$Chairman <- as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres <- as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds <- as.factor(fedFunds$RaisedFedFunds)

set.seed(201)
spl <- sample.split(fedFunds$RaisedFedFunds, 0.7)

ffTrain <- subset(fedFunds, spl == T)
ffTest <- subset(fedFunds, spl == F)

#logreg model
fflm <- glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = ffTrain, family = "binomial")
summary(fflm)

#The observation has PreviousRate=1.7, Streak=-3, Unemployment=5.1, DemocraticPres=0, MonthsUntilElection=18. Therefore, the prediction has logistic function value 9.121012 + 1.7*(-0.003427) - 3* 0.157658 + 5.1*(-0.047449) + 65.3*(-0.136451) + 0*0.347829 + 18*(-0.006931) = -0.6347861. Then you need to plug this into the logistic response function to get the predicted probabilit
9.121012 + 1.7*(-0.003427) - 3* 0.157658 + 5.1*(-0.047449) + 65.3*(-0.136451) + 0*0.347829 + 18*(-0.006931)

#The coefficients of the model are the log odds associated with that variable; so we see that the odds of being sold are exp(0.347829)=1.41599 those of an otherwise identical month. This means the month is predicted to have 41.6% higher odds of being sold.

ffPredictTest <- predict(fflm,newdata = ffTest, type="response")
summary(ffPredictTest)

table(ffTest$RaisedFedFunds, ffPredictTest > 0.5)
# FALSE TRUE
# 0    60   27
# 1    31   57

#baseline accuracy
(31+57)/(60+27+31+57)

#test set accuracy
(60+57)/(60+27+31+57)

ROCRffpredTest <- prediction(ffPredictTest, ffTest$RaisedFedFunds)
AUCff <- as.numeric(performance(ROCRffpredTest,"auc")@y.values)
AUCff

# Performance function
ROCRperf = performance(ROCRffpredTest, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

#Define cross validation experiment
set.seed(201)
numFolds <- trainControl(method = "cv",number = 10)
cpGrid <- expand.grid(.cp = seq(0.001,0.05,0.001))

#perform cross validation
train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = ffTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

#CART model
ffTreeCV <- rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = ffTrain, method = "class", cp = 0.016)
summary(ffTreeCV)
prp(ffTreeCV)

#predict on test set
predCARTCV <- predict(ffTreeCV, newdata = ffTest, type="class")
table(ffTest$RaisedFedFunds,predCARTCV)
predCARTCV
# 0  1
# 0 64 23
# 1 40 48

#compute accuracy
(64+48)/(64+23+40+48)


#Households data ##############################################################
Households <- read.csv("Households.csv")
str(Households)
table(Households$AfternoonPct == 100)

#min(Households["AvgSalesValue" > 150,"AvgDiscount"])
summary(Households$AvgDiscount)

Households[Households$AvgSalesValue>150,]

Households[Households$AvgDiscount>25,]

table(Households$NumVisits >= 300)
148/(148+2352)

distances <- dist(Households, method = "euclidean")
summary(distances)

#normalize values
preproc = preProcess(Households)

HouseholdsNorm = predict(preproc, Households)

summary(HouseholdsNorm$AfternoonPct)

#Create a dendrogram
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)


#kmeans clustering
set.seed(5000)
clusterKmeans <- kmeans(HouseholdsNorm,centers = 5)
str(clusterKmeans)

clusters <- split(HouseholdsNorm,clusterKmeans$cluster)
str(clusters)

table(clusterKmeans$cluster)
summary(clusters[[1]])

sapply(clusters,function(x) mean(x$MorningPct))

clusterKmeans$centers




