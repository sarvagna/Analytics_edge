#Assignment 1

wiki <- read.csv("wiki.csv",stringsAsFactors = F)

wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

str(wiki)

library(tm)
library(SnowballC)

# Create corpus

corpusAdded = Corpus(VectorSource(wiki$Added))

#remove stop words
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded[[1]]

#stem 
corpusAdded = tm_map(corpusAdded, stemDocument)

# Create matrix
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded <- sparseAdded
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
wordsAdded <- as.data.frame(as.matrix(wordsAdded))

#Removed
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved,removeWords,stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <-DocumentTermMatrix(corpusRemoved)
sparseRemoved <- (removeSparseTerms(dtmRemoved,0.997))
wordsRemoved <- sparseRemoved
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wordsRemoved <- as.data.frame(as.matrix(wordsRemoved))
str(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$vandal <- wiki$Vandal

set.seed(123)
library(caTools)

split <- sample.split(wikiWords$vandal,SplitRatio = 0.7)
wikiTrain <- wikiWords[split==T,]
wikiTest <- wikiWords[split==F,]

table(wikiTest$vandal)
618/(618+545)

#build cart
library(rpart)
library(rpart.plot)

wikiCART <- rpart(vandal ~ ., data = wikiTrain, method = "class")
  
predictCART <- predict(wikiCART,newdata = wikiTest, type = "class")  

table(wikiTest$vandal,predictCART )  

(618+12)/nrow(wikiTest)  
  
prp(wikiCART)  
  
wikiWords2 <- wikiWords  
  
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)

wikiTest2 = subset(wikiWords2, split==FALSE)

wiki2CART <- rpart( vandal ~ ., data = wikiTrain2, method = "class")
predictCART2 <- predict(wiki2CART, newdata = wikiTest2, type = "class")
table(wikiTest2$vandal, predictCART2)
(609+57)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)
(514+248)/nrow(wikiTest2)

str(wikiWords2)

wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, split==TRUE)

wikiTest3 = subset(wikiWords3, split==FALSE)

wiki3CART <- rpart( vandal ~ ., data = wikiTrain3, method = "class")
predictCART3 <- predict(wiki3CART, newdata = wikiTest3, type = "class")
table(wikiTest3$vandal, predictCART3)
(595+241)/nrow(wikiTest3)
prp(wiki3CART)


#Assignment2#################
trials <- read.csv("clinical_trial.csv", stringsAsFactors = F)
str(trials)
numchar <- nchar(trials$abstract)

summary(numchar)
nrow(trials[numchar==0,])

# Create Corpus
corpusTitle <- Corpus(VectorSource(trials$title)) 
corpusAbstract <- Corpus(VectorSource(trials$abstract)) 

# Convert to lower case
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

# Remove punctuation 
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# Remove Stop words
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem the words
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

# Look at the first document
corpusTitle[[1]]

# Create matrix
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
#dtmTitle
#dtmAbstract

# Filter out sparse terms by keeping only terms that appear in at least 5% or more of the documents
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

which.max(colSums(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)

dtm$trial <- trials$trial

library(caTools)
set.seed(144)
spl <- sample.split(dtm$trial, SplitRatio = 0.7)
train <- subset(dtm, spl == T)
test <- subset(dtm, spl == F)

# baseline model accuracy on the training set
table(train$trial)[1] / sum(table(train$trial))

# CART Model
library(rpart)
library(rpart.plot)
trialsCART <- rpart(trial~., data=train, method="class")
prp(trialsCART)

# Predict using the trainig set. Because the CART tree assigns the same predicted probability to each leaf node and there are a small number of leaf nodes compared to data points, we expect exactly the same maximum predicted probability.
predTrain <- predict(trialsCART)[,2]

predTrain = predict(trialCart)[,2]

summary(predTrain)

# Accuracy on the training set
t1 <-table(train$trial, predTrain >= 0.5)
(631+441)/(631+99+131+441)

# Sensitivity = TP/(TP+FN) and specificity=TN/(TN+FP)
t1[2,2]/(t1[2,2] + t1[2,1])
t1[1,1]/(t1[1,1] + t1[1,2])

# Testing set accuracy
predTest <- predict(trialsCART, newdata = test)[,2]
t2 <- table(test$trial, predTest >= 0.5)
(t2[1,1] + t2[2,2])/(sum(t2))

# ROC Curve
library(ROCR)

predROCR = prediction(predTest, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


#Assignment3################################################

emails <- read.csv("emails.csv", stringsAsFactors = F)
str(emails)

# Number of spam emails
sum(emails$spam == 1)

# How many characters are in the longest email?
max(nchar(emails$text))

# Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))

# Pre process data
library(tm)
# Create Corpus
corpus <- Corpus(VectorSource(emails$text)) 

# Convert to lower case
corpus <- tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

# Remove punctuation 
corpus <- tm_map(corpus, removePunctuation)

# Remove Stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Stem the words
corpus <- tm_map(corpus, stemDocument)

# Create matrix
dtm <- DocumentTermMatrix(corpus)
dtm

# Filter out sparse terms by keeping only terms that appear in at least 5% or more of the documents
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm

# Convert spdtm and to a data frame
emailsSparse <- as.data.frame(as.matrix(spdtm)) # make.names is set to true to make the variable names of emailsSparse valid
colnames(emailsSparse) <- make.names(colnames(emailsSparse), unique=T)

#  What is the word stem that shows up most frequently across all the emails in the dataset?
which.max(colSums(emailsSparse))

# Add variable spam
emailsSparse$spam <- emails$spam 

# Create a data set where spam == 0 (ham). The ham dataset is certainly personalized to Vincent Kaminski, and therefore it might not generalize well to a general email user. Caution is definitely necessary before applying the filters derived in this problem to other email users.
spam <- emailsSparse[emailsSparse$spam==1,]
# How many word stems appear at least 5000 times in the ham emails in the dataset?
sum(colSums(spam) >= 1000)

# Spam data set
sort(colSums(subset(emailsSparse, spam == 1)))

emailsSparse$spam <- as.factor(emailsSparse$spam)

# Load CaTools
library(caTools)
set.seed(123)
spl <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, spl == T)
test <- subset(emailsSparse, spl == F)

# Logistic regression model
spamLog <- glm(spam~., data = train, family=binomial)
summary(spamLog)
predLog <- predict(spamLog, type="response")
sum(predLog < 0.00001)
sum(predLog > 0.99999)
sum(predLog > 0.00001 & predLog < 0.99999)

# Accuracy
tLog <- table(train$spam, predLog >= 0.5)
(tLog[1,1] + tLog[2,2]) / sum(tLog)
# AUC 
library(ROCR)

predROCR = prediction(predLog, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


# CART Model
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam~., data=train, method="class")
prp(spamCART)

# Predict using the trainig set. 
predTrain <- predict(spamCART)[,2]
# Accuracy on the training set
tCART <- table(train$spam, predTrain >= 0.5)
(tCART[1,1] + tCART[2,2])/(sum(tCART))

# AUC of the CART model
predROCRCART = prediction(predTrain, train$spam)
perfROCRCART = performance(predROCRCART, "tpr", "fpr")
performance(predROCRCART, "auc")@y.values


# Random Forest model
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam~., data=train, method="class")

# Accuracy of RF Model
predRF <- predict(spamRF, type="prob")[,2]
tRF <- table(train$spam, predRF >= 0.5)
(tRF[1,1] + tRF[2,2])/(sum(tRF))

# Performance of RF Model
predROCRRF = prediction(predRF, train$spam)
performance(predROCRRF, "auc")@y.values


# Testing set accuracy
predTestLog <- predict(spamLog, newdata = test, type="response")
t2 <- table(test$spam, predTestLog >= 0.5)
(t2[1,1] + t2[2,2])/(sum(t2))

# ROC Curve
library(ROCR)

predROCRLog = prediction(predTestLog, test$spam)

# Compute AUC
performance(predROCRLog, "auc")@y.values

# CART Test Accuracy and performance
predTestCART <- predict(spamCART, newdata = test)[,2]
t3 <- table(test$spam, predTestCART >= 0.5)
(t3[1,1] + t3[2,2])/(sum(t3))
predROCRCART = prediction(predTestCART, test$spam)

# Compute AUC
performance(predROCRCART, "auc")@y.values

predTestRF <- predict(spamRF,newdata = test, type = "prob")
table(test$spam,predTestRF > 0.5)
