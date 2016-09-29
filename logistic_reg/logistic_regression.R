#logistic regresion
setwd("C:/Users/jhs/Desktop")
quality<-read.csv("quality.csv")
library(caTools)
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain<- subset(quality, split==TRUE)
qualityTest<- subset(quality, split==FALSE)

QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family = binomial)
summary(QualityLog)


predictTrain <- predict(QulaityLog,type="response")
summary(predictTrain)

tapply(predictTrain,qualityTrain$PoorCare,mean)

#to split data with continuos outcomes
spl = sample(1:nrow(data), size=0.7 * nrow(data))
train = data[spl,]
test = data[-spl,]


#Assignment part1
songs<-read.csv("songs.csv")
nrow(songs)
x<-subset(songs,artistname=="Michael Jackson" & Top10==1)[,"songtitle"]
subset(songs,artistname=="Michael Jackson" & Top10==1)
summary(songs)



table(songs$timesignature)
which.max(songs$tempo)
songs[6206,]

SongsTrain<-subset(songs,year<=2009)
SongsTest<-subset(songs,year==2010)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$loudness,SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

predictTest = predict(SongsLog3, type="response", newdata=SongsTest)
table(SongsTest$Top10, predictTest > 0.45)
# FALSE TRUE
# 0   309    5
# 1    40   19

#Accuracy
(309+19)/(309+5+40+19)

#Baseline accuracy
(309+5)/(309+5+40+19)

#sensitivity
19/(19+40)

#specificity
309/(309+5)


#second assignment###############
parole<- read.csv("parole.csv")
str(parole)
table(parole$violator)
parole$state<-as.factor(parole$state)
parole$crime<-as.factor(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model1<- glm(violator ~ .,data=train,family=binomial)
summary(model1)

# male: 1 if the parolee is male, 0 if female
# race: 1 if the parolee is white, 2 otherwise
# age: the parolee's age (in years) when he or she was released from prison
# state: a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. The three states were selected due to having a high representation in the dataset.
# time.served: the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
# max.sentence: the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
# multiple.offenses: 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
# crime: a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
# violator: 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.

#Consider a parolee who is male, of white race, aged 50 years at prison release,
#from the state of Maryland, served 3 months, had a maximum sentence of 12 months,
#did not commit multiple offenses, and committed a larceny. 
#Answer the following questions based on the model's predictions for this 
#individual. (HINT: You should use the coefficients of your model, 
#the Logistic Response Function, and the Odds equation to solve this problem.)


logit = -4.2411574+(0.3869904*1)+(0.8867192*1)+(-0.0001756*50)+(-0.1238867*3)+(0.0802954*12)+(1.61119919*0)+(0.6837143*1)
exp(logit)
1/(1+exp(-logit))

predictTest = predict(model1, type="response", newdata=test)
table(test$violator, predictTest > 0.5)
# FALSE TRUE
# 0   167   12
# 1    11   12

sensitivity<-12/(23)
specificity<-167/(167+12)
accuracy<-(167+12)/(167+12+11+12)
baseline_accuracy<- (167+12)/(167+12+11+12)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)


#Assignment 3 ####################
loan<-read.csv("loans.csv")
str(loan)
table(loan$not.fully.paid)
sapply(loan,function(x) sum(is.na(x)))

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loan), "not.fully.paid")

imputed = complete(mice(loan[vars.for.imputation]))

loan[vars.for.imputation] = imputed

loan_imputed<-read.csv("loans_imputed.csv")
summary(loan)
summary(loan_imputed)
loans<-loan_imputed

set.seed(144)

split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loansTrain = subset(loans, split == TRUE)
loansTest = subset(loans, split == FALSE)

model1<- glm(not.fully.paid ~ .,data=loansTrain,family=binomial)
summary(model1)
logitA = 9.187+(-0.009317*700)
logitB = 9.187+(-0.009317*710)
logitA-logitB
exp(logitA)/exp(logitB)

predicted.risk = predict(model1, type="response", newdata=loansTest)
summary(predicted.risk)
loansTest$predicted.risk <- predicted.risk

table(loansTest$not.fully.paid, predictTest > 0.5)
# FALSE TRUE
# 0  2400   13
# 1   457    3

accuracy<- (2400+3)/(2400+13+457+3)
baseline_accuracy <- (2400+13)/(2400+13+457+3)

ROCRpred = prediction(predicted.risk, loansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

model2<-glm(not.fully.paid ~ int.rate, data=loansTrain,family=binomial)
summary(model2)
predict2<-predict(model2,type="response",newdata=loansTest)
summary(predict2)
table(loansTest$not.fully.paid, predict2 > 0.5)

ROCRpred = prediction(predict2, loansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

loansTest$profit = exp(loansTest$int.rate*3) - 1
loansTest$profit[loansTest$not.fully.paid == 1] = -1
summary(loansTest$profit)

highInterest <- subset(loansTest,int.rate>=0.15)
str(highInterest)
summary(highInterest$profit)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans<-subset(highInterest,predicted.risk<=cutoff)
str(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)




