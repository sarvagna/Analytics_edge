library(maps)
library(ggmap)
library(caTools)

statesMap = map_data("state")
str(statesMap)

table(statesMap$group)

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

polling <- read.csv("PollingImputed.csv")
str(polling)
Train <- subset(polling, Year <= 2008)
Test <- subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(TestPredictionBinary)

mean(TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

predictionMap = predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

str(predictionMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

mean(subset(predictionMap,Test.State == "Florida")[,7])


# Facebook ----------------------------------------------------------------

edges <- read.csv("edges.csv")

users <- read.csv("users.csv")

install.packages("igraph")
library(igraph)

g = graph.data.frame(edges,FALSE,users)

plot(g, vertex.size=5, vertex.label=NA)

degree(g)

V(g)$size = degree(g)/2+2

plot(g, vertex.label=NA)

summary(V(g)$size)

V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

V(g)$color[V(g)$locale == "AB"] = "blue"

str(users)




# twitter -----------------------------------------------------------------

tweets <- read.csv("tweets.csv", stringsAsFactors = F)
str(tweets)

library(tm)
corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

frequencies = DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(frequencies))

install.packages("wordcloud")
library(wordcloud)

colSums(allTweets)

wordcloud(colnames(allTweets),colSums(allTweets))

library(RColorBrewer)





