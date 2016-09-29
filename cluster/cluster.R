dailyKos <- read.csv("dailykos.csv")
str(dailyKos)

#calculate distances
distances <- dist(dailyKos, method = "euclidean")

#hierarchical clustering
kosHierClust <- hclust(distances, method="ward.D")

#dendrogram
plot(kosHierClust)

#cut the tree!
clusterGroups = cutree(kosHierClust, k = 7)
str(clusterGroups)

sub1 <- subset(dailyKos, clusterGroups==1)
sub2 <- subset(dailyKos, clusterGroups==2)
sub3 <- subset(dailyKos, clusterGroups==3)
sub4 <- subset(dailyKos, clusterGroups==4)
sub5 <- subset(dailyKos, clusterGroups==5)
sub6 <- subset(dailyKos, clusterGroups==6)
sub7 <- subset(dailyKos, clusterGroups==7)

#alternate split method
HierCluster <- split(dailyKos,clusterGroups)
str(HierCluster)
HierCluster[[1]]

#look at top 6 words in each cluster
tail(sort(colMeans(sub1)))
tail(sort(colMeans(sub2)))
tail(sort(colMeans(sub3)))
tail(sort(colMeans(sub4)))
tail(sort(colMeans(sub5)))
tail(sort(colMeans(sub6)))
tail(sort(colMeans(sub7)))


#k-means clustering
set.seed(1000)

KclusterGroups <- kmeans(dailyKos,centers = 7)
str(KclusterGroups)

Kcluster <- split(dailyKos,KclusterGroups$cluster)

table(KclusterGroups$cluster)


tail(sort(colMeans(Kcluster[[1]])))
tail(sort(colMeans(Kcluster[[2]])))
tail(sort(colMeans(Kcluster[[3]])))
tail(sort(colMeans(Kcluster[[4]])))
tail(sort(colMeans(Kcluster[[5]])))
tail(sort(colMeans(Kcluster[[6]])))
tail(sort(colMeans(Kcluster[[7]])))

table(clusterGroups, KclusterGroups$cluster)
