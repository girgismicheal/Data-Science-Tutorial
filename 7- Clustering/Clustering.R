#Extract from Machine learning using R by Karthik Ramasubramanian and Abhishek Singh

#The house net worth is a function of StoreArea(sq.mt) and LawnArea(sq.mt).
#Data is without any target variables, the clustering algorithm will show if we can divide the data by the worth scales

# Read the Data
Data_House_Worth <- read.csv("C:/Users/olubi/Desktop/Egypt DTI5126 - Summer 2022/Tutorials/Tut8 - Clustering/House Worth Data.csv",header=TRUE);
                            
str(Data_House_Worth)
glimpse(Data_House_Worth)

#remove the extra column as we will not be using this
Data_House_Worth$BasementArea <- NULL
glimpse(Data_House_Worth) #check to see it is removed

#A quick analysis using a scatterplot shows us there is some relationship between the LawnArea and StoreArea
library(ggplot2)
ggplot(Data_House_Worth, aes(StoreArea, LawnArea, color = HouseNetWorth)) + geom_point()


# apply the hierarchical clustering algorithm
#use the hclust() function and dist() function (computes and returns the distance matrix)
clusters <- hclust(dist(Data_House_Worth[,2:3]))

#Plot the dendrogram
plot(clusters)

# Create different number of clusters
#tables show how much the clusters are able to capture the feature of net worth
clusterCut_2 <- cutree(clusters, 2)
#table the clustering distribution with actual networth
table(clusterCut_2,Data_House_Worth$HouseNetWorth)

clusterCut_3 <- cutree(clusters, 3)
#table the clustering distribution with actual networth
table(clusterCut_3,Data_House_Worth$HouseNetWorth)


clusterCut_4 <- cutree(clusters, 4)
#table the clustering distribution with actual networth
table(clusterCut_4,Data_House_Worth$HouseNetWorth)


ggplot(Data_House_Worth, aes(StoreArea, LawnArea, color = HouseNetWorth)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut_3) + 
  scale_color_manual(values = c('black', 'red', 'green'))


# Elbow Curve
wss <- (nrow(Data_House_Worth)-1)*sum(apply(Data_House_Worth[,2:3],2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(Data_House_Worth[,2:3],centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")


#K-Means Clustering Algorithm
set.seed(917)

#Run k-means cluster of the data set
Cluster_kmean <- kmeans(Data_House_Worth[,2:3], 3, nstart = 20)

#Tabulate the cross distribution
table(Cluster_kmean$cluster,Data_House_Worth$HouseNetWorth)

Cluster_kmean$cluster <- factor(Cluster_kmean$cluster)
ggplot(Data_House_Worth, aes(StoreArea, LawnArea, color = HouseNetWorth)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean$cluster) + 
  scale_color_manual(values = c('black', 'red', 'green'))

