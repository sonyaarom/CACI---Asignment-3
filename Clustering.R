df <- read.csv("smartwatch_survey.csv")
dim(df)
str(df)
summary(df)
pacman::p_load(reshape2, ggplot2, psych, corrplot, fpc, cluster)
pairs.panels(df[, 2:14],
             method = "pearson",  # correlation method
             hist.col = "grey60", # color of hist. bins
             density = TRUE,      # show density plots
             lm = TRUE) 
#Importance data and WTP as clustering basis 
df.cluster = df[, 1:14]
summary(df.cluster)
#Standardize with WTP
data.dist<-dist(apply(df.cluster[,c(-1)],2,scale)) 
as.matrix(data.dist)[1:5,1:5]

#Hierarchical
#Trial of different approaches
cl.single <- hclust(data.dist, method = "single")     # single linkage method
cl.complete <- hclust(data.dist, method = "complete") # complete linkage method
cl.average <- hclust(data.dist, method = "average")   # average linkage method
cl.centroid <- hclust(data.dist, method = "centroid") # centroid linkage method
cl.median <- hclust(data.dist, method = "median")     # median linkage method
cl.ward <- hclust(data.dist, method = "ward.D2")      # ward's method
#Single linkage method - smallest distance between members of two clusters
plot(as.dendrogram(cl.single), ylim = c(0, 3))
plot(as.dendrogram(cl.single), ylim = c(0, 3),
     leaflab = "none") 
table(cutree(cl.single, 20))
#Single linkage method shows the similar problem as in the lecture, chaining effect of single linkage
#Complete linkage method - largest distance between members of two clusters, gave better results
plot(as.dendrogram(cl.complete))
rect.hclust(cl.complete, k = 3, border = "darkred") 
table(cutree(cl.complete, 3)) #nice
table(cutree(cl.complete, 4)) #group 3 has too much, probably not the precisest groupping

#Average linkage method - average distance between members of two clusters
plot(as.dendrogram(cl.average))
rect.hclust(cl.average, k = 3, border = "darkred") 
table(cutree(cl.average, 3))
#does not give us good groupping

#Centroid linkage -euclidean distance between the cluster averages of the variable -biased toward finding round or spherical cluster
plot(as.dendrogram(cl.centroid))
rect.hclust(cl.centroid, k = 3, border = "darkred") 
table(cutree(cl.centroid, 3))
#does not give us good groupping

#Median linkage - euclidean distance between the cluster averages of the variables
plot(as.dendrogram(cl.median))
rect.hclust(cl.median, k = 3, border = "darkred") 
table(cutree(cl.median, 3))
#does not give us good groupping

#Minimum variance method - Minimum increase in total sum of squares
plot(as.dendrogram(cl.ward))
rect.hclust(cl.ward, k = 3, border = "darkred") 
table(cutree(cl.ward, 3)) 
table(cutree(cl.ward, 4)) 
#close to the complete linkage result, might be useful to compare


#Finding number of segments that we need to use
VRC.ward = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.ward[k] <- cluster.stats(d = data.dist, 
                               clustering = cutree(cl.ward, k))$ch
}
VRC.ward = VRC.ward[-1]
VRC.ward

VRC.complete = rep(0, length(2:10)) # initialize
for(k in 2:10){
  VRC.complete[k] <- cluster.stats(d = data.dist, 
                                   clustering = cutree(cl.complete, k))$ch
}
VRC.complete = VRC.complete[-1]

VRC = data.frame(K = 2:10, complete = VRC.complete, ward = VRC.ward)

VRC = melt(VRC, id.vars = "K")


ggplot(VRC, aes(x = K, y = value)) +
  geom_point() + geom_line() +
  facet_grid(variable~.) +
  labs(x = "Number of Clusters", 
       y = "Variance Ratio (Calinski-Harabasz) index") +
  theme_bw()

#4 clusters?

# Non-hierarchicak: K-mean Clustering

set.seed(185) 
cl.kmeans <- kmeans(data.dist, centers = 4)
str(cl.kmeans)
cl.kmeans$cluster

df$cluster_kmeans <- cl.kmeans$cluster
df$cluster_ward <- cutree(cl.ward, 4)
df$cluster_complete <-cutree(cl.complete, 4)
head(df)

#Need to decide which method will be used and number of cluster solutions, then to combine with initial data set

