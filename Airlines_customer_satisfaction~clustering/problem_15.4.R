#import EastWestAirlinesCluster.xls
Airlines.data <-read_excel("EastWestAirlinesSpring2022.xlsx")
View(Airlines.data)

#converting tibble to DF
Airlines.df<-data.frame(Airlines.data)
class(Airlines.df)

View(Airlines.df)

#setting the ID# to be the rownames
row.names(Airlines.df) <- Airlines.df[,1]
View(Airlines.df)

#removing ID# column

Airlines.df <- Airlines.df[,-c(1)]
View(Airlines.df)

#PARTA



#set k = 4 to create 4 clusters. The nstart paramater specifies the number of times to run the cluster analysis with
#different starting points (seeds).
kmu <- kmeans(Airlines.df, 4, nstart = 100)

kmu
kmu$cluster

# show cluster membership
kmu$cluster
View(kmu$cluster)
View(sort(kmu$cluster))
print(sort(kmu$cluster))
write.csv(kmu$cluster, "Airlinescluster.csv")
print(kmu$cluster)
kmu$centers
print(dist(kmu$centers))
View(kmu$centers)
kmu


# get centroids
View(kmu$centers)
write.csv(kmu$centers, "Airlinesclustercenters.csv")

#to remove scientific notation
options(scipen = 100, digits = 4)

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(kmu$centers), max(kmu$centers)), xlim = c(0, 11))



# label x-axes
axis(1, at = c(1:11), labels = names(Airlines.df))



# plot centroids for n = k
for (i in c(1:4))
  lines(kmu$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 4),
                                                        "black", "dark grey"))



# name clusters
text(x = 0.5, y = kmu$centers[, 1], labels = paste("Cluster", c(1:4)))



#Create an elbow chart to select the best k
#Elbow chart calculates the weighted sum of squares for each cluster.
#Look for change in slope from steep to flat



library(factoextra)
fviz_nbclust(Airlines.df, kmeans, method = "wss") + theme_minimal() + ggtitle("Elbow Chart")


#plotting a cluster plot
fviz_cluster(kmu, data = Airlines.df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#FF0000"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


#PARTB



# normalize Airlines.df variables so all variables have mean of 0 and sd of 1
Airlines.df.norm <- scale(Airlines.df, center = TRUE, scale = TRUE)
View(Airlines.df.norm)



#set k = 4 to create 4 clusters. The nstart paramater specifies the number of times to run the cluster analysis with
#different starting points (seeds).
kmu_t <- kmeans(Airlines.df.norm, 4, nstart = 100)


plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(kmu_t$centers), max(kmu_t$centers)), xlim = c(0, 11))



# label x-axes
axis(1, at = c(1:11), labels = names(Airlines.df))



# plot centroids for n = k
for (i in c(1:4))
  lines(kmu_t$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 4),
                                                        "black", "dark grey"))



# name clusters
text(x = 0.5, y = kmu_t$centers[, 1], labels = paste("Cluster", c(1:4)))

fviz_nbclust(Airlines.df.norm, kmeans, method = "wss") + theme_minimal() + ggtitle("Elbow Chart")

fviz_cluster(kmu_t, data = Airlines.df.norm,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#FF0000"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



#heatmap(as.matrix(Airlines.df.norm), Colv = NA, hclustfun = hclust,
       # col=rev(paste("gray",1:99,sep="")))

image(t(as.matrix(Airlines.df.norm))[, order(kmu$cluster)], yaxt = "n", main = "K means heat-map")

#PARTC



# show cluster membership
View(kmu$cluster)
print(sort(kmu$cluster))
write.csv(kmu$cluster, "Airlinescluster.csv")
print(kmu$cluster)
print(dist(kmu$centers))
View(kmu$centers)



# get centroids
View(kmu$centers)
write.csv(kmu$centers, "Airlinesclustercenters.csv")



# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(kmu$centers), max(kmu$centers)), xlim = c(0, 12))



# label x-axes
axis(1, at = c(1:12), labels = names(Airlines.df.norm))



# plot centroids for n = 4
for (i in c(1:12))
  lines(kmu$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                        "black", "dark grey"))



# name clusters
text(x = 0.5, y = kmu$centers[, 1], labels = paste("Cluster", c(1:4)))



#Create an elbow chart to select the best k
#Elbow chart calculates the weighted sum of squares for each cluster.
#Look for change in slope from steep to flat



library(factoextra)
fviz_nbclust(Airlines.df.norm, kmeans, method = "wss") + theme_minimal() + ggtitle("Elbow Chart")



#PARTE



# calculate normalized distance based on all 8 variables
d.norm <- dist(Airlines.df.norm, method = "euclidean")
m <- as.matrix(d.norm)
m


#In hclust() set argument method = to "ward.D", "single", "complete", "average", "median", or "centroid"
#Hierarchical clustering using "single" distance measure
hc1 <- hclust(d.norm, method = "single")



#display dendogram of hc1
#hang: hang -1 adjusts cluster names so that names appear on x axis
#ann: a logical value indicating whether the default annotation (title and x and y axis labels)
#should appear on the plot.
plot(hc1, hang = -1, ann = FALSE)



#Hierarchical clustering using "average" distance measure
hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)

#Hierarchical clustering using "median" distance measure
hc3 <- hclust(d.norm, method = "median")
plot(hc3, hang = -1, ann = FALSE)


#Hierarchical clustering using "centroid" distance measure
hc4 <- hclust(d.norm, method = "centroid")
plot(hc4, hang = -1, ann = FALSE)

hc4 <- hclust(d.norm, method = "ward.D")
plot(hc4, hang = -1, ann = FALSE)

plot(hc2)
#assign observations to '3' clusters using splits depicted in map
memb <- cutree(hc1, k = 3)




View(memb)




#assign observations to '6' clusters using splits depicted in map
memb_average <- cutree(hc2, k = 3)
View(memb_average)
write.csv(memb, "memb.csv")



# label clusters and add company name
row.names(Airlines.df.norm) <- paste(memb_average, ": ", row.names(Airlines.df.norm), sep = "")
View(Airlines.df.norm)



# plot heatmap
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(Airlines.df.norm), Colv = NA, hclustfun = hclust,
        col=rev(paste("gray",1:99,sep="")))

