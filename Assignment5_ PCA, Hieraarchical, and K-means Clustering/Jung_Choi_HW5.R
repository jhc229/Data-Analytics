# 1. (20 points) BONUS: complete the laptop cores survey on Canvas by Sunday, Oct
# 30th 2016.

#------------------------------------------------------------------------------------------------------------------------
# 2. (40 points) Principal Components Analysis.
# (a) Load the USArrests dataset in R, and describe the dataset in your own words, in 2-3
# lines.
# (b) Calculate the mean and standard deviation of the four variables.
# (d) How many principal components would you need to explain at least 
  # (i) 60% of the total variance? 
  # (ii) 75% of the total variance?
  # (iii) 90% of the total variance?

data(USArrests)
# USArrests is a dataset that contains number of arrests for murder, assault and rape for each of the 50 US states in 1973.
# There is another column that contains percent values of urban population.

usArrest = data.frame(USArrests)

summary(usArrest)
library(psych)
describe(usArrest)

# (c) Perform principal components analysis of the dataset using R. How many principal
# components are there? For each principal component, report the standard deviation and
# the proportion of variance explained.
require(graphics)

## The variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
(pc.cr <- princomp(usArrest))  # inappropriate
princomp(usArrest, cor = TRUE) # =^= prcomp(USArrests, scale=TRUE)
## Similar, but different:
## The standard deviations differ by a factor of sqrt(49/50)
arrest.pca <- prcomp(usArrest,scale. = TRUE)
arrest.pca

summary(pc.cr <- princomp(usArrest, cor = TRUE))
loadings(pc.cr)  # note that blank entries are small but not zero



mean(USArrests$Murder)
sd(USArrests$Murder)

mean(USArrests$Assault)
sd(USArrests$Assault)

mean(USArrests$UrbanPop)
sd(USArrests$UrbanPop)

mean(USArrests$Rape)
sd(USArrests$Rape)

pc = prcomp(USArrests, scale=TRUE)

summary(pc)

# Standard deviation and proportion of variance explain is given in output

# 60 percent requires 1 principal component
# 75 percent requires 2 principal components
# 90 percent requires 3 principal components


#------------------------------------------------------------------------------------------------------------------------
# 3. (60 points) Hierarchical and k-means clustering.
# (a) Load the NCI60 dataset from the ISLR package in R, and describe the dataset in your
# own words, in 2-3 lines.
install.packages("ISLR")
library(ISLR)

# NCI60 is a dataset that contains microarray data which is expressed in 64 by 6830 matrix. It contains 6830 genes from 64 cancer cell lines.


# (b) Using Euclidean distance as the dissimilarity measure, perform hierarchical clustering on
# the data, with 
# (i) Complete Linkage, 
# (ii) Average Linkage, and 
# (iii) Single Linkage.
# Cluster method   : complete 
# Distance         : euclidean 
# Number of objects: 64 
# Cluster method   : single
# Distance         : euclidean
# Number of objects: 64
# Cluster method   : average 
# Distance         : euclidean 
# Number of objects: 64 


nci.labels = NCI60$labs 
nci.data = NCI60$data 
dim(nci.data)

# hierarchical 
nci.std = scale(nci.data) # bringing all genes to the same scale 
par(mfrow =c(3,1)) 
data.dist=dist(nci.data) #Euclidean distance is used as the dissimilarity measure
hc.outComp =hclust(data.dist) 
hc.outAvg =hclust(data.dist, method="average") 
hc.outSing =hclust(data.dist, method="single") 
hc.outComp
hc.outAvg
hc.outSing


plot(hclust (data.dist), labels =nci.labels , main=" Complete
     Linkage ", xlab ="", sub ="", ylab ="") 
plot(hclust (data.dist , method ="average"), labels =nci.labels ,
     main=" Average Linkage ", xlab ="", sub ="", ylab ="") 
plot(hclust (data.dist , method ="single"), labels =nci.labels ,
     main=" Single Linkage ", xlab="", sub ="", ylab ="")

# hc.out =hclust (dist(nci.std)) 
# hc.out


# (c) For all three methods in (b), cut the hierarchical clustering tree at 4 clusters, and report
# the two-way table of actual cancer types ( NCI60$labs ) and clusters. Are there any
# differences between the tables from different methods?
hc.clustersComp =cutree (hc.outComp ,4) 
hc.clustersAvg =cutree (hc.outAvg ,4) 
hc.clustersSing =cutree (hc.outSing ,4) 

table(hc.clustersComp, nci.labels)
table(hc.clustersAvg, nci.labels)
table(hc.clustersSing, nci.labels)


# (d) Perform k-means clustering of the data with k=4 clusters. Report the two-way table of
# actual cancer types ( NCI60$labs ) and clusters.
#kmeans 


km.out =kmeans(nci.std, 4, nstart =20) #nstart: different random initializations 
km.clusters =km.out$cluster 

table(km.clusters,hc.clustersComp) # even with same K, hclus and kmeans can give different
table(km.clusters,hc.clustersAvg) # even with same K, hclus and kmeans can give different
table(km.clusters,hc.clustersSing) # even with same K, hclus and kmeans can give different




