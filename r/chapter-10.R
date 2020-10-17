# 10.4 Lab 1: Principal Component Analysis
states <- row.names(USArrests)
states

names(USArrests)
# The apply() function allows us to apply a function - in this case, the mean() function
# - to each row or column of the data set. The second input here denotes whether we wish 
# to compute the mean of the rows, 1, or the columns, 2. 
apply(USArrests, 2, mean)

# Not surprisingly, the variables also have vastly different variances: the UrbanPop
# variable measures the percentage of the population in each state living in an urban
# area, which is not a comparable number to the number of rapes in each state per 100k
# individuals. If we failed to scale the variables before performing PCA, then most of 
# the principal components we observed would be driven by the Assault variable, since
# it has by far the largest mean and variance. Thus, it is important to standardize
# the variables to have mean zero and standard deviation one before performing PCA.
apply(USArrests, 2, var)

# By default, the prcomp() function centers the variables to have mean zero. By using
# the option scale=TRUE, we scale the variables to have standard deviation one. 
pr.out <- prcomp(USArrests, scale=T)

# The output from prcomp() contains a number of useful quantities.
names(pr.out)

# The center and scale components correspond to the means and standard deviations of
# the variables that were used for scaling prior to implementing PCA.
pr.out$center
pr.out$scale

# The rotation matrix provides the principal component loadings; each column of pr.out
# $rotation contains the corresponding principal component loading vector.
pr.out$rotation

# The 50x4 matrix x has as its columns the principal component score vectors. That is,
# the kth column is the kth principal component score vector.
dim(pr.out$x)

# The scale=0 argument to biplot() ensures that the arrows are sclaed to represent the
# loadings; other values for scale give slightly different biplots with different 
# interpretations.
biplot(pr.out, scale=0)

# Recall that the principal components are only unique up to a sign change.
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x

biplot(pr.out, scale=0)

# The prcomp() function also outputs the standard deviation of each principal component.
pr.out$sdev

# The variance explained by each principal component is obtained by squaring these
pr.var <- pr.out$sdev^2
pr.var

# To compute the proportion of variance explained by each principal component, we
# simply divide the variance explained by each principal component by the total 
# variance explained by all four principal components.
pve <- pr.var/sum(pr.var)
pve

# We can plot the OVE explained by each component, as well as the cumulative PVE, as
# follows
plot(pve, xlab="Principal Compoent", ylab="Proportion of Variance Explained", 
     ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1), type="b")

# Note that the function cumsum() computes the cumulative sum of the elements of a 
# numeric vector. For instance
a <- c(1, 2, 8, -3)
cumsum(a)

# 10.5 Lab 2: Clustering
# 10.5.1 K-Means Clustering
set.seed(2)

# We begin with a simple simulated example in which there truly are two clusters in
# the data: the first 25 observations have a mean shift relative to the next 25 
# observations.
x <- matrix(rnorm(50*2), ncol=2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

# The function kmeans() performs K-means clustering in R.
km.out <- kmeans(x, 2, nstart=20)

# The cluster assignment of the 50 observations are contained in km.out$cluster
km.out$cluster

# The K-means clustering perfectly separated the observations into two clusters even
# though we did not supply any group information to kmeans(). We can plot the data, 
# with each observation colored according to its cluster assignment.
# Here the observations can be easily plotted because they are two-dimensional. If 
# there were more than two variables then we could instead perform PCA and plot the
# data, with each observation colored according to its cluster assignment.
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results With K=2",
             xlab="", ylab="", pch=20, cex=2)

set.seed(4)
# In this example, we knew that there really were two clusters because we generated
# the data. However, for real data, in general we do not know the true member of 
# clusters. We could instead have performed K-means clustering on this example with
# K=3. When K=3, K-means clustering splits up the two clusters.
km.out <- kmeans(x, 3, nstart=20)
km.out

# To run the kmeans() function in R with multiple initial cluster assignments, we use
# the nstart argument. If a value of nstart greater than one is used, then K-means
# clustering will be performed using multiple random assignments in Step 1 of 
# Algorithm 10.1, and the kmeans() function will report only the best results.
set.seed(3)
km.out <- kmeans(x, 3, nstart=1)
km.out$tot.withinss

km.out <- kmeans(x, 3, nstart=20)
# Note that km.out$tot.withinss is the total within-cluster sum of squares, which we
# seek to minimize by perfoming K-means clustering (Equation 10.11). The individual
# within-cluster sum-of-squares are contained in the vector km.out$withinss.
# We strongly recommend always running K-means clustering with a large value of nstart,
# such as 20 or 50, since otherwise an undesirable local optimum may be obtained.
# When performing K-means clustering, in addition to using multiple initial cluster
# assignments, it is also important to set a random seed using the set.seed() function.
# This way, the initial cluster assignments in Step 1 can be replicated, and the 
# K-means output will be fully reproducible.
km.out$tot.withinss

# 10.6.1 PCA on the NCI60 Data
pr.out <- prcomp(NCI60$data,scale=T)

# We now plot the first few principal component score vectors, in order to visualize
# the data. The observations (cell lines) corresponding to a given cancer type will
# be plotted in the same color, so that we can see to what extent the observations
# within a cancer type are similar to each other. We first create a simple function 
# that assigns a distinct color to each element of a numeric vector. The function will
# be used to assign a color to each of the 64 cell lines, based on the cancer type to 
# which it corresponds.
# Note that the rainbow() function takes as its argument a positive integer, and 
# returns a vector containing that number of distinct colors. We now can plot the
# principal component score vectors.
Cols <- function(vec) {
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# The resulting plots can be seen below. On the whole, cell lines corresponding to a
# single cancer type do tend to have similar values on the first few principal 
# component score vectors. This indicates that cell lines from the same cancer type
# tend to have pretty similar gene expression levels.
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(NCI60$data), pch=19, xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(NCI60$data), pch=19, xlab="Z1", ylab="Z3")

# We can obtain a summary of the proportion of variance explained (PVE) of the first
# few principal components using the summary() method for a prcomp object
summary(pr.out)

# Note that the height of each bar plot is given by squaring the corresponding element
# of pr.out$sdev
plot(pr.out)

# However, it is more informative to plot the PVE of each principal component 
# (i.e. scree plot) and the cumulative PVE of each principal component. This can be
# done with just a little work.
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))

# We see that together, the first seven principal components explain around 40% of
# the variance in the data. This is not a huge amount of the variance. However, 
# looking at the scree plot, we see that while each of the first seven principal 
# components explain a substantial amount of variance, there is a marked decrease in
# the variance explained by further principal components. That is, there is an elbow
# in the plot after approximately the seventh principal component. This suggests that
# there mat be little benefit to examining more than seven or so principal components
# (though even examining seven principal components may be difficult).
plot(pve, type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="blue")

# 10.6.2 Clustering the Observations of the NCI60 Data
# We now proceed to hierarchically cluster the cell lines in the NCI60 data, with the
# goal of finding out whether or not the observations cluster into distinct types of
# cancer. To begin, we standardize the variables to have mean zero and standard 
# deviation one. As mentioned earlier, this step is optional and should be performed
# only if we want each gene to be on the same scale.
sd.data <- scale(NCI60$data)

# We now perform hierarchical clustering of the observations using complete, single, and
# average linkage. Euclidean distance is used as the dissimilarity measure.
par(mfrow=c(3, 1))
data.dist <- dist(sd.data)
# We see that the choice of linkage certainly does affect the results obtained. Typically,
# single linkage will tend to yield trailing clusters: very large clusters onto which individual 
# observations attach one-by-one. On the other hand, complete and average linkage tend to 
# yield more balanced, attractive clusters. For this reason, complete and average linkage
# are generally preferred to single linkage. Clearly cell lines within a single cancer type
# tend to cluster together, although the clustering is not perfect.
plot(hclust(data.dist), labels=NCI60$labs, main="Complete Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="average"), labels=NCI60$labs, main="Average Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="single"), labels=NCI60$labs, main="Single Linkage", xlab="", sub="", ylab="")

# We can cut the dendrogram at the height that will yield a particular number of clusters
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, NCI60$labs)

# There are some clear patterns. All the leukemia cell lines fall in cluster 3, while the
# breast cancel cell lines are spread out over three different clusters. We can plot the cut
# on the dendrogram that produces these four clusters
par(mfrow=c(1,1))
plot(hc.out, labels=NCI60$labs)
# The abline() function draws a straight line on top of any existing plot in R. The argument
# h=139 plots a horizontal line at height 139 on the dendrogram; this is the height that 
# results in four distinct clusters. It is easy to verify that the resulting clusters are
# the same as the ones we obtained using cutree(hc.out, 4)
abline(h=139, col="red")

hc.out

# We claimed earlier in Section 10.3.2 that K-means clustering and hierarchical clustering
# with the dendrogram cut to obtain the same number of clusters can yield very different
# results
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
# We see that the four clusters obtained using hierarchical clustering and K-means clustering 
# are somewhat different. Cluster 2 in K-means clustering is identical to cluster 3 in 
# hierarchical clustering. However, the other clusters differ: for instance, cluster 4 in 
# K-means clustering contains a portion of the observations assigned to cluster 2 by 
# hierarchical clustering.
table(km.clusters, hc.clusters)

# Rather than performing hierarchical clustering on the entire data matrix, we can simply
# perform hierarchical clustering on the first few principal component score vectors.
hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=NCI60$labs, main="Hier. Clust. on First Five Score Vectors")
# Not surprisingly, these results are different from the ones that we obtained when performed
# hierarchical clustering on the full data set. Sometimes performing clusteriong on the first
# few principal component score vectors can give better results than performing clustering
# on the full data. In this situation, we might view the principal component step as one of
# denoising the data. We could also perform K-means clustering on the first few principal
# component score vectors rather than the full data set.
table(cutree(hc.out, 4), NCI60$labs)
