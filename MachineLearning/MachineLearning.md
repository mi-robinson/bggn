---
title: 'Machine Learning 01'
author: "Meg Robinson"
date: "2/14/2022"
output: word_document
---

> Clustering Methods

First, create some data to test and learn with
```{r}
tmp = c(rnorm(30,3), rnorm(30,-3))
data = cbind(x=tmp, y= rev(tmp))
plot(data)
```
Run kmeans(), specifying # of clusters.

```{r}
k = kmeans(data,centers=2,nstart=20)
k
```
> QUESTION: How many points are in each cluster?



```{r}
k$size
```

>ANSWER: 30 points in each cluster 

> QUESTION: What component of your result object details cluster size?

```{r}
k$cluster
```
> QUESTION: What component of your result object details cluser center?

```{r}
k$centers
```

> QUESTION: Plot x colored by the kmeans cluster assignment and add cluster centers as blue points

```{r}
plot(data, col=k$cluster)
points(k$centers, col='blue', pch=15, cex=1.5)
```

> Hierarchical Clustering

```{r}
h = hclust(dist(data))
h
```

Use the plot method:

```{r}
plot(h)
abline(h=7,col='red')
```
Find the membership vector using cutree()

```{r}
cutree(h,h=7)
```

And use this method to find number of k clusters

```{r}
kclst = cutree(h, k=2)
plot(data,col=kclst)
```

We can see that kmeans() uses the data and number of centers, while hclust() uses the distance of the data.

> PCA of UK food data

# import data

```{r}
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
```

> Q1. How many rows and columns are in your new data frame named x? What R functions could you use to answer this questions?

## Complete the following code to find out how many rows and columns are in x?
___(x)

```{r}
dim(x)
```

## Preview the first 6 rows
```{r}
head(x)
```


Row titles are incorrectly being stored as a column. Fix this.

```{r}
rownames(x) <- x[,1]
x <- x[,-1]
head(x)
```
```{r}
dim(x)
```

```{r}
x <- read.csv(url, row.names=1)
head(x)
```
Now there are correctly 17 rows and 4 columns, yay!

> Q2. Which approach to solving the ‘row-names problem’ mentioned above do you prefer and why? Is one approach more robust than another under certain circumstances?

The second approach, read.csv() is way better than the first one in general because it loads the data without manipulation. The first approach, indexing x[,-1], removes data & will continue to remove the last column every time it is run. 

> Spotting major differences and trends

```{r}
barplot(as.matrix(x), beside=T, col=rainbow(nrow(x)))
```

> Q3: Changing what optional argument in the above barplot() function results in the following plot?

We can remove the beside=TRUE argument to obtain the following:

```{r}
barplot(as.matrix(x), col=rainbow(nrow(x)))
```

> Q5: Generating all pairwise plots may help somewhat. Can you make sense of the following code and resulting figure? What does it mean if a given point lies on the diagonal for a given plot?

```{r}
pairs(x, col=rainbow(10), pch=16)
```

The y axis for each row of plots is the country name in that row, whereas the x axis for each column of the plots is the country name in that column. Therefore, the diagonal plots of this graph indicates that the country is the same on the row and column so there is no way to plot a pairwise interaction.

> Q6. What is the main differences between N. Ireland and the other countries of the UK in terms of this data-set?

N. Ireland appears to be more unique as it contains the most values that deviate  from  the diagonal when compared to the other countries. 

> PCA to the rescue

# Use the prcomp() PCA function 

```{r}
pca <- prcomp( t(x) )
summary(pca)
```


> Q7. Complete the code below to generate a plot of PC1 vs PC2. The second line adds text labels over the data points.

# Plot PC1 vs PC2

```{r}
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x))
```

> Q8. Customize your plot so that the colors of the country names match the colors in our UK and Ireland map and table at start of this document.

```{r}
mycols = c("orange", "red", "blue", "green4")
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x), col=mycols)
```

```{r}
v <- round( pca$sdev^2/sum(pca$sdev^2) * 100 )
v
```
```{r}
## or the second row here...
z <- summary(pca)
z$importance
```
```{r}
barplot(v, xlab="Principal Component", ylab="Percent Variation")
```
> Digging deeper (variable loadings):

```{r}
## Lets focus on PC1 as it accounts for > 90% of variance 
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,1], las=2, main="PC1 Loadings" )

```
> Q9: Generate a similar ‘loadings plot’ for PC2. What two food groups feature prominantely and what does PC2 maninly tell us about?

```{r}
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,2], las=2, main="PC2 Loadings" )
```

From looking at this graph, it looks like 'fresh potatoes' and 'soft drinks' feature prominently. The plot also tells us which groups contribute the most to the remaining 10% variance, after PC1. 

> Biplots

```{r}
## The inbuilt biplot() can be useful for small datasets 
biplot(pca)
```

> PCA of RNA-seq data

```{r}
url2 <- "https://tinyurl.com/expression-CSV"
rna.data <- read.csv(url2, row.names=1)
head(rna.data)
```
> Q10: How many genes and samples are in this data set?

```{r}
nrow(rna.data)
```
```{r}
ncol(rna.data)
```
There are 100 genes and 10 samples.

```{r}
## Again we have to take the transpose of our data 
pca <- prcomp(t(rna.data), scale=TRUE)
 
## Simple un polished plot of pc1 and pc2
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2")

text(pca$x[,1:2], labels = colnames(rna.data))
```

```{r}
summary(pca)
```
```{r}
plot(pca, main="Quick scree plot")
```
Lets make our own scree plots:

```{r}
## Variance captured per PC 
pca.var <- pca$sdev^2

## Percent variance is often more informative to look at 
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per
```

```{r}
barplot(pca.var.per, main="Scree Plot", 
        names.arg = paste0("PC", 1:10),
        xlab="Principal Component", ylab="Percent Variation")
```

Make it more attractive and useful:

```{r}
## A vector of colors for wt and ko samples
colvec <- colnames(rna.data)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"

plot(pca$x[,1], pca$x[,2], col=colvec, pch=16,
     xlab=paste0("PC1 (", pca.var.per[1], "%)"),
     ylab=paste0("PC2 (", pca.var.per[2], "%)"))

text(pca$x[,1], pca$x[,2], labels = colnames(rna.data), pos=c(rep(4,5), rep(2,5)))
```

> Using ggplot

```{r}
library(ggplot2)

df <- as.data.frame(pca$x)

# Our first basic plot
ggplot(df) + 
  aes(PC1, PC2) + 
  geom_point()
```

```{r}
# Add a 'wt' and 'ko' "condition" column
df$samples <- colnames(rna.data) 
df$condition <- substr(colnames(rna.data),1,2)

p <- ggplot(df) + 
        aes(PC1, PC2, label=samples, col=condition) + 
        geom_label(show.legend = FALSE)
p
```
```{r}
p + labs(title="PCA of RNASeq Data",
       subtitle = "PC1 clealy seperates wild-type from knock-out samples",
       x=paste0("PC1 (", pca.var.per[1], "%)"),
       y=paste0("PC2 (", pca.var.per[2], "%)"),
       caption="BIMM143 example data") +
     theme_bw()
```
