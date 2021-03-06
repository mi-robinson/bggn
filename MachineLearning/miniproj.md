
## Preparing the data

```{r}
# Save your input data file into your Project directory
fna.data <- "WisconsinCancer.csv"

# Complete the following code to input the data and store as wisc.df
wisc.df <- read.csv(fna.data, row.names=1)
head(wisc.df)
```
```{r}
# We can use -1 here to remove the first column
wisc.data <- wisc.df[,-1]
diagnosis <- as.factor(wisc.df[,1])
```

> Q1: How many observations are in the dataset?

```{r}
nrow(wisc.data)
```
There are 569 observations in the dataset.

> Q2: How many of the observations have a malignant diagnosis?

```{r}
table(diagnosis)
```

There are 212 observations with a malignant diagnosis.

> Q3: How many variables/ features in the dataset are suffixed with _mean?

```{r}
length(grep("_mean", colnames(wisc.data)))
```

There are 10 variables/features in the data suffixed with "_mean"

> Performing PCA

The next step in your analysis is to perform principal component analysis (PCA) on wisc.data

```{r}
# Check column means and standard deviations
wisc.data = wisc.data[1:(length(wisc.data)-1)]
colMeans(wisc.data)
```
```{r}
apply(wisc.data,2,sd)
```



```{r}
# Perform PCA on wisc.data by completing the following code. Use scale=TRUE since input variables after different means

wisc.pr <- prcomp(wisc.data, scale=TRUE)
summary(wisc.pr)

```
> Q4: From your results, what proportion of the original variance is captured by PC1?

```{r}
summary(wisc.pr)$importance[2,]
```

44.272% of the original variance is captured by PC1.

> Q5. How many principal components (PCs) are required to describe at least 70% of the original variance in the data?

```{r}
summary(wisc.pr)$importance[3,]
```
PC1-PC3 (3 PC's) describe at least 70% of the original variance. Specifically, they describe 72.636%.

> Q6. How many principal components (PCs) are required to describe at least 90% of the original variance in the data?

We can see from above that 7 PC's are required to describe at least 90% of the original vairance. Specifically, PC1 through PC7 describe 91.010%. 

## Interpreting PCA results

Create a biplot.

```{r}
biplot(wisc.pr)
```

> Q7. What stands out to you about this plot? Is it easy or difficult to understand? Why?

This is a very messy plot and it is hard to gather important features from this. It is not helpful when determining PCA results. 

Now let's do a scatter plot:

```{r}
# Scatter plot observations by components 1 and 2
plot( wisc.pr$x[,1:2] , col = diagnosis , 
     xlab = "PC1", ylab = "PC2")
```

> Q8. Generate a similar plot for principal components 1 and 3. What do you notice about these plots?

```{r}
# Repeat for components 1 and 3
plot(wisc.pr$x[, 1 ], wisc.pr$x[,3], col = diagnosis, 
     xlab = "PC1", ylab = "PC3")
```
**\textcolor{blue}{These graphs are similar as PC1 accounts for the majority of variance in both comparisons, so the distribution of points lies mostly along the x-axis. Since PC2 describes more variance than PC3, the first plot has more of a spread between points compared to PC1 vs PC3 graph. In both plots, however, they capture the separation between benign and malignant samples through their clustering.}**

## Using ggplot for analysis of PCA

```{r}
# Create a data.frame for ggplot
df <- as.data.frame(wisc.pr$x)
df$diagnosis <- diagnosis

# Load the ggplot2 package
library(ggplot2)

# Make a scatter plot colored by diagnosis
ggplot(df) + 
  aes(PC1, PC2, col=diagnosis) + 
  geom_point()
```

## Variance explained 

```{r}
# calculate variance
pr.var = wisc.pr$sdev^2
head(pr.var)
```

```{r}
# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "o")
```
The significant drop of the curve occurs around PC3-4. After this, additional PC's do little to improve the amount of variance explained. Now let's view this as a barplot:

```{r}
# Alternative scree plot of the same data, note data driven y-axis
barplot(pve, ylab = "Precent of Variance Explained",
     names.arg=paste0("PC",1:length(pve)), las=2, axes = FALSE)
axis(2, at=pve, labels=round(pve,2)*100 )
```
## Communicating PCA results

> Q9. For the first principal component, what is the component of the loading vector (i.e. wisc.pr$rotation[,1]) for the feature concave.points_mean?


```{r}
wisc.pr$rotation[,1]
```

The concave.points_mean is the feature with the highest absolute value, with loading vector -0.26085. This contributes the most to the first PC.

> Q10. What is the minimum number of principal components required to explain 80% of the variance of the data?

```{r}
summary(wisc.pr)$importance[3,]
```
So we can see 5 PC's describe at least 80% of the original variance in the data. Specifically, PC1-PC5 describe 84.734%. 
