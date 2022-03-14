
## 1. Bioconductor and DESeq2 setup

Install BioConductor and DESeq2

```{r message=FALSE, warning=FALSE}
#install.packages("BiocManager")
#BiocManager::install()
```
```{r message=FALSE, warning=FALSE}
#BiocManager::install("DESeq2")
library(BiocManager)
library(DESeq2)
```


## 2. Import countData and colData

Read in our data 

```{r}
counts <- read.csv("airway_scaledcounts.csv", row.names=1)
metadata <-  read.csv("airway_metadata.csv")
head(counts)
```

```{r}
head(metadata)

```
>Q1. How many genes are in this dataset?

```{r}
nrow(counts)
```

> ANSWER: There are 38694 genes in the dataset.

>Q2. How many ‘control’ cell lines do we have?

```{r}
sum(metadata$dex=="control")
```

> ANSWER: There are 4 control cell lines

## 3. Toy differential gene expression

Let's calculate the mean counts per gene across these samples:

```{r}
control <- metadata[metadata[,"dex"]=="control",]
control.counts <- counts[ ,control$id]
control.mean <- rowSums( control.counts )/4 
head(control.mean)
```

And do it again using the dplyr package
```{r}
#remove.packages("rlang")
#install.packages("rlang")
library(rlang)
library(dplyr)
```

```{r message=FALSE}
# install.packages("dplyr")
library(dplyr)
control <- metadata %>% filter(dex=="control")
control.counts <- counts %>% select(control$id) 
control.mean <- rowSums(control.counts)/4
head(control.mean)
```

>Q3. How would you make the above code in either approach more robust?

To calculate the mean manually, the above codes use (control.counts)/4. However, a more robust way of writing this would be to use rowMeans(), as such:

```{r}
control.mean <- rowMeans(control.counts)
head(control.mean)
```
>Q4. Follow the same procedure for the treated samples (i.e. calculate the mean per gene across drug treated samples and assign to a labeled vector called treated.mean)

```{r}
treated <- metadata[metadata[,"dex"]=="treated",]
treated.counts <- counts[ ,treated$id]
treated.mean <- rowMeans(treated.counts)
head(treated.mean)
```
Combine our meancount data for bookkeeping purposes:

```{r}
meancounts <- data.frame(control.mean, treated.mean)
head(meancounts)
```

> Q5 (a). Create a scatter plot showing the mean of the treated samples against the mean of the control samples. Your plot should look something like the following.

```{r}
plot(meancounts) + title("Treated vs Control")  
```


> Q5 (b).You could also use the ggplot2 package to make this figure producing the plot below. What geom_?() function would you use for this plot? 

> ANSWER: geom_point()

```{r}
library(ggplot2)
ggplot(meancounts,aes(control.mean,treated.mean)) +
  geom_point(alpha=0.2)
```

> Q6. Try plotting both axes on a log scale. What is the argument to plot() that allows you to do this?

> ANSWER: log


```{r}
plot(meancounts,log="xy")
```


Here we calculate log2foldchange, add it to our meancounts data.frame and inspect the results either with the head() or the View() function for example.

```{r}
meancounts$log2fc <- log2(meancounts[,"treated.mean"]/meancounts[,"control.mean"])
head(meancounts)
```

There are a couple of wird NaN or Inf points. Lets filter these out.

```{r}
zero.vals <- which(meancounts[,1:2]==0, arr.ind=TRUE)
#head(zero.vals)
to.rm <- unique(zero.vals[,1])
mycounts <- meancounts[-to.rm,]
head(mycounts)
```


> Q7. What is the purpose of the arr.ind argument in the which() function call above? Why would we then take the first column of the output and need to call the unique() function?

> ANSWER: The purpose of arr.ind argument in the which() function is to return the true row and column indices as a matrix. If arr.ind=FALSE (default), it would return the true results as as integers. We then take the first column of the output and call the unique() function because although the first line gives us rows containing zerors, some rows may have zeroes in both columns annd may be repeated. So, we'd want to focus on the unique row numbers. We ultimately want non-zero genes, so we use negative (-)to.rm to select the rows that do not have zeros.

Let's filter to see how many genes are up or down regulated

```{r}
up.ind <- mycounts$log2fc > 2
down.ind <- mycounts$log2fc < (-2)
```

 
> Q8. Using the up.ind vector above can you determine how many up regulated genes we have at the greater than 2 fc level?

```{r}
sum(up.ind)
```

> ANSWER: There are 250 genes that are up-regulated at the greater than 2 fc level.

> Q9. Using the down.ind vector above can you determine how many down regulated genes we have at the greater than 2 fc level?

```{r}
sum(down.ind)
```

> ANSWER: There are 267 genes that are down-regulated greater than the 2 fc level.

> Q10. Do you trust these results? Why or why not?

> ANSWER: Ultimately, one would have to look at another statistical test to gain greater confidence in these results. For example, a large log2 fc may occur yet be statistically insignificant. Therefore, some p-value test would be required to see if the cahnges in expression are significant. 


# 4. DESeq2 Analysis

```{r}
library(DESeq2)
citation("DESeq2")
```

Import the data

```{r}
dds <- DESeqDataSetFromMatrix(countData=counts, 
                              colData=metadata, 
                              design=~dex)
dds
```

DESeq Analysis 

```{r}
dds <- DESeq(dds)
```

get results

```{r}
res <- results(dds)
res
```

## 5. Adding annotation data

```{r}
#BiocManager::install("AnnotationDbi")
#BiocManager::install("org.Hs.eg.db")
library("AnnotationDbi")
library("org.Hs.eg.db")
```

```{r}
columns(org.Hs.eg.db)
```

```{r}
res$symbol <- mapIds(org.Hs.eg.db,
                     keys=row.names(res), # Our genenames
                     keytype="ENSEMBL",        # The format of our genenames
                     column="SYMBOL",          # The new format we want to add
                     multiVals="first")
head(res)
```

> Q11. Run the mapIds() function two more times to add the Entrez ID and UniProt accession and GENENAME as new columns called res$entrez, res$uniprot and res$genename.

```{r}
res$entrez <- mapIds(org.Hs.eg.db,
                     keys=row.names(res),
                     column="ENTREZID",
                     keytype="ENSEMBL",
                     multiVals="first")
res$uniprot <- mapIds(org.Hs.eg.db,
                     keys=row.names(res),
                     column="UNIPROT",
                     keytype="ENSEMBL",
                     multiVals="first")
res$genename <- mapIds(org.Hs.eg.db,
                     keys=row.names(res),
                     column="GENENAME",
                     keytype="ENSEMBL",
                     multiVals="first")
```

```{r}
ord <- order( res$padj )
#View(res[ord,])
head(res[ord,])
```

Let's write out the ordered significant results with annotations:

```{r}
write.csv(res[ord,], "deseq_results.csv")
```

## 6. Data Visualization


Volcano plot! 

```{r}
plot( res$log2FoldChange,  -log(res$padj), 
      xlab="Log2(FoldChange)",
      ylab="-Log(P-value)")
```
```{r}
plot( res$log2FoldChange,  -log(res$padj), 
      xlab="Log2(FoldChange)",
      ylab="-Log(P-value)")
```
```{r}
# Setup our custom point color vector 
mycols <- rep("gray", nrow(res))
mycols[ abs(res$log2FoldChange) > 2 ]  <- "red" 

inds <- (res$padj < 0.01) & (abs(res$log2FoldChange) > 2 )
mycols[ inds ] <- "blue"

# Volcano plot with custom colors 
plot( res$log2FoldChange,  -log(res$padj), 
 col=mycols, ylab="-Log(P-value)", xlab="Log2(FoldChange)" )

# Cut-off lines
abline(v=c(-2,2), col="gray", lty=2)
abline(h=-log(0.1), col="gray", lty=2)
```

```{r}
BiocManager::install("EnhancedVolcano")
library(EnhancedVolcano)
```

```{r}
x <- as.data.frame(res)

EnhancedVolcano(x,
    lab = x$symbol,
    x = 'log2FoldChange',
    y = 'pvalue')
```

## 7. Pathway Analysis

```{r}
library(pathview)
library(gage)
library(gageData)

data(kegg.sets.hs)

# Examine the first 2 pathways in this kegg set for humans
head(kegg.sets.hs, 2)
```
```{r}
foldchanges = res$log2FoldChange
names(foldchanges) = res$entrez
head(foldchanges)
```

```{r}
# Get the results
keggres = gage(foldchanges, gsets=kegg.sets.hs)
```

```{r}
attributes(keggres)
```

```{r}
# Look at the first three down (less) pathways
head(keggres$less, 3)

```

```{r}
pathview(gene.data=foldchanges, pathway.id="hsa05310")
```

```{r}
# A different PDF based output of the same data
pathview(gene.data=foldchanges, pathway.id="hsa05310", kegg.native=FALSE)
```
