---
title: 'Week 5: R Functions'
author: "Meg Robinson"
date: "2/5/2022"
output: word_document
---

> Q1. Write a function grade() to determine an overall grade from a vector of student homework
assignment scores dropping the lowest single score. If a student misses a homework (i.e. has an
NA value) this can be used as a score to be potentially dropped. Your final function should be
adquately explained with code comments and be able to work on an example class gradebook
such as this one in CSV format: “https://tinyurl.com/gradeinput” [3pts]

```{r}
# Example input vectors to start with
student1 <- c(100, 100, 100, 100, 100, 100, 100, 90)
student2 <- c(100, NA, 90, 90, 90, 90, 97, 80)
student3 <- c(90, NA, NA, NA, NA, NA, NA, NA)

```


Average of student 1
```{r}
student1
mean(student1)
```
Use 'min()' to find lowest score
```{r}
min(student1)
```

Find index at which min occured using which.min()

```{r}
which.min(student1)
```

Get everything except lowest score using minus ("-") and caclulate mean (as long as no "NA" in vector)

```{r}
mean(student1[-which.min(student1)])
```
Try it on student 2
```{r}
mean(student2[-which.min(student1)])
```
It does not work because mean() function doesn't allow for **NA** values. Now find **NA** values

```{r}
is.na(student2)
```
Replace *NA* with zero
```{r}
student.prime = student2
student.prime[is.na(student.prime)] = 0
student.prime
```

Now get mean()

```{r}
mean(student.prime[-which.min(student.prime)])
```
Which we can see is the value of *student2*
```{r}
mean(c(100,90,90,90,90,97,80))
```
So now do the above with *student3*

```{r}
x = student3
x[is.na(x)] = 0
mean(x[-which.min(x)])

```
So we can write our function
```{r}
#' Calculate avg scores for a vector hw socres
#' Drop lowest homework score
#' Missing values treated as 0
#'
#' @param x Numeric vector of homework scores
#'
#' @return average score
#' @export
#'
#' @examples
#' student = c(100,NA,90, 80)
#' grade(student)
#' 
grade = function(x){
  # Map NA missing hw vals to 0
  # Assign hw scores 0
  x[is.na(x)] = 0
  # Drop the lowest score
  mean(x[-which.min(x)])
}
```
And use it

```{r}
grade(student1)
grade(student2)
grade(student3)
```

### Now grade entire class
```{r}
url = "https://tinyurl.com/gradeinput"
gradebook = read.csv(url, row.names=1)
gradebook
```
Use **apply()** to grade all of the students using our **grade()** function

```{r}
apply(gradebook,1,grade)
```
> Q2. Using your grade() function and the supplied gradebook, Who is the top scoring student
overall in the gradebook? [3pts]


```{r}
finalgrades <- apply(gradebook,1,grade)
which.max(finalgrades)
max(finalgrades)
```
**student 18 is the top scoring student with an average score of 94.5**

> Q3. From your analysis of the gradebook, which homework was toughest on students (i.e. obtained
the lowest scores overall? [2pts

Average hw's
```{r}
hwavg = apply(gradebook,2,mean, na.rm=TRUE)
which.min(hwavg)
```
Median of hw's
```{r}
hwmed = apply(gradebook,2,median, na.rm=TRUE)
which.min(hwmed)
```
Since results were different look at plot of gradebook
```{r}
boxplot(gradebook)
```
It looks like **homework 2** is the toughest.

> Q4. Optional Extension: From your analysis of the gradebook, which homework was most
predictive of overall score (i.e. highest correlation with average grade score)? [1pt]

Use the **cor()** function
```{r}
gradebook[is.na(gradebook)] = 0
cor(finalgrades, gradebook$hw1)
```

```{r}
apply(gradebook, 2, cor, x=finalgrades)
```

**Homework 5** was most predictive of a studen't overall score
