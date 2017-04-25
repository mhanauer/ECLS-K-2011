# ECLS-K-2011
---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Here we are setting the WD to google drive.  Then we need to subset the data and rename the variables.  Then because the format is not correct reload the data.
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
data = read.csv("ELCS-K-2011.csv", header = TRUE)

# Get the data subsetted
data1 = cbind(data$X1PRNCON, data$X2PRNCON, data$X1PAR1EMP, data$P1CURMAR, data$P1NUMBRS, data$W12P0)
dim(data1)
head(data1)
colnames(data1) = c("X1PRNCON", "X2PRNCON", "X1PAR1EMP", "P1CURMAR", "P1NUMBRS", "W12P0")
head(data1)

#Getting rid of the missing data
data1 = na.omit(data1)
head(data1)
dim(data1)
data1 = as.data.frame(data1)

ecls = svydesign(id = ~1, weights = ~ W12P0, data = data1)
svymean(~P1CURMAR , ecls)

```
