---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Here we are setting the WD to google drive.  Then we need to subset the data and rename the variables.  Then because the format is not correct reload the data.
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
data = read.csv("ELCS-K-2011.csv", header = TRUE)

# Get the data subsetted.  Creating the change variable, which is the difference between the two social control variables.  
data1 = cbind(XChangePRNCON = data$X1PRNCON - data$X2PRNCON, X1PAR1EMP =  data$X1PAR1EMP, P1CURMAR = data$P1CURMAR,P1NUMBRS =  data$P1NUMBRS, W12P0 = data$W12P0)
dim(data1)
head(data1)

#Getting rid of the missing data
data1 = na.omit(data1)
head(data1)
dim(data1)
data1 = as.data.frame(data1)

# Not right, but does work 
ecls = svydesign(id = ~1, weights = ~ W12P0, data = data1)
svymean(~P1CURMAR , ecls)

# Need to find the replicate weights and get them into a compressed variable


repweights = compressWeights(c(data$W12P0, data$W12P1, data$W12P2))

scdrep = svrepdesign(type="JKn", repweights=repweights)

```


