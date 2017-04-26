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
data1 = cbind(XChangePRNCON = data$X1PRNCON - data$X2PRNCON, X1PAR1EMP =  data$X1PAR1EMP, P1CURMAR = data$P1CURMAR,P1NUMBRS =  data$P1NUMBRS, W12P0 = data$W12P0, data[,11054:11133])

dim(data1)
head(data1)

# Getting rid of the missing data, and change to a data.frame
data1 = na.omit(data1)
data1 = as.data.frame(data1)
head(data1)
dim(data1)


# Need to find the replicate weights and get them into a compressed variable
# ECLS says to to use JK2, but JKn is fine for two or more.  The program suggested that I am using combined weights, which has all of the weights together, rscales just means scale the variance by 1, because there is not need for changing the variances. 

scdrep = svrepdesign(variables = data1[,1:4], type="JKn", repweights = data1[,6:85], weights = data1[,5], combined.weights = TRUE, rscales = 1, scale = 1)


model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)
summary(model1)

```
