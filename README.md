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
data1 = cbind(XChangePRNCON = data$X1PRNCON - data$X2PRNCON, X1PAR1EMP =  data$X1PAR1EMP, P1CURMAR = data$P1CURMAR,P1NUMBRS =  data$P1NUMBRS, W12P0 = data$W12P0, W12P1 = data$W12P1, W12P2 = data$W12P2)
dim(data1)
head(data1)

# Getting rid of the missing data
data1 = na.omit(data1)
head(data1)
dim(data1)


# Need to find the replicate weights and get them into a compressed variable
# ECLS says to to use JK2, but JKn is fine for two or more.  The program suggested that I am using combined weights, which has all of the weights together, rscales just means scale the variance by 1, because there is not need for changing the variances. 

scdrep = svrepdesign(variables = data1[,1:4], type="JKn", repweights = data1[,6:7], weights = data1[,5], combined.weights = TRUE, rscales = 1)



model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)

```
Different things I tried: Tried getting rid of combined.weights still doesn't work
```{r}
scdrep = svrepdesign(variables = data1[,1:4], type="JKn", repweights = data1[,6:7], weights = data1[,5], combined.weights = TRUE, rscales = 1)

model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)

```
Tried adding a ~ to weights, but still doesn't work
```{r}

scdrep = svrepdesign(variables = data1[,1:4], type="JKn", repweights = data1[,6:7], weights =~ data1[,5], combined.weights = TRUE, rscales = 1)


model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)

```
There is something wrong with the design$variables section
```{r}
scdrep = svrepdesign(variables = data1[,1:4], type="JKn", repweights = data1[,6:7], weights = data1[,5], combined.weights = TRUE, rscales = 1)


svymean(~XChangePRNCON, scdrep)

model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)
```
Trying to see if the problem exists with the simple version with the same data naming strategy.  
```{r}
ecls = svydesign(id = ~1, weights = ~ W12P0, data = data1)

summary(ecls)

head(ecls$prob)

svymean(~P1CURMAR , ecls)

```
Trying with the data arugment instead, doesn't work
```{r}
scdrep = svrepdesign(variables = data1[,1:4], type="JKn", repweights = data1[,6:7], weights = data1[,5], combined.weights = TRUE, rscales = 1)

summary(scdrep)
head(scdrep$pweights)

svymean(~XChangePRNCON, scdrep)

model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)
```
Trying with different ways to assign the repweights and weights variables, doesn't work
```{r}
scdrep = svrepdesign(variables = data1[c("XChangePRNCON", "X1PAR1EMP", "P1CURMAR", "P1NUMBRS")], type="JKn", repweights = data1[c("W12P1", "W12P2")], weights = data1[c("W12P0")], combined.weights = TRUE, rscales = 1)

summary(scdrep)
head(scdrep$pweights)

svymean(~XChangePRNCON, scdrep)

model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)
```
Maybe go back the combined replicate weight thing
```{r}
scdrep = svrepdesign(variables = data1[,1:4], type="JKn", repweights = data1[,6:7], weights = data1[,5], combined.weights = TRUE, rscales = 1)

summary(scdrep)
head(scdrep$pweights)

test = data.frame(a = rnorm(10), b = rnorm(10))
dimnames(test)

svymean(~XChangePRNCON, scdrep)

model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)
```

```{r}

test = data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10), d = 10+rnorm(10), e = 10+rnorm(10), f = 10+rnorm(10), g = 10+rnorm(10))
dimnames(test)

# Doesn't work
scdrep = svrepdesign(variables = test[,1:3], type="JKn", repweights = test[,4:6], weights = test[,7], combined.weights = TRUE, scale = 1,  rscales = 1, rho = NULL, data = NULL)

summary(scdrep)
scdrep$scale

svymean(~c, scdrep)
mean(test$c)
sd(test$c)

model1 = svyglm(a ~ b + c, scdrep)
summary(model1)

```
This works for the mean, but not regression, added scale
```{r}
scdrep = svrepdesign(variables = test[,1:3], type="JKn", repweights = test[,4:6], weights = test[,7], combined.weights = TRUE, scale = 1,  rscales = 1)

summary(scdrep)

svymean(~a, scdrep)
mean(test$a)

model1 = svyglm(a  ~ b + c, scdrep)
```

Works but something wrong with standard errors
```{r}
scdrep = svrepdesign(variables = data1[,1:4], type="JKn", repweights = data1[,6:7], weights = data1[,5], combined.weights = FALSE, rscales = 1, scale = 1)

summary(scdrep)
head(scdrep$pweights)

svymean(~XChangePRNCON, scdrep)

model1 = svyglm(XChangePRNCON ~ X1PAR1EMP + P1CURMAR + P1NUMBRS, scdrep)
summary(model1)

```
Trying the compressed weights, but didn't work
```{r}

test = data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10), d = 10+rnorm(10), e = 10+rnorm(10), f = 10+rnorm(10), g = 10+rnorm(10), h = 10+rnorm(10), i = 10+rnorm(10), j = 10+rnorm(10), k = 10+rnorm(10))


repWeights = compressWeights(test[,4:10])

scdrep = svrepdesign(variables = test[,1:3], type="JKn", repweights = repWeights, weights = test[,11], combined.weights = TRUE, scale = 1,  rscales = 1)

summary(scdrep)

svymean(~a, scdrep)
mean(test$a)

model1 = svyglm(a  ~ b + c, scdrep)

summary(model1)
```

