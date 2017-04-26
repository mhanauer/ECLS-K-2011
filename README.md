---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Here we are setting the WD to google drive.  Then we need to subset the data and rename the variables.  Just having the variable set up in three waves, because that is what I did earlier and didn't want to redo stuff.  Need to change the -9's to NAs and then omit the NAs.
```{r}
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
data = read.csv("ELCS-K-2011.csv", header = TRUE)

data1 = cbind(X1PRNCON = data$X1PRNCON, X1PRNSOC = data$X1PRNSOC, X1PRNSAD = data$X1PRNSAD, X1PRNIMP = data$X1PRNIMP, X1PRNAPP = data$X1PRNAPP, X1BMI = data$X1BMI, X1PAR1AGE = data$X1PAR1AGE, X1PAR1EMP = data$X1PAR1EMP, X1HTOTAL = data$X1HTOTAL, X1NUMSIB = data$X1NUMSIB, X2POVTY = data$X2POVTY, X12SESL = data$X12SESL, W1P0 = data$W1P0, data[,10894:10973])

data2 = cbind(X_HISP_R = data$X_HISP_R, X_WHITE_R = data$X_WHITE_R, X_BLACK_R = data$X_BLACK_R, X_ASIAN_R = data$X_ASIAN_R, X_AMINAN_R = data$X_AMINAN_R, X_HAWPI_R = data$X_HAWPI_R, X_MULTR_R = data$X_MULTR_R, X12LANGST = data$X12LANGST,  X_CHSEX_R = data$X_CHSEX_R, X1RESREL = data$X1RESREL, X1HPARNT = data$X1HPARNT, X12PAR1ED_I = data$X12PAR1ED_I, X1PRIMNW = data$X1PRIMNW,  X1PUBPRI = data$X1PUBPRI)

data3  = cbind( X1PAR1RAC = data$X1PAR1RAC)

data1 = cbind(data2, data3, data1)

data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})

data1 = as.data.frame(data1)

data1 = na.omit(data1)

dim(data1)
```
Now we need to alter the variables to be binary in necessary.  First we create get all the variables where 1 is the interest and get those as 1 and rest as zero.  Then for parent ethnicty we change the ones to zero and everything else to one to have a non-white be one.  Then we need to grab a seperate subset of the all the remaining variables, so we don't double up on those variables when we cbind them togehter at the end.
```{r}
data2 = cbind(X_HISP_R = data1$X_HISP_R, X_WHITE_R = data1$X_WHITE_R, X_BLACK_R = data1$X_BLACK_R, X_ASIAN_R = data1$X_ASIAN_R, X_AMINAN_R = data1$X_AMINAN_R, X_HAWPI_R = data1$X_HAWPI_R, X_MULTR_R = data1$X_MULTR_R, X12LANGST = data1$X12LANGST,  X_CHSEX_R = data1$X_CHSEX_R, X1RESREL = data1$X1RESREL, X1HPARNT = data1$X1HPARNT, X12PAR1ED_I = data1$X12PAR1ED_I, X1PRIMNW = data1$X1PRIMNW,  X1PUBPRI = data1$X1PUBPRI)

data2 = apply(data2, 2, function(x){ifelse(x == 1, 1, 0)})
data2 = as.data.frame(data2)
head(data2)

# Need to change 2 through 8 to be 1 and 1 to be zero
data3 = cbind(X1PAR1RAC = data1$X1PAR1RAC)
head(data3)

data3 = apply(data3, 2, function(x){ifelse(x == 1, 0, 1)})
data3 = as.data.frame(data3)

head(data1)

data1 = cbind(X1PRNCON = data1$X1PRNCON, X1PRNSOC = data1$X1PRNSOC, X1PRNSAD = data1$X1PRNSAD, X1PRNIMP = data1$X1PRNIMP, X1PRNAPP = data1$X1PRNAPP, X1BMI = data1$X1BMI, X1PAR1AGE = data1$X1PAR1AGE, X1PAR1EMP = data1$X1PAR1EMP, X1HTOTAL = data1$X1HTOTAL, X1NUMSIB = data1$X1NUMSIB, X2POVTY = data1$X2POVTY, X12SESL = data1$X12SESL, W1P0 = data1$W1P0, data1[,29:108])

data1 = cbind(data2, data3, data1)

```
Running the actual analyses
```{r}

# Need to find the replicate weights and get them into a compressed variable
# ECLS says to to use JK2, but JKn is fine for two or more.  The program suggested that I am using combined weights, which has all of the weights together, rscales just means scale the variance by 1, because there is not need for changing the variances. 

head(data1)

scdrep = svrepdesign(data = data1, type="JKn", repweights = data1[,30:108], weights = data1[,29], combined.weights = TRUE, rscales = 1, scale = 1)



model1 = svyglm(X1PRNCON ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+ X12LANGST+ X_CHSEX_R+ X1RESREL+ X1HPARNT+ X12PAR1ED_I+ X1PRIMNW + X1PUBPRI + X1PAR1RAC + X1BMI + X1PAR1AGE + X1PAR1EMP + X1HTOTAL + X1NUMSIB + X2POVTY + X12SESL, scdrep)
summary(model1)
```

