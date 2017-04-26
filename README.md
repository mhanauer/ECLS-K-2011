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
data1 = cbind(X1PRNCON = data$X1PRNCON, X1PRNSOC = data$X1PRNSOC, X1PRNSAD = data$X1PRNSAD, X1PRNIMP = data$X1PRNIMP, X1PRNAPP = data$X1PRNAPP, X_HISP_R = data$X_HISP_R, X_WHITE_R = data$X_WHITE_R, X_BLACK_R = data$X_BLACK_R, X_ASIAN_R = data$X_ASIAN_R, X_AMINAN_R = data$X_AMINAN_R, X_HAWPI_R = data$X_HAWPI_R, X_MULTR_R = data$X_MULTR_R, X12LANGST = data$X12LANGST, X1BMI = data$X1BMI, X_CHSEX_R = data$X_CHSEX_R, X1RESREL = data$X1RESREL, X1HPARNT = data$X1HPARNT, X1PAR1AGE = data$X1PAR1AGE, X1PAR1RAC = data$X1PAR1RAC, X12PAR1ED_I = data$X12PAR1ED_I, X1PAR1EMP = data$X1PAR1EMP, X1HTOTAL = data$X1HTOTAL, X1NUMSIB = data$X1NUMSIB, X1PRIMNW = data$X1PRIMNW, X2POVTY = data$X2POVTY, X12SESL = data$X12SESL, X1PUBPRI = data$X1PUBPRI, F1CLASS = data$F1CLASS, W12P0 = data$W12P0, data[,11054:11133])

dim(data1)
head(data1)





```
Running the actual analyses
```{r}
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

