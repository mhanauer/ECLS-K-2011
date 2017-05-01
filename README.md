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
data1 = cbind(X1PRNCON = data$X1PRNCON, X1PRNSOC = data$X1PRNSOC, X1PRNSAD = data$X1PRNSAD, X1PRNIMP = data$X1PRNIMP, X1PRNAPP = data$X1PRNAPP, X1BMI = data$X1BMI, X1PAR1AGE = data$X1PAR1AGE, X1PAR1EMP = data$X1PAR1EMP, X1HTOTAL = data$X1HTOTAL, X1NUMSIB = data$X1NUMSIB, X2POVTY = data$X2POVTY, X12SESL = data$X12SESL, X12PAR1ED_I = data$X12PAR1ED_I, X12LANGST = data$X12LANGST,  X_CHSEX_R = data$X_CHSEX_R, X1RESREL = data$X1RESREL, X1HPARNT = data$X1HPARNT, X1PRIMNW = data$X1PRIMNW, W1P0 = data$W1P0, data[,10894:10973])


#data2 = cbind(X_HISP_R = data$X_HISP_R, X_WHITE_R = data$X_WHITE_R, X_BLACK_R = data$X_BLACK_R, X_ASIAN_R = data$X_ASIAN_R, X_AMINAN_R = data$X_AMINAN_R, X_HAWPI_R = data$X_HAWPI_R, X_MULTR_R = data$X_MULTR_R, X12LANGST = data$X12LANGST,  X_CHSEX_R = data$X_CHSEX_R, X1RESREL = data$X1RESREL, X1HPARNT = data$X1HPARNT, X1PRIMNW = data$X1PRIMNW)

data2 = cbind(X_RACETHP_R = data$X_RACETHP_R)

data3  = cbind( X1PAR1RAC = data$X1PAR1RAC)

#Combining all three see we can get rid of the NA's
data1 = cbind(data2, data3, data1)

data1 = apply(data1, 2, function(x){ifelse(x == -9, NA, x)})

data1 = as.data.frame(data1)

data1 = na.omit(data1)

dim(data1)

```
Now we need to alter the variables to be binary in necessary.  First we create get all the variables where 1 is the interest and get those as 1 and rest as zero.  Then for parent ethnicty we change the ones to zero and everything else to one to have a non-white be one.  Then we need to grab a seperate subset of the all the remaining variables, so we don't double up on those variables when we cbind them togehter at the end.  I then needed to get the variables in the same order and rename with meaningful names.  Need to grab the correct data from data sets two and three, because those have the binary transformations.
```{r}

# First we need to get the orignal data2's back, because those are the ones that we need to create binary variables with.  The same with data3, but need to create a unique variable for that one.
data2 = cbind(X12LANGST = data1$X12LANGST,  X_CHSEX_R = data1$X_CHSEX_R, X1RESREL = data1$X1RESREL, X1HPARNT = data1$X1HPARNT, X1PRIMNW = data1$X1PRIMNW)

data2 = apply(data2, 2, function(x){ifelse(x == 1, 1, 0)})
data2 = as.data.frame(data2)
head(data2)

# Need to change 2 through 8 to be 1 and 1 to be zero
data3 = cbind(X1PAR1RAC = data1$X1PAR1RAC)
data3 = apply(data3, 2, function(x){ifelse(x == 1, 0, 1)})
data3 = as.data.frame(data3)

# Here is the ethnicity variable that is transformed into the original variables that you used.
data4  = cbind(X_RACETHP_R = data1$X_RACETHP_R)
data4 = as.data.frame(data4)
head(data4)
X_HISP_R = apply(data4, 2, function(x){ifelse(x == 3, 1, 0)})
X_HISP_R = as.data.frame(X_HISP_R)
names(X_HISP_R) = c("X_HISP_R")

X_WHITE_R = apply(data4, 2, function(x){ifelse(x == 1, 1, 0)})
X_WHITE_R = as.data.frame(X_WHITE_R)
names(X_WHITE_R) = c("X_WHITE_R")

X_BLACK_R = apply(data4, 2, function(x){ifelse(x == 2, 1, 0)})
X_BLACK_R = as.data.frame(X_BLACK_R)
names(X_BLACK_R) = c("X_BLACK_R")

X_ASIAN_R = apply(data4, 2, function(x){ifelse(x == 5, 1, 0)})
X_ASIAN_R = as.data.frame(X_ASIAN_R)
names(X_ASIAN_R) = c("X_ASIAN_R")

X_AMINAN_R = apply(data4, 2, function(x){ifelse(x == 7, 1, 0)})
X_AMINAN_R = as.data.frame(X_AMINAN_R)
names(X_AMINAN_R) = c("X_AMINAN_R")

X_HAWPI_R = apply(data4, 2, function(x){ifelse(x == 6, 1, 0)})
X_HAWPI_R = as.data.frame(X_HAWPI_R)
names(X_HAWPI_R) = c("X_HAWPI_R")

X_MULTR_R = apply(data4, 2, function(x){ifelse(x == 8, 1, 0)})
X_MULTR_R = as.data.frame(X_MULTR_R)
names(X_MULTR_R) = c("X_MULTR_R")

data4 = cbind(X_HISP_R, X_WHITE_R, X_BLACK_R, X_ASIAN_R, X_AMINAN_R, X_AMINAN_R, X_HAWPI_R, X_MULTR_R)
data4 = as.data.frame(data4)
head(data4)

```


Reording the variables to be in the correct order.  Grab each variable from the correct data set from above.
```{r}
# Rearrange and then rename variables to get them in the correct order.  This includes getting data2 and data3 into the correct order as well, because you need to rename all of these variables. 

data5 = cbind(X1PRNCON = data1$X1PRNCON, X1PRNSOC = data1$X1PRNSOC, X1PRNSAD = data1$X1PRNSAD, X1PRNIMP = data1$X1PRNIMP, X1PRNAPP = data1$X1PRNAPP,X_HISP_R = data4$X_HISP_R, X_WHITE_R = data4$X_WHITE_R, X_BLACK_R = data4$X_BLACK_R, X_ASIAN_R = data4$X_ASIAN_R, X_AMINAN_R = data4$X_AMINAN_R, X_HAWPI_R = data4$X_HAWPI_R, X_MULTR_R = data4$X_MULTR_R, X_CHSEX_R = data2$X_CHSEX_R, X1BMI = data1$X1BMI,X1RESREL = data2$X1RESREL,X1HPARNT = data2$X1HPARNT, X1PAR1AGE = data1$X1PAR1AGE, X1PAR1RAC = data3$X1PAR1RAC,X12PAR1ED_I = data1$X12PAR1ED_I, X1PAR1EMP = data1$X1PAR1EMP,X12LANGST = data2$X12LANGST, X1HTOTAL = data1$X1HTOTAL, X1NUMSIB = data1$X1NUMSIB, X1PRIMNW = data2$X1PRIMNW, X2POVTY = data1$X2POVTY, X12SESL = data1$X12SESL)
data5 = as.data.frame(data5)

head(data5)

#data4 = rename(data4, c("X1PRNCON" = "Self Control","X1PRNSOC" = "Social Interaction","X1PRNSAD" =  "Sad / Lonely", "X1PRNIMP" = "Impulsive / Overactive","X1PRNAPP" =  "Approaches to Learning", "X_HISP_R" = "Hispanic", "X_WHITE_R" = "White", "X_BLACK_R" = "African American", "X_ASIAN_R" =  "Asian", "X_AMINAN_R" = "American Indian / Alaska Native","X_HAWPI_R" = "Native Hawiian / Pacific Islander", "X_MULTR_R" = "Multiracial", "X_CHSEX_R" = "Child gender", "X1BMI" = "Child BMI", "X1RESREL" = "Relationship of respondant to child ", "X1HPARNT" = "Parental martial status", "X1PAR1AGE" = "Parent age", "X1PAR1RAC" = "Parent's ethnicity", "X12PAR1ED_I" = "Parent's education level", "X1PAR1EMP" = "Parent employment status", "X12LANGST" = "Home lanuage non-English", "X1HTOTAL" = "Number of household members", "X1NUMSIB" = "Number of siblings in household", "X1PRIMNW" = "Relative provides daycare", "X2POVTY" = "Poverty level", "X12SESL" = "SES")

data1 = as.data.frame(data1)
head(data1)

data1 = cbind(data5, W1P0 = data1$W1P0, data1[,22:101])
head(data1)

write.csv(data1, "data1.csv")
```


Running the actual analyses
```{r}

# Need to find the replicate weights and get them into a compressed variable
# ECLS says to to use JK2, but JKn is fine for two or more.  The program suggested that I am using combined weights, which has all of the weights together, rscales just means scale the variance by 1, because there is not need for changing the variances. 

dim(data1)
setwd("~/Google Drive/PARCS/Projects/ECLSK2011/Data")
data1 = read.csv("data1.csv", header = TRUE)
library(survey)
head(data1)
scdrep = svrepdesign(data = data1, type="JKn", repweights = data1[,27:106], weights = data1[,27], combined.weights = TRUE, rscales = 1, scale = 1)

# Grab descriptives
svymean(data1, scdrep)

modelSC = svyglm(X1PRNCON ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelSC)

modelSI = svyglm(X1PRNSOC ~X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelSI)


modelSL = svyglm(X1PRNSAD ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelSL)

modelIO = svyglm(X1PRNIMP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelIO)

modelAP = svyglm(X1PRNAPP ~ X_HISP_R + X_WHITE_R + X_BLACK_R + X_ASIAN_R+ X_AMINAN_R+ X_HAWPI_R+ X_MULTR_R+  X_CHSEX_R+ + X1BMI + X1RESREL + X1HPARNT + X1PAR1AGE  + X1PAR1RAC  + X12PAR1ED_I  + X1PAR1EMP + X12LANGST + X1HTOTAL + X1NUMSIB +  X1PRIMNW  + X2POVTY + X12SESL, scdrep)

summary(modelAP)
```

