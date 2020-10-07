---
title: "Predicting Exercise Manner"
author: "Ahmed Yahya Khaled"
date: "10/7/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Project : Practical Machine Learning 
#### Coursera

## Introduction
People do exercises and One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, our goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants and predict the manner in which they did the exercise.

The outcome variable is `classe`, a factor variable with 5 levels. For our data set, participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in 5 different fashions:

- exactly according to the specification (Class A)
- throwing the elbows to the front (Class B)
- lifting the dumbbell only halfway (Class C)
- lowering the dumbbell only halfway (Class D)
- throwing the hips to the front (Class E)

## Required Packages

These packages need to install if not done already

```{r pack1, message=FALSE, warning=FALSE, paged.print=FALSE}
# install.packages("tidyverse") # ggplot2 is encapsulated in tidyverse
# install.packages("caret")
# install.packages("randomForest")
# install.packages("rpart")
# install.packages("rpart.plot")
```

Then we need to load the packages

```{r pack2, message=FALSE, warning=FALSE, paged.print=FALSE}
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
```

## Import Dataset
links : \
training set : <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv> \
test set : <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

set working directory : 
```{r wd}
setwd("E:/My_learning/R/Practical Machine Learning _ Coursera/Project")
```

import from working directory :
```{r data}
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))

# View(training) ; View(testing)
```

### set seed for reproducibility
```{r seed}
set.seed(1213)
```


## Data Processing

### data cleaning
```{r clean}
# removing columns with NA
training <- training[ , colSums(is.na(training)) == 0]
testing <- testing[ , colSums(is.na(testing)) == 0]

# # removing irrelevant columns
training <- training[ , -c(1:7)]
testing <- testing[ , -c(1:7)]
 
dim(training) ; dim(testing)
```

## Cross Validation
For cross validation purpose splitting the `training` data into `subTraining` (75%) & `subTesting` (25%) data
```{r sub}
inTrain <- createDataPartition(y = training$classe, p = 0.75, list = F)
subTraining <- training[inTrain, ] ; subTesting <- training[-inTrain, ]
dim(subTraining) ; dim(subTesting)
```

## Data Exploration

As independent variable are many, checking only the dependent variable - `classe`

```{r expl}
ggplot(subTraining)+
  geom_bar(aes(x = classe), fill = 'lightblue') +
  ggtitle("Distribution of 'classe'") +
  theme_bw()
table(subTraining$classe)
```



# Prediction Models

## Decision Tree

### fitting model
```{r dT}
FitDt <- rpart(classe ~ . , 
               data = subTraining, method = "class")
```

### ploting trees
```{r dTpl}
rpart.plot(FitDt, main="Decision Tree")
```

### predicting value
```{r dTpr}
ypredDt <- predict(FitDt, newdata = subTesting, type = "class")
table(ypredDt, subTesting$classe)
```

### confusion matrix
```{r dTcm}
confusionMatrix(ypredDt, subTesting$classe)
```

## Random Forest

### fitting model
```{r Rf}
FitRf <- randomForest(classe ~ . ,
                      data = subTraining, methods = "class" )
```

### plotting error vs number of trees
```{r Rfpl}
plot(FitRf, main = "Error vs. no. of Trees")
```

### predicting value
```{r Rfpr}
ypredRf <- predict(FitRf, newdata = subTesting, type = "class")
table(ypredRf, subTesting$classe)
```

### confusion matrix
```{r Rfcm}
confusionMatrix(ypredRf, subTesting$classe)
```


## Model Comparison
Let's present the outcome of the two models in Tiles plot

```{r compTP}
cmDt <- as.data.frame(table(ypredDt, subTesting$classe))
cmRf <- as.data.frame(table(ypredRf, subTesting$classe))

ggplot(cmDt) +
  geom_tile(aes(x = ypredDt, y = Var2, fill = Freq)) +
  geom_text(aes(x = ypredDt, y = Var2, label = Freq), color = "yellow") +
  ggtitle("Desision Tree Prediction vs. Actual classe") +
  labs(x = "Decision Tree Prediction", y = "Actual classe") +
  theme_bw()
#/
ggplot(cmRf) +
  geom_tile(aes(x = ypredRf, y = Var2, fill = Freq)) +
  geom_text(aes(x = ypredRf, y = Var2, label = Freq), color = "yellow") +
  ggtitle("Random Forest Prediction vs. Actual classe") +
  labs(x = "Random Forest Prediction", y = "Actual classe") +
  theme_bw()
```


Apparently, Random Forest is provding better prediction here.

|Feature  |Decision Tree |Random Forest |
|---------|------------- |--------------|
|Accurqacy|     0.739    |  	0.995     |
|95% CI   |(0.719, 0.743)|(0.993, 0.997)|


## Prediction on `test set`
Applying the Random Forest model on the testing data

```{r final}
testing$classe <- predict(FitRf, newdata = testing, type = "class")
table(testing$classe)
```
