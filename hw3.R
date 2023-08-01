---
title: "HW3_STAT515_Mykola_Signayevskyy"
author: "Mykola Signayesvkyy"
date: "2023-04-27"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Problem 1: Logistic Regression

This question should be answered using the “Banknote Authentication” data set. Description about the data set can be found on the link provided. Objective of this question is to fit an logistic regression model to classify forged banknote from genuine banknotes. (Presumably 0 for genuine and 1 for forged bank notes)

```{r}
banknote <- read.table("/Users/mykola/Desktop/STAT515/hw3/banknote_authentication(1).txt", header=TRUE, sep=",")
head(banknote)
```
```{r}
any(is.na(banknote))
```


### Produce some numerical and graphical summaries of the data set. Explain the relationships.

```{r}
summary(banknote)
```

```{r}
library(ggplot2)
ggplot(banknote, aes(x=class, y=Variance, fill=as.factor(class))) + 
  geom_boxplot() +
  labs(title="Variance by Class", x="Class", y="Variance") 

```
```{r}
ggplot(banknote, aes(x=class, y=skewness, fill=as.factor(class))) +
  geom_boxplot() +
  labs(title = "Skewness by Class", x='Class', y='Skewness') 
```
```{r}
ggplot(banknote, aes(x=class, y=curtosis, fill=as.factor(class))) +
  geom_boxplot() +
  labs(title = 'Curtosis by Class', x="Class", y="Curtosis") 
```

```{r}
ggplot(banknote, aes(x=class, y=entropy, fill=as.factor(class))) +
  geom_boxplot() +
  labs(title = "Entropy by Class", x="Class", y="Entropy") 
```
```{r}
library(ggcorrplot)

# I am computing the correlation matrix
corr_matrix <- cor(banknote[, 1:5])

ggcorrplot(corr_matrix, 
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("BLUE", "WHITE", "RED"), 
           title = "Correlation Matrix Heatmap")
```

Variance has the biggest negative relationship for forged banknotes. Also I can see that variance has positive correlations with skewness and entropy. 

Skewness has negative relationship for forged banknotes, but not that significant as Variance. Skewness has a very big negative correlation with curtosis and smaller negative correlations with entropy. 

Curtosis has a small positive correlations with class. Also, curtosis has a very big negative correlation with Skewness. 

Entropy does not have significant relationship with class. It has some positive correlations with variance and curtosis, negative correlation with skewness. 



### Is this a balanced data set?.
```{r}
table(banknote$class)
```
I see that there are more genuine banknotes than forged ones. I would say that the difference is not that huge, but the dataset is not balanced. 


### Use the full data set to perform a logistic regression with Class as the response variable. Do any of the predictors appear to be statistically significant? If so, which ones?
```{r}
logit_model <- glm(class ~ ., data = banknote, family = binomial)
summary(logit_model)
```
All of the variables are statistically significant except entropy. So Variance, skewness, and curtosis




### Compute the confusion matrix and overall fraction of correct predictions. Explain what the confusion matrix id telling you about the types of mistakes made by logistic regression.

```{r}
#confusion matrix
predicted_class <- ifelse(predict(logit_model, type = "response") > 0.5, 1, 0)
confusion_matrix <- table(predicted_class, banknote$class)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
confusion_matrix
accuracy
```
I see that there are 757 True negatives and 5 False negatives. Meaning that out of 762 genuine banknotes, 757 were correctly classified while 5 are mistakenly were considered as fakes. 
Also, 604 banknotes were correctly classified as forged banknotes (True positives) and only 6 were mistakenly classified as genuine (False poisitives).


```{r}
accuracy
```
Accuracy is very high. 



### Create a training set with 80% of the observations, and a testing set containing the remaining 20%.Compute the confusion matrix and the overall fraction of correct prediction for the testing data set.



```{r}
set.seed(123)
train_index <- sample(nrow(banknote), round(0.8 * nrow(banknote)))
train <- banknote[train_index, ]
test <- banknote[-train_index, ]
```

```{r}
model <- glm(class ~ ., family = binomial, data = train)
test$predicted <- ifelse(predict(model, test, type = "response") > 0.5, "Real", "Fake")
table(test$class, test$predicted)

```
```{r}
ggplot(banknote, aes(x = Variance, y = class)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  xlab("Variance") +
  ylab("Class")
```
```{r}
ggplot(banknote, aes(x = skewness, y = class)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  xlab("Skewness") +
  ylab("Class")
```

```{r}
sum(diag(table(test$class, test$predicted))) / nrow(test)
```



# Problem 2: Tree based models

## This question should be answered using the “Wine Quality” data set. Description about the data set can be found on the link provided. Objective of this question is to fit an regression tree model to predict quality of wine.

```{r}
wine <- read.csv("/Users/mykola/Desktop/STAT515/hw3/winequality(1).csv", header=TRUE, sep=";")
head(wine)
```
```{r}
any(is.na(wine))
```


### Produce some numerical and graphical summaries of the data set. Explain the relationships.
```{r}
summary(wine)
```
I see that some variables are in different numerical scales, but as we use tree based model we can avoid normalizaton of the dataset. 

```{r}
library(ggcorrplot)
# I am computing the correlation matrix
corr_matrix_w <- cor(wine[, 1:12])

ggcorrplot(corr_matrix_w, 
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("BLUE", "#FFFFFF", "RED"), 
           title = "Correlation Matrix Heatmap")
```

I see that alcohol has highest positive relationship with quality. Also, density has some negative relationship with quality. 
I can also see some interesting correlations between residual sugar and density (positive), alcohol and density (negative), free and total dioxide (positive). 

```{r}
ggplot(wine, aes(x = quality, y = alcohol, fill=as.factor(quality))) +
  geom_boxplot() +
  labs(title = "Alcohol vs. Quality")
```
I plotted alcohol to quality relationship to have a better obsirvation of this relathionship. 

```{r}
ggplot(wine, aes(x = quality, y = residual.sugar)) +
  geom_point() +
  labs(title = "Residual Sugar vs. Quality")
```

```{r}
ggplot(wine, aes(x = quality, y = density, fill=as.factor(quality))) +
  geom_boxplot() +
  labs(title = "Density vs. Quality")
```



### Create a training set with 80% of the observations, and a testing set containing the remaining 20%.
```{r}
library(caret)
set.seed(123) 

index <- createDataPartition(wine$quality, p = 0.8, list = FALSE)
w_train <- wine[index, ]

w_test <- wine[-index, ]
```



### Fit a regression tree with quality as the response variable using the training set. Plot the tree and interpret the results. What test MSE do you obtain?

```{r}
library(rpart)
library(rpart.plot)
library(rattle)
```
```{r}
wine_tree <- rpart(quality ~ ., data = w_train)
```
```{r}
fancyRpartPlot(wine_tree)
```
```{r}
#to get MSA I am going to apple model to test dataset to see actual error 
wine_pred <- predict(wine_tree, newdata = w_test)

test_mse <- mean((w_test$quality - wine_pred)^2)
test_mse
```
MSE is 0.5924379

### Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test MSE?

```{r}
library(tree)

set.seed(123)
train_index <- sample(nrow(wine), nrow(wine)*0.8)
wine_train <- wine[train_index, ]
wine_test <- wine[-train_index, ]
wine_tree2 <- tree(quality ~ ., data = wine_train)

```
```{r}
str(wine_test)
```
```{r}
summary(wine_tree2)
```
```{r}
wine_tree <- tree(quality ~ ., data = w_train)
cv.wine <- cv.tree(wine_tree)

plot(cv.wine$size,cv.wine$dev,type='b')
```

```{r}
best.size <- cv.wine$size[which.min(cv.wine$dev)]
best.size
```



```{r}
pruned_wtree <- prune.tree(wine_tree, best=5)
str(pruned_wtree)
```

```{r}
wine_test_pred3 <- predict(pruned_wtree, newdata=wine_test)
```

```{r}
mse2 <- mean((wine_test$quality - wine_test_pred3)^2)
mse2
```
accuracy is almost same as it was before pruning, but now I have less fewer nodes so it is easier to explain how model works. 

### Use random forests to analyze this data. What test MSE do you obtain?
```{r}
library(randomForest)
```
```{r}
set.seed(123)

rf_train_indices <- sample(nrow(wine), 0.8 * nrow(wine))
rf_train_data <- wine[rf_train_indices, ]
rf_test_data <- wine[-rf_train_indices, ]
```

```{r}
rf_wine <- randomForest(quality ~ ., data = rf_train_data)
```
```{r}
rf_pred <- predict(rf_wine, newdata = rf_test_data)
rf_mse <- mean((rf_test_data$quality - rf_pred)^2)
```

```{r}
plot(rf_wine)
```
```{r}
summary(rf_wine)
```
```{r}
rf_mse
```
New test MSE for random forest is much lower than I had with just a one tree model. 


### Use the importance() function to determine which variables are most important.
```{r}
importance(rf_wine)
```
The variable alcohol has the greatest relevance rating which is around 500 . Second highest relevant rating variable  I can specify is the density. Also I can highlight volatile.acidity and free.sulfur.dioxide. The alcohol variable is the most significant one for my random forest model.

