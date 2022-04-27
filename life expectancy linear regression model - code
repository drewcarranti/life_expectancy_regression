---
title: "final project"
author: "drew carranti"
date: "4/20/2022"
output: pdf_document
---

#we started
```{r}
library(leaps)
data = read.csv('LifeExp.csv')
data$Country = factor(data$Country)
data$Year = factor(data$Year)

set.seed(1)
index = sample(1:nrow(data), 100, replace=F)
train = data[-index,]
test = data[index,]
```


```{r}
subsets = regsubsets(LifeExpectancy ~ LifeExpectancy + AdultMortality + infantDeaths + Alcohol + percentageExpenditure + HepatitisB + Measles + BMI + underfiveDeaths + Polio + TotalExpenditure + Diphtheria + HIVAIDS + GDP + Population + thinness119Years + thinness59Years + IncomeCompositionOfResources + Schooling, data = data)
summary(subsets)
adjr2 = summary(subsets)$adj
best = which.max(adjr2)
best
```


```{r}

#summary(model)
## adding the year also helped
# MSE Testing
test_pred = predict(model, test)
test_RSS = sum((test_pred-test$LifeExpectancy)^2)
test_MSE = train_RSS / nrow(test)
test_MSE

model = lm(LifeExpectancy ~ Schooling + Country + HIVAIDS + BMI + IncomeCompositionOfResources + TotalExpenditure , data = train)
train_pred = predict(model, test)
train_RSS = sum((train_pred-test$LifeExpectancy)^2)
train_MSE = train_RSS / nrow(test)
train_MSE


model = lm(LifeExpectancy ~ Schooling + Country + GDP + Measles + HIVAIDS + BMI + IncomeCompositionOfResources + TotalExpenditure , data = train)
train_pred = predict(model, test)
train_RSS = sum((train_pred-test$LifeExpectancy)^2)
train_MSE = train_RSS / nrow(test)
train_MSE

```

```{r}
library(leaps)
data = read.csv('LifeExp.csv')
data$Country = factor(data$Country)
data$Year = factor(data$Year)

model = lm(LifeExpectancy ~ AdultMortality + infantDeaths + percentageExpenditure + BMI + underfiveDeaths + HIVAIDS + IncomeCompositionOfResources + Schooling + Country + Year, data = data)





save(model, file = 'model.Rdata' )
```




```{r}
load('model.RData')
y_hat = predict(model, train)
```

