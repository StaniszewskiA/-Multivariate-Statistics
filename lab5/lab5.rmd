---
title: "lab5"
output: html_document
date: "2024-06-13"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r packages}
install.packages('tree')
```

```{r cars}
library(tree)

energy_data <- read.csv("energy_dataset.csv")
print(energy_data)

energy_data_clean <- na.omit(energy_data)
```
```{r quantiles}
breaks <- quantile(energy_data$GHG_Emission_Reduction_tCO2e, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
energy_data$reduction <- cut(energy_data$GHG_Emission_Reduction_tCO2e, breaks = breaks, labels = c("Class 1", "Class 2", "Class 3", "Class 4"))


energy_dataH <- data.frame(energy_data, reduction = as.factor(energy_data$reduction))

print(energy_dataH)
```


```{r}
reduction_height_tree <- tree(reduction ~ . -Air_Pollution_Reduction_Index, data = energy_dataH, mincut = 8)
summary(reduction_height_tree)
```

```{r}
plot(reduction_height_tree)
text(reduction_height_tree, pretty = 0)
```

```{r}
set.seed(1)
n <- nrow(energy_dataH)
train <- sample(n, n / 2)
test <- -train
reduction_height_tree <- tree(reduction ~ . - Air_Pollution_Reduction_Index, data = energy_dataH, subset = train)
tree_class <- predict(reduction_height_tree, newdata = energy_dataH[test,], type = "class")
table(tree_class, energy_dataH$reduction[test])
mean(tree_class != energy_dataH$reduction[test])
```

```{r}
plot(reduction_height_tree)
text(reduction_height_tree, pretty = 0)
```

```{r}
set.seed(1)
reduction_high_cv <- cv.tree(reduction_height_tree, FUN = prune.misclass)
reduction_high_cv
plot(reduction_high_cv$size, reduction_height_tree$dev, type = "b")
```

```{r}
size_opt <- reduction_high_cv$size[which.min(reduction_high_cv$dev)]
reduction_high_pruned <- prune.misclass(reduction_height_tree, best = size_opt)
plot(reduction_high_pruned)
text(reduction_high_pruned, pretty = 0)
```

```{r}
pruned_class <- predict(reduction_high_pruned, newdata = energy_dataH[test,], 
                        type = "class")
table(pruned_class, energy_dataH$reduction[test])
mean(pruned_class != energy_dataH$reduction[test])
```

```{r}
reductionv_tree <- tree(GHG_Emission_Reduction_tCO2e ~ ., data = energy_data)
summary(reductionv_tree)
```

```{r}
set.seed(1)
n <- nrow(energy_data)
train <- sample(n, n / 2)
test <- -train
reductionv_tree <- tree(GHG_Emission_Reduction_tCO2e ~ ., data = energy_data, subset = train)
reduction_pred <- predict(reductionv_tree, newdata = energy_data[test,])
mean((reduction_pred - energy_data$GHG_Emission_Reduction_tCO2e[test])^2)
```

```{r}
production_cv <- cv.tree(reductionv_tree)
plot(production_cv$size, production_cv$dev, type = "b")
```

```{r}
production_pruned <- prune.tree(reductionv_tree, best = 4)
plot(reductionv_tree)
text(reductionv_tree)
```

```{r}
install.packages('randomForest')
```

```{r bagging}
library(randomForest)

production_bag <- randomForest(GHG_Emission_Reduction_tCO2e ~ ., data = energy_data_clean, mtry = 4, importance = TRUE)
production_bag
```

```{r}
plot(production_bag, type = "l")
```

```{r}
importance(production_bag)
```

```{r}
varImpPlot(production_bag)
```

```{r}
set.seed(2)
production_bag_forest <- randomForest(GHG_Emission_Reduction_tCO2e ~ ., data = energy_data_clean, subset = train, mtry = 4, importance = TRUE)
production_bag <- predict(production_bag_forest, newdata = energy_data_clean[test,])
mean((production_bag - energy_data_clean$GHG_Emission_Reduction_tCO2e[test])^2)
```

```{r}
varImpPlot(production_bag_forest)
```

```{r}
set.seed(2)
reduction_rf <- randomForest(GHG_Emission_Reduction_tCO2e ~ ., data = energy_data_clean, subset = train,
                         importance = TRUE)
reduction_pred_rf <- predict(reduction_rf, newdata = energy_data_clean[test,])
mean((reduction_pred_rf - energy_data_clean$GHG_Emission_Reduction_tCO2e[test])^2)
```

```{r}
varImpPlot(reduction_rf)
```

```{r}
library(ggplot2)

set.seed(2)

# Model 1: Random Forest with mtry = 4
production_bag_forest <- randomForest(GHG_Emission_Reduction_tCO2e ~ ., data = energy_data_clean, subset = train, mtry = 4, importance = TRUE)
production_bag <- predict(production_bag_forest, newdata = energy_data_clean[test,])
mse_production_bag <- mean((production_bag - energy_data_clean$GHG_Emission_Reduction_tCO2e[test])^2)

# Model 2: Random Forest with default mtry
reduction_rf <- randomForest(GHG_Emission_Reduction_tCO2e ~ ., data = energy_data_clean, subset = train, importance = TRUE)
reduction_pred_rf <- predict(reduction_rf, newdata = energy_data_clean[test,])
mse_reduction_rf <- mean((reduction_pred_rf - energy_data_clean$GHG_Emission_Reduction_tCO2e[test])^2)

# Create a data frame for ggplot
results <- data.frame(
  Actual = energy_data_clean$GHG_Emission_Reduction_tCO2e[test],
  Predicted_mtry4 = production_bag,
  Predicted_default = reduction_pred_rf
)

# Plot the results
ggplot(results, aes(x = Actual)) +
  geom_point(aes(y = Predicted_mtry4, color = "mtry = 4")) +
  geom_point(aes(y = Predicted_default, color = "default mtry")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  labs(title = "Comparison of Random Forest Predictions",
       x = "Właściwa wartość GHG Emission Reduction (tCO2e)",
       y = "Przewidywana wartość GHG Emission Reduction (tCO2e)",
       color = "Model") +
  theme_minimal()
```

```{r}
install.packages('xgboost')
```
```{r}
library(xgboost)

data(energy_data_clean)
X <- as.matrix(energy_data_clean[, !names(energy_data_clean) %in% "GHG_Emission_Reduction_tCO2e"]) 
y <- energy_data_clean$GHG_Emission_Reduction_tCO2e

params <- list(
  objective = "reg:squarederror",  
  max_depth = 4,                   
  eta = 0.01,                      
  nthread = 2                      
)

nrounds <- 5000

reduction_boost <- xgboost(data = X, label = y, params = params, nrounds = nrounds, verbose = 0)

print(reduction_boost)
```

```{r}
varImpPlot(reduction_boost)
```



```{r}
set.seed(2)

train <- sample(1:nrow(energy_data_clean), nrow(energy_data_clean) * 0.8)  
test <- setdiff(1:nrow(energy_data_clean), train)  

X_train <- as.matrix(energy_data_clean[train, !names(energy_data_clean) %in% "GHG_Emission_Reduction_tCO2e"])  
y_train <- energy_data_clean$GHG_Emission_Reduction_tCO2e[train]

X_test <- as.matrix(energy_data_clean[test, !names(energy_data_clean) %in% "GHG_Emission_Reduction_tCO2e"])  
y_test <- energy_data_clean$GHG_Emission_Reduction_tCO2e[test]

params <- list(
  objective = "reg:squarederror",  
  max_depth = 4,                   
  nthread = 2                     
)

nrounds <- 5000

reduction_boost <- xgboost(data = X_train, label = y_train, params = params, nrounds = nrounds, verbose = 0)

reduction_pred_boost <- predict(reduction_boost, newdata = X_test)

mse <- mean((reduction_pred_boost - y_test)^2)
print(mse)
```

```{r}
params <- list(
  objective = "reg:squarederror",  
  max_depth = 4,                   
  eta = 0.01,                     
  nthread = 2                     
)

reduction_boost <- xgboost(data = X_train, label = y_train, params = params, nrounds = nrounds, verbose = 0)
reduction_pred_boost <- predict(reduction_boost, newdata = X_test)
mse <- mean((reduction_pred_boost - y_test)^2)
print(mse)
```

```{r}
params <- list(
  objective = "reg:squarederror",  
  max_depth = 1,                   
  nthread = 2                     
)

reduction_boost <- xgboost(data = X_train, label = y_train, params = params, nrounds = nrounds, verbose = 0)
reduction_pred_boost <- predict(reduction_boost, newdata = X_test)
mse <- mean((reduction_pred_boost - y_test)^2)
mse
```