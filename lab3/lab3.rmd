---
title: "lab3"
output: html_document
date: "2024-06-03"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r loading the dataset}
energy_data <- read.csv("energy_dataset.csv")
head(energy_data)
# names(energy_data)
# dim(energy_data)
```


```{r}
set.seed(1)
n <- nrow(energy_data)
train <- sample(n, n / 2)
```

```{r}
energy_lm <- lm(Type_of_Renewable_Energy ~ Energy_Production_MWh, data = energy_data, subset = train)
validation_set <- energy_data[-train,]
mse <- mean((validation_set$Type_of_Renewable_Energy - predict(energy_lm, validation_set))^2)
mse
```

```{r}
for (i in 2:5) {
  energy_lm_poly <- lm(Type_of_Renewable_Energy ~ poly(Energy_Production_MWh, degree = i), data = energy_data, 
                     subset = train)
  print(mean((validation_set$Type_of_Renewable_Energy - predict(energy_lm_poly, validation_set))^2))
}
```

```{r}
degree_max <- 5

compute_mse <- function(degree, train) {
  energy_lm <- lm(Type_of_Renewable_Energy ~ poly(Energy_Production_MWh, degree), data = energy_data, subset = train)
  validation_set <- energy_data[-train,]
  mean((validation_set$Type_of_Renewable_Energy - predict(energy_lm, validation_set))^2)
}

mse <- vapply(1:degree_max, compute_mse, FUN.VALUE = numeric(1), train = train)
mse
```
```{r}
plot(mse, xlab = "Stopień wielomianu", ylab = "MSE", type = "b", pch = 20, 
     col = "blue")
```

```{r}
library(boot)
library(stats)

degree_max <- 1

compute_loocv_mse <- function(degree) {
  energy_glm <- glm(Type_of_Renewable_Energy ~ poly(Energy_Production_MWh, degree), data = energy_data)
  cv.glm(energy_data, energy_glm)$delta[1]
}

mse <- sapply(1:degree_max, compute_loocv_mse)
mse
```

```{r}
plot(mse, xlab = "Stopień wielomianu", ylab = "LOOCV MSE", type = "b", pch = 20, 
     col = "blue")
```

```{r}
compute_kcv_mse <- function(degree, k) {
  energy_glm <- glm(Type_of_Renewable_Energy ~ poly(Energy_Production_MWh, degree), data = energy_data)
  cv.glm(energy_data, energy_glm, K = k)$delta[1]
}
mse10 <- sapply(1:degree_max, compute_kcv_mse, k = 10)
mse10
```

```{r}
matplot(mse10, pch = 20, type = "l", xlim = c(1, degree_max), ylim = c(18, 25),
        xlab = "Stopień wielomianu", ylab = "Walidacyjny MSE")
```

```{r}
lm_coefs <- function(data, index = 1:nrow(data)) {
  coef(lm(Type_of_Renewable_Energy ~ (Energy_Production_MWh)^2, data = energy_data, subset = index))
}

n <- nrow(energy_data)
lm_coefs(energy_data, sample(n, n, replace = TRUE))

lm_coefs(energy_data)

boot(energy_data, lm_coefs, R = 1000)
```

```{r}
library(ISLR)
library(leaps)
```

```{r}
energy_bs <- regsubsets(Type_of_Renewable_Energy ~ ., data = energy_data, nvmax=12)
energy_bs_sum <- summary(energy_bs)

energy_bs_sum$cp
bic_min <- which.min(energy_bs_sum$bic)
bic_min
energy_bs_sum$bic[bic_min]

energy_bs_sum

plot(energy_bs_sum$bic, xlab = "Liczba zmiennych", ylab = "BIC", col = "green",
     type = "b", pch = 20)
points(bic_min, energy_bs_sum$bic[bic_min], col = "red", pch = 9)

plot(energy_bs, scale = "bic")
```
```{r}
library(leaps)  
energy_bs <- regsubsets(Type_of_Renewable_Energy ~ ., data = energy_data, nvmax = 12)
energy_bs_sum <- summary(energy_bs)

r2_values <- energy_bs_sum$rsq

r2_max <- which.max(r2_values)
r2_max_value <- r2_values[r2_max]

print(paste("Highest R2 is at model:", r2_max))
print(paste("R2 value:", r2_max_value))

plot(r2_values, xlab = "Liczba zmiennych", ylab = "R2", col = "blue", type = "b", pch = 20)
points(r2_max, r2_values[r2_max], col = "red", pch = 9)

plot(energy_bs, scale = "r2")
```


```{r}
coef(energy_bs, id = 8)
```

```{r}
energy_fwd <- regsubsets(Type_of_Renewable_Energy ~ ., data = energy_data, nvmax=10, 
                          method = "forward")
energy_fwd_sum <- summary(energy_fwd)
energy_fwd_sum
energy_back <- regsubsets(Type_of_Renewable_Energy ~ ., data = energy_data, nvmax=10, 
                          method = "backward")
energy_back_sum <- summary(energy_back)
energy_back_sum
```

```{r}
n <- nrow(energy_data)
train <- sample(c(TRUE, FALSE), n, replace = TRUE)
test <- !train
energy_bs_v <- regsubsets(Type_of_Renewable_Energy ~ ., data = energy_data, nvmax=10)
```

```{r}
predict.regsubsets <- function(object, newdata, id, ...) {
  model_formula <- as.formula(object$call[[2]])
  mat <- model.matrix(model_formula, newdata)
  coefs <- coef(object, id = id)
  mat[, names(coefs)] %*% coefs
}
```

```{r}
prediction_error <- function(i, model, subset) {
  pred <- predict(model, energy_data[subset,], id = i)
  mean((energy_data$Type_of_Renewable_Energy[subset] - pred)^2)
}
val_errors <- sapply(1:10, prediction_error, model = energy_bs_v, subset = test)
val_errors

best_model_index <- which.min(val_errors)

best_model <- energy_bs_v[[best_model_index]]
print(best_model_index)

```

```{r}
k <- 10
folds <- sample(1:k, n, replace = TRUE)
val_err <- NULL
for (j in 1:k) {
  fit_bs <- regsubsets(Type_of_Renewable_Energy ~ ., data = energy_data[folds != j,], nvmax=10)
  err <- sapply(1:10, prediction_error, model = fit_bs, subset = (folds == j))
  val_err <- rbind(val_err, err)
}

cv_errors <- colMeans(val_err)
cv_errors
```
