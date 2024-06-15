---
title: "lab4"

output: html_document
date: "2024-06-08"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r Loading packages}
install.packages("glmnet")
```

```{r loading the dataset}
library(glmnet)

energy_data <- read.csv("energy_dataset.csv")
head(energy_data)
```

```{r pressure, echo=FALSE}
X <- model.matrix(Installed_Capacity_MW ~ ., data = energy_data)[, -1]
y <- energy_data$Installed_Capacity_MW
```

```{r}
lambda_grid <- 10^seq(10, -2, length.out = 100)
fit_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_grid)

dim(coef(fit_ridge))
```

```{r}
fit_ridge$lambda[50]
coef_ridge <- coef(fit_ridge)[, 50]
coef_ridge
sqrt(sum(coef_ridge[-1]^2))
```
````{r}
fit_ridge$lambda[70]
coef(fit_ridge)[, 70]
sqrt(sum(coef(fit_ridge)[-1, 70]^2))
````
````{r}
predict(fit_ridge, s = 50, type = "coefficients")
````
````{r}
set.seed(1)
n <- nrow(X)
train <- sample(n, n / 2)
test <- -train
fit_ridge <- glmnet(X[train,], y[train], alpha = 0, lambda = lambda_grid,
                    thresh = 1e-12)
````

````{r}
pred_ridge <- predict(fit_ridge, s = 4, newx = X[test,])
mean((pred_ridge - y[test])^2)
````

````{r}
pred_null <- mean(y[train])
mean((pred_null - y[test])^2)
````

````{r}
pred_ridge_big <- predict(fit_ridge, s = 1e10, newx = X[test,])
mean((pred_ridge_big - y[test])^2)
````

````{r}
pred_ridge_0 <- predict(fit_ridge, x = X[train,], y = y[train], s = 0, 
                      newx = X[test,], exact = TRUE)
mean((pred_ridge_0 - y[test])^2)
````

````{r}
lm(y ~ X, subset = train)
predict(fit_ridge, x = X[train,], y = y[train], s = 0, exact = TRUE, 
        type = "coefficients")[1:13,]
````

````{r}
set.seed(1)
cv_out <- cv.glmnet(X[train,], y[train], alpha = 0)
plot(cv_out)
cv_out$lambda.min
````

````{r}
pred_ridge_opt <- predict(fit_ridge, s = cv_out$lambda.min, newx = X[test,])
mean((pred_ridge_opt - y[test])^2)
````

````{r}
fit_ridge_full <- glmnet(X, y, alpha = 0)
predict(fit_ridge_full, s = cv_out$lambda.min, type = "coefficients")
````

````{r}
fit_lasso <- glmnet(X[train,], y[train], alpha = 1)
plot(fit_lasso, xvar = "lambda")
````

````{r}
cv_out <- cv.glmnet(X[train,], y[train], alpha = 1)
plot(cv_out)
cv_out$lambda.min
pred_lasso <- predict(fit_lasso, s = cv_out$lambda.min, newx = X[test,])
mean((pred_lasso - y[test])^2)
````

````{r}
# Load necessary library
library(glmnet)

# Perform Lasso regression with cross-validation on training data
cv_out <- cv.glmnet(X[train,], y[train], alpha = 1)

# Plot the cross-validation curve
plot(cv_out)

# Retrieve the optimal lambda
optimal_lambda <- cv_out$lambda.min

# Calculate the mean squared error on the test set
pred_lasso <- predict(cv_out$glmnet.fit, s = optimal_lambda, newx = X[test,])
mse <- mean((pred_lasso - y[test])^2)
print(paste("Mean Squared Error on Test Set:", mse))

# Fit the Lasso model to the entire dataset
fit_lasso_full <- glmnet(X, y, alpha = 1)

# Extract the coefficients using the optimal lambda from cv_out
coefficients <- predict(fit_lasso_full, s = optimal_lambda, type = "coefficients")

# Display the first 13 coefficients
print(coefficients[1:13,])

print(optimal_lambda)
````

````{r Part 2}
fit_poly <- lm(Air_Pollution_Reduction_Index ~ poly(Energy_Production_MWh, 4), data = energy_data)
summary(fit_poly)
````

```{r}
fit_poly_raw <- lm(Air_Pollution_Reduction_Index ~ poly(Energy_Production_MWh, 4, raw = TRUE), data = energy_data)
summary(fit_poly_raw)
```

```{r}
production_lims <- range(energy_data$Energy_Production_MWh)
production_grid <- seq(production_lims[1], production_lims[2])
pred_poly <- predict(fit_poly, list(Energy_Production_MWh = production_grid), se.fit = TRUE)
se_bands <- cbind(pred_poly$fit + 2 * pred_poly$se.fit, 
                  pred_poly$fit - 2 * pred_poly$se.fit)
plot(energy_data$Energy_Production_MWh, energy_data$Air_Pollution_Reduction_Index, col = "darkgrey", cex = 0.5, xlim = production_lims)
lines(production_grid, pred_poly$fit, col = "red", lwd = 2)
matlines(production_grid, se_bands, col = "red", lty = "dashed")
```
```{r}
threshold <- 40
fit_log_poly <- glm(I(Air_Pollution_Reduction_Index > threshold) ~ poly(Energy_Production_MWh, 4), data = energy_data, family = binomial)
```

```{r}
pred_log_poly <- predict(fit_log_poly, list(Energy_Production_MWh = production_grid), se.fit = TRUE)
pred_probs <- plogis(pred_log_poly$fit)
se_bands_logit <- cbind(pred_log_poly$fit + 2 * pred_log_poly$se.fit,
                        pred_log_poly$fit - 2 * pred_log_poly$se.fit)
se_bands <- plogis(se_bands_logit)
plot(energy_data$Energy_Production_MWh, I(energy_data$Energy_Production_MWh > 40), xlim = production_lims, ylim = c(0, 1), 
     col = "darkgrey", cex = 0.5, ylab = "P(Air_Pollution_Reduction_Index > 40 | Energy_Production_MWh)")
lines(production_grid, pred_probs, col = "red", lwd = 2)
matlines(production_grid, se_bands, lty = "dashed", col = "red")
```
```{r}
table(cut(energy_data$Air_Pollution_Reduction_Index, breaks = 4))
```
```{r}
fit_step <- lm(Air_Pollution_Reduction_Index ~ cut(Energy_Production_MWh, 4), data = energy_data)
pred_step <- predict(fit_step, list(Energy_Production_MWh = production_grid), se.fit = TRUE)
se_bands <- cbind(pred_step$fit + 2 * pred_step$se.fit, 
                  pred_step$fit - 2 * pred_step$se.fit)
plot(energy_data$Energy_Production_MWh, energy_data$Air_Pollution_Reduction_Index, col = "darkgrey", cex = 0.5, xlim = production_lims)
lines(production_grid, pred_step$fit, col = "red", lwd = 2)
matlines(production_grid, se_bands, col = "red", lty = "dashed")
```

```{r}
library(splines)


fit_bs_knots <- lm(Air_Pollution_Reduction_Index ~ bs(Energy_Production_MWh, knots = c(5, 25, 40, 60, 120, 150, 200)), data = energy_data)
pred_bs_knots <- predict(fit_bs_knots, list(Energy_Production_MWh = production_grid), se.fit = TRUE)
plot(energy_data$Energy_Production_MWh, energy_data$Air_Pollution_Reduction_Index, cex = 0.5, col = "darkgrey")
lines(production_grid, pred_bs_knots$fit, col = "red", lwd = 2)
lines(production_grid, pred_bs_knots$fit + 2 * pred_bs_knots$se.fit, col = "red",
      lty = "dashed")
lines(production_grid, pred_bs_knots$fit - 2 * pred_bs_knots$se.fit, col = "red",
      lty = "dashed")
abline(v = c(25, 40, 60), lty = "dotted")
```

```{r}
fit_bs_df <- lm(Air_Pollution_Reduction_Index ~ bs(Energy_Production_MWh, df = 6), data = energy_data)
pred_bs_df <- predict(fit_bs_df, list(Energy_Production_MWh = production_grid), se.fit = TRUE)
plot(energy_data$Energy_Production_MWh, energy_data$Air_Pollution_Reduction_Index, cex = 0.5, col = "darkgrey")
lines(production_grid, pred_bs_df$fit, col = "red", lwd = 2)
lines(production_grid, pred_bs_df$fit + 2 * pred_bs_df$se.fit, col = "red",
      lty = "dashed")
lines(production_grid, pred_bs_df$fit - 2 * pred_bs_df$se.fit, col = "red",
      lty = "dashed")
bs_knots <- attr(bs(energy_data$Energy_Production_MWh, df = 6), "knots")
abline(v = bs_knots, lty = "dotted")
```

```{r}
fit_ns <- lm(Air_Pollution_Reduction_Index ~ ns(Energy_Production_MWh, df = 4), data = energy_data)
pred_ns <- predict(fit_ns, list(Energy_Production_MWh = production_grid), se.fit = TRUE)
plot(energy_data$Energy_Production_MWh, energy_data$Air_Pollution_Reduction_Index, cex = 0.5, col = "darkgrey")
lines(production_grid, pred_ns$fit, col = "red", lwd = 2)
lines(production_grid, pred_ns$fit + 2 * pred_ns$se.fit, col = "red",
      lty = "dashed")
lines(production_grid, pred_ns$fit - 2 * pred_ns$se.fit, col = "red",
      lty = "dashed")
abline(v = attr(ns(energy_data$Energy_Production_MWh, df = 4), "knots"), lty = "dotted")
```

```{r}
fit_smooth_df <- smooth.spline(energy_data$Air_Pollution_Reduction_Index, energy_data$Energy_Production_MWh, df = 7)
plot(energy_data$Air_Pollution_Reduction_Index, energy_data$Energy_Production_MWh, cex = 0.5, col = "darkgrey")
lines(fit_smooth_df, col = "red", lwd = 2)
```

```{r}
fit_smooth_cv <- smooth.spline(energy_data$Air_Pollution_Reduction_Index, energy_data$Energy_Production_MWh, cv = TRUE)
plot(energy_data$Air_Pollution_Reduction_Index, energy_data$Energy_Production_MWh, cex = 0.5, col = "darkgrey")
lines(fit_smooth_cv, col = "red", lwd = 2)
```

```{r}
spans <- c(0.2, 10)
clrs <- c("red", "blue")
plot(energy_data$Air_Pollution_Reduction_Index, energy_data$Energy_Production_MWh, cex = 10, col = "darkgrey")
for (i in 1:length(spans)) {
   fit_loess <- loess(Air_Pollution_Reduction_Index ~ Energy_Production_MWh, span = spans[i], data = energy_data)
   pred_loess <- predict(fit_loess, data.frame(Energy_Production_MWh = production_grid))
   lines(production_grid, pred_loess, col = clrs[i], lwd = 2)
}
legend("topright", legend = paste("s =", spans), col = clrs, lty = 1, lwd = 2)
```

```{r}
spans <- c(0.2, 0.5)
clrs <- c("red", "blue")
plot(energy_data$Type_of_Renewable_Energy, energy_data$Energy_Production_MWh, cex = 0.5, col = "darkgrey")
for (i in 1:length(spans)) {
   fit_loess <- loess(Type_of_Renewable_Energy ~ Energy_Production_MWh, span = spans[i], degree = 1, data = energy_data)
   pred_loess <- predict(fit_loess, data.frame(age = age_grid))
   lines(age_grid, pred_loess, col = clrs[i], lwd = 2)
}
legend("topright", legend = paste("s =", spans), col = clrs, lty = 1, lwd = 2)
```
```{r Gams}
library(splines)

fit_gam_ls <- lm(Air_Pollution_Reduction_Index ~ ns(Installed_Capacity_MW, df = 4) + ns(Energy_Production_MWh, df = 5) + Energy_Consumption_MWh,
                 data = energy_data)
fit_gam_ls
summary(fit_gam_ls)
```
```{r}
install.packages('gam')
```

```{r}
library(gam)

fit_gam_bf <- gam(Air_Pollution_Reduction_Index ~ s(Installed_Capacity_MW, df = 4) + s(Energy_Production_MWh, df = 5) + Energy_Consumption_MWh, data = energy_data)
summary(fit_gam_bf)

par(mfrow = c(1, 3))
plot(fit_gam_bf, col = "red", se = TRUE)
```

```{r}
fit_gam_1 <- gam(Air_Pollution_Reduction_Index ~ s(Installed_Capacity_MW, df = 5) + Energy_Production_MWh, data = energy_data)
fit_gam_2 <- gam(Air_Pollution_Reduction_Index ~ Energy_Storage_Capacity_MWh + s(Installed_Capacity_MW, df = 5) + Energy_Production_MWh, data = energy_data)
anova(fit_gam_1, fit_gam_2, fit_gam_bf, test = "F")
```

```{r}
fit_gam_lo <- gam(Air_Pollution_Reduction_Index ~ s(Installed_Capacity_MW, df = 4) + lo(Energy_Production_MWh, span = 0.7) + Energy_Consumption_MWh, 
                  data = energy_data)
summary(fit_gam_lo)
par(mfrow = c(1, 3))
plot(fit_gam_lo, col = "green", se = TRUE)
```

```{r}
threshold <- 40

fit_logistic_gam <- gam(I(Air_Pollution_Reduction_Index > threshold)  + s(Energy_Production_MWh, df = 5) + Energy_Consumption_MWh, 
                        family = binomial, data = energy_data)
summary(fit_logistic_gam)
par(mfrow = c(1, 3))
plot(fit_logistic_gam, col = "blue", se = TRUE)
```

```{r}
head(energy_data)
```
