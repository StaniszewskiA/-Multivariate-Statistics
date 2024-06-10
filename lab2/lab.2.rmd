---
title: "lab2"
output: html_document
date: "2024-06-03"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r installs}
install.packages("caret")
```


```{r imports}
library(MASS)
library(caret)
```

```{r loading the dataset}
energy_data <- read.csv("energy_dataset.csv")
head(energy_data)
# names(energy_data)
# dim(energy_data)
```

```{r  one hot encoding - could be upgraded}
unique_types <- unique(energy_data$Type_of_Renewable_Energy)

for (type in unique_types) {
  column_name <- paste("Type_of_Renewable_Energy", type, sep = "_")
  energy_data[[column_name]] <- ifelse(energy_data$Type_of_Renewable_Energy == type, 1, 0)
}

energy_data <- subset(energy_data, select = -c(Type_of_Renewable_Energy))

print(energy_data)
```

```{r}
energy_data
```
```{r}
numeric_data <- energy_data[sapply(energy_data, is.numeric)]
corr_mat <- cor(numeric_data)
corr_df <- as.data.frame(as.table(corr_mat))
corr_df <- corr_df[corr_df$Var1 != corr_df$Var2,]
corr_df <- corr_df[order(abs(corr_df$Freq)),]
top_corrs <- head(corr_df, 10)

print(top_corrs)
```

```{r}
dir_logistic <- list()
dir_logistic$fit <- glm(Type_of_Renewable_Energy_1 ~ Installed_Capacity_MW + Energy_Production_MWh 
                        + Energy_Storage_Capacity_MWh + Storage_Efficiency_Percentage, 
                   family = binomial, data = energy_data)
summary(dir_logistic$fit)
```

```{r}
dir_logistic$probs <- predict(dir_logistic$fit, type = "response")
head(dir_logistic$probs)
```
```{r}
contrasts(energy_data$Type_of_Renewable_Energy_1)
```

```{r}
dir_logistic$predicted <- ifelse(dir_logistic$probs > 0.5, "Type 1", "Not type 1")
dir_logistic$cm <- table(dir_logistic$predicted, energy_data$Type_of_Renewable_Energy_1)
dir_logistic$cm
```

```{r}
mean(dir_logistic$predicted == energy_data$Type_of_Renewable_Energy_1)
```

```{r}
set.seed(123)

train_indices <- sample(nrow(energy_data), round(0.7 * nrow(energy_data)))
train <- energy_data[train_indices, ]
energy_data_test <- energy_data[-train_indices, ]
Type_of_Renewable_Energy_1_test <- energy_data_test$Type_of_Renewable_Energy_1

print(train)
```

```{r}
dir_log_t <- list()
dir_log_t$fit <- glm(Type_of_Renewable_Energy_1 ~ Installed_Capacity_MW + Energy_Production_MWh, 
                   family = binomial, data = train)
summary(dir_logistic$fit)
```

```{r}
dir_log_t$probs <- predict(dir_log_t$fit, energy_data_test, type = "response")
dir_log_t$predicted <- ifelse(dir_log_t$probs > 0.5, "Type 1", "Not type 1")
table(dir_log_t$predicted, Type_of_Renewable_Energy_1_test)
```
```{r}
table(dir_log_best2$predicted, Type_of_Renewable_Energy_1_test)
```

```{r}
dir_log_best2$fit <- glm(Type_of_Renewable_Energy_1 ~ Energy_Production_MWh + Energy_Storage_Capacity_MWh, 
                         family = binomial, 
                    data = energy_data, 
                    subset = train)

```

```{r lda}
dir_lda <- list()
dir_lda$fit <- lda(Type_of_Renewable_Energy_1 ~ Energy_Storage_Capacity_MWh + Energy_Production_MWh,
                   data = energy_data, subset = train)
dir_lda$fit
```

```{r qda}
dir_qda$predicted <- predict(dir_qda$fit, energy_data_test)
table(dir_qda$predicted$class, Type_of_Renewable_Energy_1_test)
```

```{r knn}
train_set <- energy_data[train, c("Energy_Storage_Capacity_MWh", "Energy_Production_MWh")]
test_set <- energy_data[!train, c("Energy_Storage_Capacity_MWh", "Energy_Production_MWh")]
renewable_energy_train <- energy_data$Type_of_Renewable_Energy_1[train]
dir_knn_1 <- knn(train_set, test_set, renewable_energy_train, k = 1)
table(dir_knn_1, Type_of_Renewable_Energy_1_test)
```