energy_data <- read.csv("energy_dataset.csv")
head(energy_data)

numeric_data <- energy_data[sapply(energy_data, is.numeric)]
cor(numeric_data)

energy_data$Type_of_Renewable_Energy_1 <- ifelse(energy_data$Type_of_Renewable_Energy == 1, 1, 0)
energy_data$Type_of_Renewable_Energy_2 <- ifelse(energy_data$Type_of_Renewable_Energy == 2, 1, 0)
energy_data$Type_of_Renewable_Energy_3 <- ifelse(energy_data$Type_of_Renewable_Energy == 3, 1, 0)
energy_data$Type_of_Renewable_Energy_4 <- ifelse(energy_data$Type_of_Renewable_Energy == 4, 1, 0)

head(energy_data)

dir_logistic <- list()
dir_logistic$fit <- glm(Type_of_Renewable_Energy_1 ~ Installed_Capacity_MW + Energy_Production_MWh 
                        + Energy_Storage_Capacity_MWh + Storage_Efficiency_Percentage, 
                   family = binomial, data = energy_data)
summary(dir_logistic$fit)

dir_logistic$probs <- predict(dir_logistic$fit, type = "response")
head(dir_logistic$probs)

dir_logistic$predicted <- ifelse(dir_logistic$probs > 0.5, "Type 1", "Not type 1")

dir_logistic$cm <- table(dir_logistic$predicted, energy_data$Type_of_Renewable_Energy_1)
dir_logistic$cm

mean(dir_logistic$predicted != energy_data$Type_of_Renewable_Energy_1)

set.seed(123)

train_indices <- sample(nrow(energy_data), round(0.7 * nrow(energy_data)))
train <- energy_data[train_indices, ]
energy_data_test <- energy_data[-train_indices, ]
Type_of_Renewable_Energy_1_test <- energy_data_test$Type_of_Renewable_Energy_1

dir_log_t <- list()
dir_log_t$fit <- glm(Type_of_Renewable_Energy_1 ~ Installed_Capacity_MW + Energy_Production_MWh, 
                   family = binomial, data = train)
summary(dir_logistic$fit)

dir_log_t$probs <- predict(dir_log_t$fit, energy_data_test, type = "response")
dir_log_t$predicted <- ifelse(dir_log_t$probs > 0.5, "Type 1", "Not type 1")
table(dir_log_t$predicted, Type_of_Renewable_Energy_1_test)

dir_log_best2$fit <- glm(Type_of_Renewable_Energy_1 ~ Installed_Capacity_MW + Energy_Production_MWh, 
                         family = binomial, 
                    data = energy_data, subset = train)
summary(dir_log_best2$fit)
dir_log_best2$probs <- predict(dir_log_best2$fit, energy_data_test, type = "response")
dir_log_best2$predicted <- ifelse(dir_log_best2$probs > 0.5, "Type 1", "Not type 1")
table(dir_log_best2$predicted, Type_of_Renewable_Energy_1_test)

names(train)

dir_lda <- list()
dir_lda$fit <- lda(Type_of_Renewable_Energy_1_test ~ Installed_Capacity_MW + Energy_Production_MWh,
                   data = energy_data, subset = train)
dir_lda$fit

dir_lda$predicted <- predict(dir_lda$fit, energy_data_test)
table(dir_lda$predicted$class, Type_of_Renewable_Energy_1_test)

max(dir_lda$predicted$posterior[, 2])
max(dir_lda$predicted$posterior[, 1])

dir_qda <- list()
dir_qda$fit <- qda(Type_of_Renewable_Energy_1_test ~ Installed_Capacity_MW + Energy_Production_MWh,
                   data = energy_data, subset = train)
dir_qda$fit

dir_qda$predicted <- predict(dir_qda$fit, energy_data_test)
table(dir_qda$predicted$class, Type_of_Renewable_Energy_1_test)

train_set <- Smarket[train, c("Installed_Capacity_MW", "Energy_Production_MWh")]
test_set <- Smarket[!train, c("Installed_Capacity_MW", "Energy_Production_MWh")]
renewable_energy_train <- energy_data$Type_of_Renewable_Energy_1[train]
dir_knn_1 <- knn(train_set, test_set, renewable_energy_train, k = 1)
table(dir_knn_1, Type_of_Renewable_Energy_1_test)