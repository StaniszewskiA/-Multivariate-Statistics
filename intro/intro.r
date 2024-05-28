data <- read.csv("Auto.csv")
head(data)

mean_usage <- mean(data$mpg, na.rm = TRUE)
print(mean_usage)

data_4_cyl <- subset(data, cylinders == 4)
mean_mpg_4_cyl <- mean(data_4_cyl$mpg, na.rm = TRUE)

print(mean_mpg_4_cyl)

median_weight <- median(data$weight, na.rm = TRUE)

print(median_weight)

data_1972 <- data[data$year == 72, ]
mean_mpg_1972 <- mean(data_1972$mpg, na.rm = TRUE)

print(mean_mpg_1972)

var_acceleration <- var(data$acceleration, na.rm = TRUE)

print(var_acceleration)

japanese <- data[data$origin == 3, ]
var_acceleration_japanese <- var(japanese$acceleration, na.rm = TRUE)

print(var_acceleration_japanese)

mean_horsepower <- mean(as.numeric(data$horsepower), na.rm = TRUE)
above_average <- sum(as.numeric(data$horsepower) > mean_horsepower, na.rm = TRUE)

print(above_average)

mean_weigth <- mean(data$weight, na.rm = TRUE)
below_average_weight <- data[data$weight < mean_weigth, ]
max_power <- max(below_average_weight$horsepower, na.rm = TRUE)

print(max_power)

mean_mpg <- mean(data$mpg, na.rm = TRUE)
above_average_mpg <- data[data$mpg > mean_mpg, ]
amount_above <- nrow(above_average_mpg)

print(amount_above)

min_cylinders <- min(above_average_mpg$cylinders)

print(min_cylinders)

max_volume <- max(data$displacement, na.rm = TRUE)
max_volume_cars <- data[data$displacement == max_volume, ]
max_volume_cars_amount <- nrow(max_volume_cars)

print(max_volume_cars_amount)

volume_median <- median(data$displacement, na.rm = TRUE)
below_median <- data[data$displacement < volume_median, ]
max_weigth<- max(samochody_mniejsze_od_mediany$weight, na.rm = TRUE)

print(maksymalna_waga)
