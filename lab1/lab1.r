energy_data <- read.csv("energy_dataset.csv")
head(energy_data)

names(energy_data)

dim(energy_data)

fit_simple <- lm(energy_data$Installed_Capacity_MW ~ energy_data$Energy_Production_MWh)
fit_simple <- lm(Installed_Capacity_MW ~ Energy_Production_MWh, data = energy_data)

class(fit_simple)
is.list(fit_simple)
names(fit_simple)

fit_simple$coefficients
coef(fit_simple)

?summary.lm
summary(fit_simple)

summaryList <- summary(fit_simple)
summaryList$sigma
summaryList$r.squared
summaryList$fstatistic

confint(fit_simple)

plot(energy_data$Installed_Capacity_MW, energy_data$Energy_Production_MWh)
abline(fit_simple)

plot(fit_simple)

plot(predict(fit_simple), residuals(fit_simple))
plot(predict(fit_simple), rstudent(fit_simple))

plot(hatvalues(fit_simple))
which.max(hatvalues(fit_simple))

fit_la <- lm(energy_data$Installed_Capacity_MW ~ energy_data$Energy_Production_MWh + energy_data$Energy_Consumption_MWh)
summary(fit_la)

numerical_data <- energy_data[, sapply(energy_data, is.numeric)]

fit_all <- lm(Installed_Capacity_MW ~ ., data = numerical_data)
summary(fit_all)

fit_no_grid_int <- update(fit_all, ~ . - Grid_Integration_Level)
summary(fit_no_grid_int)

plot(ellipse(fit_la, which = -1), type = "l")
la_coefs <- coef(fit_la)
points(la_coefs[2], la_coefs[3])

summary(lm(energy_data$Installed_Capacity_MW ~ energy_data$Energy_Production_MWh * energy_data$Energy_Consumption_MWh))

fit_l2 <- lm(energy_data$Installed_Capacity_MW ~ energy_data$Energy_Production_MWh + I(energy_data$Energy_Consumption_MWh^2))
summary(fit_l2)

anova(fit_simple, fit_l2)

fit_l5 <- lm(energy_data$Energy_Consumption_MWh ~ poly(energy_data$Energy_Production_MWh, 5))
summary(fit_l5)

summary(lm(Energy_Production_MWh ~ log(Energy_Consumption_MWh), data = energy_data))