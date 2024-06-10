---
title: "Multi-variate Stats"
output: html_document
date: "2024-06-02"
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


```{r simple fit}
fit_simple <- lm(energy_data$Installed_Capacity_MW ~ energy_data$Energy_Production_MWh)
fit_simple <- lm(Installed_Capacity_MW ~ Energy_Production_MWh, data = energy_data)
```

```{r simple fit analysis}
class(fit_simple)
is.list(fit_simple)
names(fit_simple)
```
```{r coefficients}
fit_simple$coefficients
coef(fit_simple)
```

``` {r fit simple summary}
?summary.lm
summary(fit_simple)
```

``` {r further summary}
summaryList <- summary(fit_simple)
summaryList$sigma
summaryList$r.squared
summaryList$fstatistic
```

``` {r confidence intervals}
confint(fit_simple)
```
```{r test}
names(energy_data)
```

```{r predictions}
predict(fit_simple, data.frame(Energy_Production_MWh = c(5, 10, 15)), interval = "confidence")
predict(fit_simple, data.frame(Energy_Production_MWh = c(5, 10, 15)), interval = "prediction")
```

```{r prediction plots}
plot(energy_data$Installed_Capacity_MW, energy_data$Energy_Production_MWh)
abline(fit_simple)
plot(fit_simple)
plot(predict(fit_simple), residuals(fit_simple))
plot(predict(fit_simple), rstudent(fit_simple))
plot(hatvalues(fit_simple))
which.max(hatvalues(fit_simple))
```
``` {r multiple regression}
fit_la <- lm(energy_data$Installed_Capacity_MW ~ energy_data$Energy_Production_MWh + energy_data$Energy_Consumption_MWh)
summary(fit_la)
```
```{r multiple regression}
numerical_data <- energy_data[, sapply(energy_data, is.numeric)]

fit_all <- lm(Installed_Capacity_MW ~ ., data = numerical_data)
summary(fit_all)
```

```{r no grid int level}
fit_no_grid_int <- update(fit_all, ~ . - Grid_Integration_Level)
summary(fit_no_grid_int)
```

```{r packages}
install.packages("ellipse")
```

```{r elipse}
library(ellipse)
plot(ellipse(fit_la, which = -1), type = "l")
la_coefs <- coef(fit_la)
points(la_coefs[2], la_coefs[3])
summary(lm(energy_data$Installed_Capacity_MW ~ energy_data$Energy_Production_MWh * energy_data$Energy_Consumption_MWh))
```

```{r interactions}
fit_l2 <- lm(energy_data$Installed_Capacity_MW ~ energy_data$Energy_Production_MWh + I(energy_data$Energy_Consumption_MWh^2))
summary(fit_l2)
```

```{r anova}
anova(fit_simple, fit_l2)
fit_l5 <- lm(energy_data$Energy_Consumption_MWh ~ poly(energy_data$Energy_Production_MWh, 5))
summary(fit_l5)
summary(lm(Energy_Production_MWh ~ log(Energy_Consumption_MWh), data = energy_data))
```