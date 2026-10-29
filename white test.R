# White test
```{r setup, include=FALSE}
library(pacman)
p_load(dplyr, tidyverse, ggplot2, fixest)
setwd("C:/Users/16085/Documents/ECON/EC 421")
```
#Main dataset for this problem set
```{r}
lab4_data <- read_csv("data-ps1.csv")
```{r}
reg_w <- lm(data = lab4_data, life_exp ~ pct_exercise)
lab4_data <- lab4_data |>
  mutate(errors = residuals(reg_w))
glimpse(lab4_data)
```

```{r}
reg_error <- lm(data = lab4_data, life_exp ~ pct_exercise + I(pct_exercise^2))
summary(reg_error)
reg_err_r2 <- summary(reg_error)$r.squared
```

```{r}
LMM <- 3106 * reg_err_r2
# Chi-squared (3103 degrees of freedom) 124.342, reject null hypothesis of homoskedasticity
```

```{r}
p_load(lmtest, sandwich)
coeftest(reg_w, vcovHC(reg_w, type = "HC0"))
```

```{r}
X_bar <- mean(lab4_data$pct_exercise)
denom <- sum((lab4_data$pct_exercise - X_bar)^2)
numerator <- sqrt(sum(((lab4_data$pct_exercise - X_bar)^2 * lab4_data$errors^2)))
numerator/denom
```

```{r}
ggplot(data = lab4_data, aes(x = poverty_rate, y = life_exp, color = income_quartile, size = pop)) +
  geom_point() +
  theme_minimal()
```