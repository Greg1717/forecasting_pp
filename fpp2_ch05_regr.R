library(fpp2)
library(forecast)

# 5.1 The linear model ========================================================

## Simple linear regression ===========

data(uschange)

autoplot(uschange, facet=TRUE)

autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x = Income, y = Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


tslm(Consumption ~ Income, data = uschange)

## Multiple linear regression =========

# Each of the predictor variables must be numerical.

uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()


# 5.2 Least squares estimation ================================================
fit.consMR <- tslm(Consumption ~ Income + Production + Unemployment + Savings,
                   data = uschange)

summary(fit.consMR)

## Fitted values ======================

# Time plot of actual US consumption expenditure and predicted US consumption expenditure.
autoplot(uschange[, 'Consumption'], series = "Data") +
  autolayer(fitted(fit.consMR), series = "Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour = guide_legend(title = " "))

# Actual US consumption expenditure plotted against predicted US consumption expenditure.
cbind(Data = uschange[, "Consumption"],
      Fitted = fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept = 0, slope = 1)


## Goodness-of-fit ====================


## Standard error of the regression ====


# 5.3 Evaluating the regression model =========================================

# Analysing the residuals from a regression model for US quarterly consumption.
checkresiduals(fit.consMR)

# The autocorrelation plot shows a significant spike at lag 7, but it is not quite enough for the Breusch-Godfrey to be significant at the 5% level. In any case, the autocorrelation is not particularly large, and at lag 7 it is unlikely to have any noticeable impact on the forecasts or the prediction intervals.


## Residual plots against predictors ====

# We would expect the residuals to be randomly scattered without showing any systematic patterns. A simple and quick way to check this is to examine scatterplots of the residuals against each of the predictor variables. If these scatterplots show a pattern, then the relationship may be nonlinear and the model will need to be modified accordingly. See Section 5.8 for a discussion of nonlinear regression.

# It is also necessary to plot the residuals against any predictors that are not in the model. If any of these show a pattern, then the corresponding predictor may need to be added to the model (possibly in a nonlinear form).
df <- as.data.frame(uschange)

df[,"Residuals"]  <- as.numeric(residuals(fit.consMR))

p1 <- ggplot(df, aes(x = Income, y = Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x = Production, y = Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x = Savings, y = Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x = Unemployment, y = Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)


## Residual plots against fitted values =====

# A plot of the residuals against the fitted values should also show no pattern. If a pattern is observed, there may be “heteroscedasticity” in the errors which means that the variance of the residuals may not be constant. If this problem occurs, a transformation of the forecast variable such as a logarithm or square root may be required


cbind(Fitted = fitted(fit.consMR),
      Residuals = residuals(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Fitted, y = Residuals)) + geom_point()


