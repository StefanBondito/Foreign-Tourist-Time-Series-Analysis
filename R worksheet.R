library(dplyr)
library(readxl)
library(tseries)
library(xts)
library(neuralnet)
library(forecast)
data.Raw <- read_excel("OneDrive - Bina Nusantara/Sem.6/Time Series/AOL/dataForR.xlsx")
data.decom <- data.Raw[2]
data.ts <- ts(data.decom, frequency = 12,start = c(2020,4), end = c(2024,3))
decomposed <- decompose(data.ts)


#### Plotting ####
time_index <- seq(from = as.Date("2020-04-01"), by = "month", length.out = length(data.ts))
# Load necessary libraries
library(readxl)
library(ggplot2)

# Convert the ts object to a data frame
df <- data.frame(
  date = time_index,
  value = as.numeric(data.ts)
)

# Plot the time series data
plot.ts <- ggplot(df, aes(x = date, y = value)) +
  geom_line(color = "black", size = 1) +       # Line plot with blue color and thicker line
  geom_point(color = "black", size = 2) +       # Points on the line with red color
  labs(title = "Yearly Tourist Arrrival in Indonesia",            # Title of the plot
       x = "Date",                            # X-axis label
       y = "Tourist") +                         # Y-axis label
  theme_minimal() +                           # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5),   # Center align the title
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels
  )+
  scale_y_continuous(labels = scales::comma)


### Seasonal
seasonal.component <- decomposed$seasonal
df.seasonal <- data.frame(
  date = time_index,
  value = as.numeric(seasonal.component)
)
# Plot the time series data
plot.seasonal <- ggplot(df.seasonal, aes(x = date, y = value)) +
  geom_line(color = "black", size = 1) +       # Line plot with blue color and thicker line
  geom_point(color = "black", size = 2) +       # Points on the line with red color
  labs(title = "Seasonal",            # Title of the plot
       x = "Date") +                         # Y-axis label
  theme_minimal() +                           # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5),   # Center align the title
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


### Trend
trend.component<- decomposed$trend
df.trend  <- data.frame(
  date= time_index,
  value = trend.component
)

plot.trend <- ggplot(df.trend, aes(x = date, y = value)) +
  geom_line(color = "black", size = 1) +       # Line plot with blue color and thicker line
  geom_point(color = "black", size = 2) +       # Points on the line with red color
  labs(title = "Trend",            # Title of the plot
       x = "Date") +                         # Y-axis label
  theme_minimal() +                           # Minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5),   # Center align the title
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

print(plot.ts)
print(plot.seasonal)
print(plot.trend)


#### TSR ####
library(caret)
dummy <- dummyVars("~.", data = data.Raw)
data.withDummy <- data.frame(predict(dummy, newdata = data.Raw))
data.withDummy$t <- seq(1:48)
data.withDummy

data.Model <- data.withDummy[,-c(5)]
#Training Testing
TSR.training <- data.Model[1:39,]
TSR.testing <- data.Model[40:nrow(data.Model),]

#Base Model
model = lm(Jumlah~., data = TSR.training)
summary(model) # melihat tnya significant atau enggak

#lagged 1 period
x = TSR.training[2:39,]
xt1 = TSR.training$Jumlah[1:38]
lag1 = data.frame(x, xt1)

model2 = lm(Jumlah~., data=lag1)
summary(model2)
TSR.error <- resid(model2)

# lagged 2 period
x = TSR.training[3:39,]
xt1 = TSR.training$Jumlah[2:38]
xt2 = TSR.training$Jumlah[1:37]
lag2 = data.frame(x, xt1, xt2)
model3 = lm(Jumlah~., data=lag2)
summary(model3)
# Model Evaluation
TSR.MSE <- mean(TSR.error^2)
TSR.RMSE <- sqrt(TSR.MSE)
TSR.MAPE <- mean(abs((TSR.error) / TSR.training$Jumlah[2:nrow(TSR.training)])) * 100
TSR.AIC <- AIC(model2)
TSR.MPE <- mean(TSR.error/TSR.training$Jumlah[2:nrow(TSR.training)]) * 100
TSR.MPE
output <- sprintf("MSE: %.2f\nMAPE: %.2f%%\nRMSE: %.2f\nAIC: %.2f", TSR.MSE, TSR.MAPE, TSR.RMSE, TSR.AIC)
cat(output, "\n")

# lagged for testing
x = TSR.testing[2:9,]
xt1 = TSR.testing$Jumlah[1:8]
lag1.test = data.frame(x, xt1)

yhat<-predict.lm(model2, newdata = lag1.test)
TSR.testError <- TSR.testing$Jumlah[2:nrow(TSR.testing)] - yhat


# Model Evaluation 
MSE <- mean(TSR.testError^2)
MAPE <- mean(abs((TSR.testError) / TSR.testing$Jumlah[2:nrow(TSR.testing)])) * 100
RMSE <- sqrt(MSE)
MPE <- mean(TSR.testError/TSR.testing$Jumlah[2:nrow(TSR.testing)]) * 100
MPE
output <- sprintf("MSE: %.2f\nMAPE: %.2f%%\nRMSE: %.2f\n", MSE, MAPE, RMSE)
cat(output, "\n")

## TESTING REGRESI
#berlaku asumsi untuk Linear Regression
library(nortest)
library(lmtest)

#Distributed Normally
# Ho = Distributed Normally
# H1 = Not DN
testing1 <- lillie.test(TSR.error)

#AutoCorrelation
# Ho = No AutoCorrelation
# H1 = AutoCorrelation
testing2 <- dwtest(model2)

#Homoscedasticitas
# Ho = Homoscedastic
# H1 = Heteroscedastic
testing3 <- bptest(model2)


output <- sprintf("DN: %f\nAutocorrelation: %f\nHomoscedastic: %f\n", testing1$p.value, testing2$p.value, testing3$p.value)
cat(output, "\n")


#### ARIMA ####
# Training and Testing for ARIMA 
arima_training = window(data.ts, start=c(2020,4), end=c(2023,6))
# Test set
arima_testing = window(data.ts,  start=c(2023,7), end=c(2024,3))

library(car)

### ARIMA Testing 
## Stationary towards Variance
summary(powerTransform(data.ts))
# Stationary toward Mean
adf.test(data.ts)
dif1 <- diff(data.ts, differences = 1)
adf.test(dif1)
dif2 <- diff(data.ts, differences = 2)
adf.test(dif2)

# pdq for ARIMA
acf_result <- acf(dif2, lag.max = 24)
pacf_result<- pacf(dif2, lag.max =24)
par(mfrow = c(1, 2))
plot(acf_result, main = "ACF")
plot(pacf_result, main = "PACF")
par(mfrow = c(1,1))

#arima model
arima_model <- arima(arima_training, order=c(2,0,1), seasonal=list(order=c(0,1,0), period=12)) 
coeftest(arima_model)
arima_model
arima_model.error <- resid(arima_model)


arima.MSE <- mean(arima_model.error^2)
arima.RMSE <- sqrt(mean(arima_model.error^2))
arima.MAPE <- mean(abs(arima_model.error / arima_training))*100
arima.MPE <- mean(arima_model.error /arima_training ) * 100
output <- sprintf("MSE: %.2f\nMAPE: %.2f%%\nRMSE: %.2f\n", arima.MSE, arima.MAPE, arima.RMSE)
cat(output, "\n")

## model testing
Box.test(arima_model.error, type = "Ljung-Box")
lillie.test(arima_model.error)

# Make predictions on the testing data
arima_forecast <- forecast(arima_model, h = length(arima_testing))
yhat.arima <- arima_forecast$mean
error <- arima_testing - yhat.arima

MSE <- mean(error^2)
RMSE <- sqrt(mean(error^2))
MAPE <- mean(abs(error/arima_testing)) * 100
MPE <- mean(error/arima_testing) *100
MPE
output <- sprintf("MSE: %.2f\nMAPE: %.2f%%\nRMSE: %.2f\n", MSE, MAPE, RMSE)
cat(output, "\n")


#### Neural Network ####
lag_transform <- function(x, k=1) {
  lagged <- c(rep(NA, k), x[1:(length(x) - k)])
  DF <- data.frame(lagged, x)
  DF <- na.omit(DF)
  return(DF)
}

scaled_data <- scale(data.ts)
data.NN <- lag_transform(scaled_data,1)
train.NN <- data.NN[1:38, ]
test.NN <- data.NN[39:47, ]
nn <- neuralnet("Jumlah ~ lagged", data=train.NN, hidden=c(14,9))

# Plot the neural network
#plot(nn)

#model evaluation Training
train_features <- subset(train.NN, select = -Jumlah)
train_predictions <- compute(nn, train_features)$net.result

train_predictions <- train_predictions * attr(scaled_data, 'scaled:scale') + attr(scaled_data, 'scaled:center')
actual_train <- train.NN$Jumlah * attr(scaled_data, 'scaled:scale') + attr(scaled_data, 'scaled:center')
residuals <- actual_train - train_predictions

mseTrain.NN <- mean(residuals^2)
rmseTrain.NN <- sqrt(mseTrain.NN)
mapeTrain.NN <- mean(abs(residuals/actual_train)) * 100
mpeTrain.NN <- mean(residuals/actual_train) * 100
output <- sprintf("MSE: %.2f\nMAPE: %.2f%%\nRMSE: %.2f\n", mseTrain.NN, mapeTrain.NN, rmseTrain.NN)
cat(output, "\n")

#model evaluation Testing
test_features <- subset(test.NN, select = -Jumlah)
test_predictions <- compute(nn, test_features)$net.result
test_predictions <- test_predictions * attr(scaled_data, 'scaled:scale') + attr(scaled_data, 'scaled:center')

actual_test <- test.NN$Jumlah * attr(scaled_data, 'scaled:scale') + attr(scaled_data, 'scaled:center')
errorTest.NN <- actual_test - test_predictions

mseTest.NN <- mean(errorTest.NN^2)
rmseTest.NN <- sqrt(mseTest.NN)
mapeTest.NN <- mean(abs(errorTest.NN/actual_test)) * 100
mpeTest.NN <- mean(errorTest.NN/actual_test) * 100
mpeTest.NN
output <- sprintf("MSE: %.2f\nMAPE: %.2f%%\nRMSE: %.2f\n", mseTest.NN, mapeTest.NN, rmseTest.NN)
cat(output, "\n")