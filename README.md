# Foreign-Tourist-Time-Series-Analysis
![Indonesia_Tourism](https://github.com/user-attachments/assets/8ae4c2ac-392e-42f5-87c6-36700eb257c7)

A Time Series project for analyzing the resurgence of foreign tourists in Indonesia post COVID-19 using Naive, Exponential Smoothing, Time Series Regression, ARIMA, and Neural Network methods. 

# Workflow
## Data Collection
Data was compiled from multiple datasets of numbers of foreign tourists from the year of 2020 until 2024 available and obtained from annual open publication data issued by the Ministry of Tourism and Creative Economy/Tourism and Creative Economy Agency Republic of Indonesia.

## Data Preprocessing
1. Combined the datasets of numbers of foreign tourists from year 2020 until 2024 into one dataset.
2. Created two separate Microsoft Excel files assigned for R analysis and manual calculations based on time series equations.

## Train Model
1. Created a Time Series plot to determine whether the data contains seasonality or not.
2. Produced  Time Series models using Naive Seasonal and Triple Exponential Smoothing model by manual calculations.
3. Produced  Time Series models using Time Series Regression, ARIMA, and Neural Network model in R-Studio.
4. Obtained the metrics of Mean Percentage Error (MPE), Mean Square Error (MSE), Root Mean Square Error (RMSE), and Mean Absolute Percentage Error (MAPE) for each model.
5. Compared each model available by using the metrics.

## Result (Model Evaluation)
![image](https://github.com/user-attachments/assets/fe0bc5bc-f957-48eb-8f3c-52caeb6d495e)
![image](https://github.com/user-attachments/assets/1f644cba-ba1d-4082-8224-8409fac62a76)


## Conclusion
Due to the nature of the research for predicting foreign tourist resurgence in Indonesia, the Neural Network model is considered to be the best Time Series model for this task. This conclusion is further proven by the fact that the Mean Percentage Error (MPE) metric is highly responsible for determining the overestimating and underestimating of the forecast result.
