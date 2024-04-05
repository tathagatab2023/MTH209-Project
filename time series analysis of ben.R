df=Top_Beneficiary_Banks
a=unique(df$UPI_Beneficiary_Bank)
df2=df[which(df$UPI_Beneficiary_Bank==a[2]),]
df2=as.data.frame(df2)[,c(2,4,5)]
# Create a scatter plot using ggplot
ggplot(df2, aes(x = 1:30, y = df2$Total_Volume)) +
  geom_point() +  # Add points for the scatter plot
  labs(x = "X-axis Label", y = "Y-axis Label", title = "Scatter Plot of Ben") +
  theme_minimal()  # Use a minimal theme for the plot
# Example time series data (replace this with your actual data)
ts_data_ben <- ts(c(as.numeric(df2$Total_Volume)),
              start = c(2021, 8), frequency = 12)

### decompose ts data
decompose_result_2=decompose(ts_data_ben)
plot(decompose_result_2)

# Perform Augmented Dickey-Fuller (ADF) Test
adf_test_result <- adf.test(decompose_result_2$random[7:24])
print(adf_test_result)
## as it is not stationary for alpha0.01 make it stationary.

# Assuming your original time series data is stored in 'ts_data_ben'
# You can create a new column for the differenced values

# Create a new column for differenced data
ts_data_ben_diff <- c(NA, diff(ts_data_ben))
# Remove rows with NA values from ts_data$differenced
ts_data_ben_diff <- ts_data_ben_diff[!is.na(ts_data_ben_diff)]

# Plot the differenced data
plot(ts_data_ben_diff, type = "l", xlab = "Time", ylab = "Differenced Total Volume", main = "Differenced Time Series of Ben")

# Check stationarity using ADF test
adf_result <- ur.df(ts_data_ben_diff, type = "trend", lags = 10)
summary(adf_result)


# Plot ACF and PACF
ggtsdisplay(ts_data_ben_diff,main="pacf of ben")

plot(decompose_result_2$seasonal)
plot(ts_data_ben_diff)




# Combine plots using grid.arrange (assuming you have the gridExtra package installed)
# Perform time series decomposition
decomposed_ts <- decompose(ts_data_ben)

# Extract decomposed components
trend <- decomposed_ts$trend
seasonal <- decomposed_ts$seasonal
random <- decomposed_ts$random

# Create separate plots for each component and original time series in ggplot 
plot_original <- ggplot(data.frame(Date = time(ts_data_ben), Original = ts_data_ben), aes(x = Date, y = Original)) +
  geom_line() +
  labs(title = "Original Time Series of Ben", x = "Date", y = "Value") +
  theme_minimal()

plot_trend <- ggplot(data.frame(Date = time(ts_data_ben), Trend = trend), aes(x = Date, y = Trend)) +
  geom_line() +
  labs(title = "Trend Component", x = "Date", y = "Value") +
  theme_minimal()

plot_seasonal <- ggplot(data.frame(Date = time(ts_data_ben), Seasonal = seasonal), aes(x = Date, y = Seasonal)) +
  geom_line() +
  labs(title = "Seasonal Component", x = "Date", y = "Value") +
  theme_minimal()

plot_random <- ggplot(data.frame(Date = time(ts_data_ben), Random = random), aes(x = Date, y = Random)) +
  geom_line() +
  labs(title = "Random Component", x = "Date", y = "Value") +
  theme_minimal()


grid.arrange(plot_original,plot_trend, plot_seasonal, plot_random,
             ncol = 2, nrow = 2)

write.xlsx(ts_data_ben,file="time_series_data_ben.xlsx")

