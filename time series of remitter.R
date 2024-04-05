library(tseries)
library(forecast)
library(ggplot2)
library(urca)
library(lubridate) ### for dates
library(gridExtra)# Combine plots using grid.arrange (assuming you have the gridExtra package installed)



df= as.data.frame(Remitter_Bank_Data)
df2=df[,c(2,4,5)]
a=unique(df$UPI_Remitter_Bank)
sbi_data= df2[which(df2$UPI_Remitter_Bank == a[1]),]

as.data.frame(sbi_data)

# Create a scatter plot using ggplot
ggplot(sbi_data, aes(x = 1:30, y = sbi_data$Total_Volume)) +
  geom_point() +  # Add points for the scatter plot
  labs(x = "X-axis Label", y = "Y-axis Label", title = "Scatter plot of rem") +
  theme_minimal()  # Use a minimal theme for the plot



# Example time series data (replace this with your actual data)
ts_data <- ts(c(1018.15, 1036.45, 1193.24, 1176.24, 1296.56, 1291.88, 1242.45, 1482.61, 1540.38, 1644.91,
                1625.09, 1709.44, 1793.55, 1865.75, 1972.78, 1951.85, 2128.94, 2153.20, 1987.68, 2260.05,
                2298.06, 2427.45, 2415.09, 2574.67, 2707.09, 2730.35, 2930.95, 2861.65, 3073.18, 3128.97),
              start = c(2021, 8), frequency = 12)



### decompose ts data
decompose_result=decompose(ts_data)
plot(decompose_result)
# Perform Augmented Dickey-Fuller (ADF) Test
adf_test_result <- adf.test(decompose_result$random[7:24])
print(adf_test_result)
## as it is not stationary make it stationary.



# Assuming your original time series data is stored in 'ts_data'
# You can create a new column for the differenced values

# Create a new column for differenced data
ts_data_diff <- c(NA, diff(ts_data))
# Remove rows with NA values from ts_data$differenced
ts_data_diff <- ts_data_diff[!is.na(ts_data_diff)]

# Plot the differenced data
plot(ts_data_diff, type = "l", xlab = "Time", ylab = "Differenced Total Volume", main = "Differenced Time Series")

# Check stationarity using ADF test
adf_result <- ur.df(ts_data_diff, type = "trend", lags = 10)
summary(adf_result)


# Plot ACF and PACF
ggtsdisplay(ts_data_diff,main="PACF of remitter")


# Perform time series decomposition
decomposed_ts <- decompose(ts_data)

# Extract decomposed components
trend <- decomposed_ts$trend
seasonal <- decomposed_ts$seasonal
random <- decomposed_ts$random

# Create separate plots for each component and original time series in ggplot 
plot_original <- ggplot(data.frame(Date = time(ts_data), Original = ts_data), aes(x = Date, y = Original)) +
  geom_line() +
  labs(title = "Original Time Series of Rem", x = "Date", y = "Value") +
  theme_minimal()

plot_trend <- ggplot(data.frame(Date = time(ts_data), Trend = trend), aes(x = Date, y = Trend)) +
  geom_line() +
  labs(title = "Trend Component", x = "Date", y = "Value") +
  theme_minimal()

plot_seasonal <- ggplot(data.frame(Date = time(ts_data), Seasonal = seasonal), aes(x = Date, y = Seasonal)) +
  geom_line() +
  labs(title = "Seasonal Component", x = "Date", y = "Value") +
  theme_minimal()

plot_random <- ggplot(data.frame(Date = time(ts_data), Random = random), aes(x = Date, y = Random)) +
  geom_line() +
  labs(title = "Random Component", x = "Date", y = "Value") +
  theme_minimal()

grid.arrange(plot_original,plot_trend, plot_seasonal, plot_random,
             +              ncol = 2, nrow = 2)

grid.arrange(plot_original,plot_trend, plot_seasonal, plot_random,
             ncol = 2, nrow = 2)


write.xlsx(ts_data,file="time_series_remitter_data.xlsx")
