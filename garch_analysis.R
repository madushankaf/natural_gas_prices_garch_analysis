library(fGarch)  # Load the fGarch library for GARCH modeling
library(rugarch)  # Load the rugarch library for GARCH modeling
library(tseries)  # Load the tseries library for time series analysis
library(readxl)  # Load the readxl library for reading Excel files
library(ggplot2)  # Load the ggplot2 library for data visualization
library(tidyverse)  # Load the tidyverse library for data manipulation
library(nortsTest)# Load the nortest library for normality tests


# Read the Excel file for daily price data
daily_price_data <- read_excel("RNGWHHDd.xls", sheet = "Data 1", skip = 2)

# Read the Excel file for weekly price data
weekly_price_data <- read_excel("RNGWHHDw.xls", sheet = "Data 1", skip = 2)

head(daily_price_data)  # Display the first few rows of daily price data
head(weekly_price_data)  # Display the first few rows of weekly price data

# Rename price columns to 'Price' for daily and weekly data
colnames(daily_price_data)[2] <- "Price"
colnames(weekly_price_data)[2] <- "Price"

# Count the number of missing values in daily price data
sum(is.na(daily_price_data$Price))  
# Count the number of missing values in weekly price data
sum(is.na(weekly_price_data$Price))  

# Fill missing values in daily_price_data using the fill() function
#Assuming that the missing values are equal to previous available value.
daily_price_data <- daily_price_data |>
    fill(everything())

# Fill missing values in weekly_price_data using the fill() function
#Assuming that the missing values are equal to previous available value.
weekly_price_data <- weekly_price_data |>
    fill(everything())

# Plot daily natural gas prices
ggplot(data = daily_price_data, aes(x = Date, y = Price)) +
    geom_line() +
    labs(title = "Daily Natural Gas Prices",
         x = "Date",
         y = "Price")

# Plot weekly natural gas prices
ggplot(data = weekly_price_data, aes(x = Date, y = Price)) +
    geom_line() +
    labs(title = "Weekly Natural Gas Prices",
         x = "Date",
         y = "Price")

#Simple returns exhibits 'heteroskedasticity'- meaning voltality is not constant', to avoid that log returns are used.

# Calculate log returns of daily data
daily_price_data$log_returns <- c(NA,diff(log(daily_price_data$Price)))

# Calculate log returns of weekly data
weekly_price_data$log_returns <- c(NA,diff(log(weekly_price_data$Price)))

ggplot(data = daily_price_data, aes(x = Date, y = log_returns)) +
    geom_line() +
    labs(title = "Daily Log Returns",
         x = "Date",
         y = "Log Returns")

ggplot(data = weekly_price_data, aes(x = Date, y = log_returns)) +
    geom_line() +
    labs(title = "Weekly Log Returns",
         x = "Date",
         y = "Log Returns")


# Summary statistics of the daily log returns
round(mean(daily_price_data$log_returns, na.rm = TRUE)*100, 7)  # Calculate the mean of daily log returns
round(sd(daily_price_data$log_returns, na.rm = TRUE)*100, 7)  # Calculate the standard deviation of daily log returns
round(median(daily_price_data$log_returns, na.rm = TRUE)*100, 7)  # Calculate the median of daily log returns
round(var(daily_price_data$log_returns, na.rm = TRUE)*100, 7)  # Calculate the variance of daily log returns
sum(is.na(daily_price_data$log_returns))  # Count the number of missing values in daily log returns

# Summary statistics of the weekly log returns
round(mean(weekly_price_data$log_returns, na.rm = TRUE)*100, 7)  # Calculate the mean of weekly log returns
round(sd(weekly_price_data$log_returns, na.rm = TRUE)*100, 7)  # Calculate the standard deviation of weekly log returns
round(median(weekly_price_data$log_returns, na.rm = TRUE)*100, 7)  # Calculate the median of weekly log returns
round(var(weekly_price_data$log_returns, na.rm = TRUE)*100, 7)  # Calculate the variance of weekly log returns
sum(is.na(weekly_price_data$log_returns))  # Count the number of missing values in weekly log returns


# Perform Augmented Dickey-Fuller test on daily log returns
adf.test(daily_price_data$log_returns[2:nrow(daily_price_data)])  

# Perform Augmented Dickey-Fuller test on weekly log returns
adf.test(weekly_price_data$log_returns[2:nrow(weekly_price_data)])


# Perform ARCH test on daily log returns
arch.test(daily_price_data$log_returns[2:nrow(daily_price_data)])

# Perform ARCH test on weekly log returns
arch.test(weekly_price_data$log_returns[2:nrow(weekly_price_data)])


daily_data_garch_model_f_garch <- garchFit( data = daily_price_data$log_returns[2:nrow(daily_price_data)], trace = FALSE)
daily_data_garch_model_f_garch


garch.m.spec_daily <- ugarchspec( variance.model = list(model = "sGARCH",garchorder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,archm=TRUE, archpow = 1), distribution.model = "norm")
garch.m.fit_daily <- ugarchfit(spec = garch.m.spec_daily, data = daily_price_data$log_returns[2:nrow(daily_price_data)])


garch.m.fit_daily

daily_price_data$conditional_var <-c(NA, daily_data_garch_model_f_garch@sigma.t**2)

ggplot(data = daily_price_data, aes(x = Date, y = conditional_var)) +
    geom_line() +
    labs(title = "Conditional Variance",
         x = "Date",
         y = "Conditional Variance")


weekly_data_garch_model_f_garch <- garchFit( data = weekly_price_data$log_returns[2:nrow(weekly_price_data)], trace = FALSE)
weekly_data_garch_model_f_garch

garch.m.spec_weekly <- ugarchspec( variance.model = list(model = "sGARCH",garchorder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,archm=TRUE, archpow = 1), distribution.model = "norm")
garch.m.fit_weekly <- ugarchfit(spec = garch.m.spec_weekly, data = weekly_price_data$log_returns[2:nrow(weekly_price_data)])

garch.m.fit_weekly

weekly_price_data$conditional_var <-c(NA, weekly_data_garch_model_f_garch@sigma.t**2)

ggplot(data = weekly_price_data, aes(x = Date, y = conditional_var)) +
    geom_line() +
    labs(title = "Conditional Variance",
         x = "Date",
         y = "Conditional Variance")



