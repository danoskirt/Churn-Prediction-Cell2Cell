import requests
import json

# Set the API endpoint URL
url = "http://api.marketstack.com/v1/eod"

# Set the API parameters
params = {
    "access_key": "5880333f0e8a678083a4af7762c83c80",
    "symbols": "AAPL",
    "date_from": "2022-01-01",
    "date_to": "2023-02-19"
}

# Send a GET request to the API and retrieve the response
response = requests.get(url, params=params)

# Parse the JSON data returned by the API
data = json.loads(response.text)

# Store the data in a variable for further analysis
stock_data = data["data"]

import pandas as pd

# convert the stock_data list to a DataFrame
stock_data = pd.DataFrame(stock_data)

stock_d = stock_data

# print the first 5 rows of the DataFrame
print(stock_data.head())

stock_data.describe()

import matplotlib.pyplot as plt

# plot the stock prices
plt.plot(stock_data['date'], stock_data['close'])
plt.xlabel('date')
plt.ylabel('close')
plt.title('AAPL Stock Prices')
plt.show()

# calculate daily returns
daily_returns = stock_data['close'].pct_change()
print(daily_returns.head())

# calculate various financial metrics
average_daily_return = daily_returns.mean()
volatility = daily_returns.std()
sharpe_ratio = average_daily_return / volatility
print('Average Daily Return:', average_daily_return)
print('Volatility:', volatility)
print('Sharpe Ratio:', sharpe_ratio)


#pip install mplfinance

import pandas as pd
import mplfinance as mpf

# convert the dataframe to the required format for mplfinance library
stock_data_mpl = stock_data.reset_index()
stock_data_mpl['date'] = pd.to_datetime(stock_data_mpl['date'])
stock_data_mpl = stock_data_mpl.set_index('date')
stock_data_mpl = stock_data_mpl.rename(columns={'open': 'Open', 'high': 'High', 'low': 'Low', 'close': 'Close', 'volume': 'Volume'})

# plot the candlestick chart
mpf.plot(stock_data_mpl, type='candle', volume=True, mav=(3, 6, 9), figratio=(20,10))


import matplotlib.pyplot as plt

# plot the time series of closing prices
plt.figure(figsize=(20, 10))
plt.plot(stock_data['close'])
plt.title('Time Series of Closing Prices')
plt.xlabel('Date')
plt.ylabel('Closing Price')
plt.show()



import seaborn as sns

# calculate the daily returns
stock_data['daily_return'] = stock_data['close'].pct_change()

# plot the histograms of the daily returns
plt.figure(figsize=(20, 10))
sns.histplot(stock_data['daily_return'].dropna(), bins=50, kde=True)
plt.title('Histograms of Daily Returns')
plt.xlabel('Daily Return')
plt.ylabel('Frequency')
plt.show()


# Split data into training and testing sets
train_data = stock_data.iloc[:int(0.7*len(stock_data))]
test_data = stock_data.iloc[int(0.7*len(stock_data)):]

# Print the lengths of the training and testing sets
print('Training set length:', len(train_data))
print('Testing set length:', len(test_data))


#pip install fbprophet

# Prepare data for Prophet

stock_data['date'] = pd.to_datetime(stock_data['date'])
stock_data = stock_data[['date', 'close']]
stock_data = stock_data.rename(columns={'date': 'ds', 'close': 'y'})

# Convert the 'ds' column to a timezone-naive datetime object
stock_data['ds'] = pd.to_datetime(stock_data['ds']).dt.tz_localize(None)

# Create and fit Prophet model
from prophet import Prophet
model = Prophet()
model.fit(stock_data)

# Make predictions
future = model.make_future_dataframe(periods=7)
forecast = model.predict(future)

# Plot forecast
fig = model.plot(forecast)


# Preprocessing

df = stock_d

df.isnull().sum()
df.dropna(inplace=True)
df = df.reset_index(drop=True)
df = df.sort_values('date')
df['date'] = pd.to_datetime(df['date'])
df.set_index('date', inplace=True)

df = df.drop(['exchange', 'symbol'], axis=1)





