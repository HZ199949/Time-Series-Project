
library(forecast)
library(ggplot2)
library(dplyr)
library(xts)
library(fma)
library(tseries)
```


```{r, message=FALSE}
## Importing the data
setwd("/Users/haroonzia/Downloads")
# Loading the data set
avocado <- read.csv("avocado.csv")
## Selecting West and Conventional Type (reasoning is explained in the report)
avocado.W<-avocado[avocado$type=="conventional"&avocado$region=="West",]

```

```{r, message=FALSE}
# Converting Date variable to Date format as it is necessary in R
avocado.W$Date <- as.Date(avocado.W$Date)
#Converting into a Timeseries
avocado.ts <- ts(avocado.W$AveragePrice, start=c(2015, 0), end=c(2018, 10), frequency=52)


# Filtering training dataset (observations until the end of 2017)
training_data <- window(avocado.ts, end=c(2017, 52)) # Last index of 2017

# Filtering test dataset (observations from January 2018 to March 2018)
test_data <- window(avocado.ts, start=c(2018, 1), end=c(2018, 10))

#Checking the splitted data sets
tail(test_data)
tail(training_data)
```

```{r}
#Generating Summary Statistics for the Training Data
summary(training_data)



## EDA OF ENTIRE DATA SET TO CHOOSE THE REIGON

# 'avocado' is our original dataframe with all the columns
# First, we ensure the Date column in avocado is of type Date
avocado$Date <- as.Date(avocado$Date)

# Created the training data set again including: 
# region and type for the entire Dataset and not only West
training_data_full <- avocado %>% 
  filter(Date <= as.Date("2017-12-31")) %>%
  select(Date, AveragePrice, region, type, Total.Volume)

# Summary statistics for each region and type with the .groups argument
summary_stats <- training_data_full %>%
  group_by(region, type) %>%
  summarise(
    AveragePrice_mean = mean(AveragePrice, na.rm = TRUE),
    AveragePrice_median = median(AveragePrice, na.rm = TRUE),
    AveragePrice_sd = sd(AveragePrice, na.rm = TRUE),
    .groups = 'drop'  
  )
#Plot 1: Mean Average Prices by Region and Type
# General plot of average prices

ggplot(summary_stats, aes(x = region, y = AveragePrice_mean, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("organic" = "skyblue", "conventional" = "brown")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Plot (1): Mean Average Prices by Region and Type", x = "Region", 
       y = "Mean Average Price (USD)")

# Calculating the standard deviation of prices and mean of total volume by region
region_stats <- training_data_full %>%
  group_by(region) %>%
  summarize(
    PriceVolatility = sd(AveragePrice, na.rm = TRUE),
    AvgVolume = mean(Total.Volume, na.rm = TRUE),
    .groups = 'drop'  # Ensures that the output is no longer grouped
  ) %>%
  arrange(desc(PriceVolatility), desc(AvgVolume))

# Plotting to visualize the results
# Plot 2: Plot for Price Volatility by Region
ggplot(region_stats, aes(x = reorder(region, PriceVolatility), y = PriceVolatility)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Plot (2): Price Volatility by Region", 
       x = "Region", y = "Standard Deviation of Price")

# Plot 3: Plot for Average Production Volume by Region
ggplot(region_stats, aes(x = reorder(region, AvgVolume), y = AvgVolume)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Plot (3): Average Production Volume by Region", x = "Region", 
       y = "Average Volume")


## Plot 4: 
# Boxplot of Average Price by Type
ggplot(avocado.W, aes(x = type, y = AveragePrice, fill = type)) +
  geom_boxplot() +
  labs(title = " Plot (4): Boxplot of Average Price by Type in West",
       x = "Type",
       y = "Average Price (USD)",
       fill = "Type")

## AFTER RUNNING EDA ON THE ENTIRE TRAINING DATASET WE: #CHOOSE WEST DUE TO ITS SIGNIFICANT AVERAGE PRODUCTION VOLUME 

## EDA ON TRAINING DATA 

#PLOT: 5
# Time series plot of Average Price
autoplot(training_data) +
  labs(title = "Plot (5): Time Series Plot of Average Price",
       x = "Date",
       y = "Average Price (USD)")


# Plot 6: Histogram of Average Price
ggplot(data.frame(training_data), aes(x = training_data)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Plot (6): Histogram of Average Price",
       x = "Average Price (USD)",
       y = "Frequency")





# Plot: 7 qqplot
{qqnorm(avocado.ts)
qqline(avocado.ts,distribution=qnorm)}
```

```{r}
#SINCE OUR DATA SET HAS MISSING VALUES WE DO LINEAR INTERPOLATION

#Checking for the number of missing values
sum(is.na(training_data))
sum(is.na(test_data))
#Linear Interpolation by na.approx()
interpolated_training <- na.approx(training_data)
interpolated_test <- na.approx(test_data)
# Checking for any Remaining Missing values
sum(is.na(interpolated_test))
sum(is.na(interpolated_training))

#SINCE WE HAVE NOW INTERPOLATED WE WILL SEE OUR ACF, PACF: #PLOTS AND ALSO CHECK FOR STATIONARITY AND SEASONALITY.

#ACF AND PACF :
#Plot: 8 and 9 
Acf(interpolated_training)
Pacf(interpolated_training)

#ADF  TEST TO CHECK FOR STATIONARITY.
adf.test(interpolated_training)


#Plot 10:SEASONAL DECOMPOSITION PLOT: 
plot(decompose(interpolated_training))
```


```{r}
#Since Seasonality was present as evident by our SD plot we adjusted it.
SS<-seasadj(decompose(interpolated_training))
```



```{r}
#Section 1: ARIMA MODELS AND D VALUES =1 for differencing
arima1<-arima(SS,order=c(1,1,1))
arima2<-arima(SS,order=c(0,1,1))
arima3<-arima(SS,order=c(1,1,0), seasonal = c(1,0,0))
arima4<-arima(SS,order=c(0,1,1), seasonal = c(0,0,1))
arima5<-arima(SS,order=c(1,0,0))
arima6<-arima(SS,order =c(0,1,3))
AIC.ar1<-AIC(arima1)
AIC.ar2<-AIC(arima2)
AIC.ar3<-AIC(arima3)
AIC.ar4<-AIC(arima4)
AIC.ar5<-AIC(arima5)
AIC.ar6<-AIC(arima6)
AIC.ar1;AIC.ar2;AIC.ar3;AIC.ar4;AIC.ar5;AIC.ar6
#SINCE ARIMA4 had the lowest AIC value we choose that.

### checking residuals
#PLOT: 11
checkresiduals(arima4)
#PLOT: 12
pacf(arima4$residuals)

# forecast the testing set to find the performance metrics
forcast_arima4 <-forecast(arima4,h=10)

forcast_arima4

#PLOT: 13
plot(forcast_arima4, main="Plot(13): Forecast ARIMA(0,1,1)(0,0,1)", xlab="Time", ylab="Average Price")
lines(interpolated_test, col= 'red')
accuracy(forcast_arima4$mean,interpolated_test)
```


```{r}
# Section 2: AUTO ARIMA Model 
AUTO <- auto.arima(SS,d=1) #d is first difference 
AUTO
checkresiduals(AUTO)
pacf(AUTO$residuals)

forcast_ar1 <-forecast(AUTO,h=10)
forcast_ar1

#PLOT: 14
plot(forcast_ar1, main="PLot(14): Forecast ARIMA(1,1,0)(1,0,0)", xlab="Time", ylab="Average Price")
lines(interpolated_test, col= 'red')
accuracy(forcast_ar1$mean,interpolated_test)
```
