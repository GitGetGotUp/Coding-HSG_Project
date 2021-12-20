#Introduction to Programming@HSG Project 2021               
#Niklas Halfar and Florian Lerf
#Forecasting US Consumption expenditure using Machine Learning
rm(list = ls())
#0 PAckages and Libraries#######################################################

#The installing of the packages only needs to be done once, the packages will be loaded
#from CRAN to your local machine:
install.packages(c("tidyverse", "tsibble", "dplyr", "patchwork", "hrbrthemes", "feasts", "stats",
                   "GGally", "tsibbledata", "fable", "forecast", "TSA", "fable.prophet", "gridExtra",
                   "RCurl"))

#When the packages are installed, we have to call them into the IDE with library. 
#This has to be done every time when starting an R session. 

library(tidyverse)
library(tsibble)
library(dplyr)
library(patchwork)
library(hrbrthemes)
library(feasts)
library(GGally)
library(tsibbledata)
library(fable)
library(forecast)
library(TSA)
library(fable.prophet)
library(gridExtra)
library(stats)
library(RCurl)




#1 Problem Definition###########################################################
#Our goal is to use machine learning tools to forecast US Consumption. 
#Forecasting the consumption can help economists with policy decisions and
#businesses in their strategic planning. 


#2 Data Cleaning & preparation##################################################
df <- economics

#From the dataset economics which we labelled df we create a tsibble, which is similar 
#to the R class tibble but used for time series.
#the month is the index as our data is recorded monthly.
df_ts <- df %>%
  mutate(Month = yearmonth(date)) %>%
  as_tsibble(index = Month)

df_ts

#we rename the columns for easier identification
colnames(df_ts) <- c("date", "Consumption", "Population", "Savingsrate", "UnemploymentDuration", "Unemployed", "Month")
df_ts

#Consumption was in billions, Unemployed and Population in thousands 
#so we adjust by multiplying. 
df_ts <- df_ts %>%
  mutate(Consumption = Consumption *1000000000)%>%
  mutate(Unemployed = Unemployed*1000)%>%
  mutate(Population = Population*1000)

#We get the consumption PER person as we divide by population. 
df_ts <- df_ts %>%
  mutate(Consumption = Consumption/Population)


#Analysis of the target variable: Consumption
range(df_ts$Consumption)
mean(df_ts$Consumption)

#This gives us some idea of our data as we see the range and mean. 

#The monetary amount of the Consumption data was more valuable 50 years ago because of inflation. 
#So we have to adjust to get the real value. 
#We use the CPI from the global_economy data and join it to our data set. 
#We then divide monetary values by the CPI and multiply with 100. 
#Now we have adjusted the consumption for inflation. 
inflation_growth <- global_economy %>%
  filter(Code == "USA")%>%
  select(Year, CPI)

#we convert it again to a tsibble
inflation_growth <- as_tsibble(inflation_growth, index = Year)

#we mutate the Year with substring to loose the month.
df_ts <- df_ts %>%
  mutate(Year = substr(date, 1,4))

#We still have two different classes for the two date variables in the inflation and original dataets
#numeric and character. 
class(inflation_growth$Year)
class(df_ts$Year)

#We want them both to be a Date. 
df_ts$Year <- as.Date(df_ts$Year, format = "%Y")
inflation_growth$Year <- as.character(inflation_growth$Year)
inflation_growth$Year <- as.Date(inflation_growth$Year, format = "%Y")

class(inflation_growth$Year)
class(df_ts$Year)
#Now both are class Date

#We now join the two datasets and mutate consumption so it is adjusted for inflation. 
df_ts <- df_ts %>%
  left_join(inflation_growth, by = "Year")

df_ts <- df_ts %>%
  mutate(Consumption = Consumption/CPI*100)

range(df_ts$Consumption)
mean(df_ts$Consumption)
#The Consumption in 1967 has changed from 2549$ to 16656$. This is because 2549$
#Would have been roughly 16656$ in 2015.


#We thougt that probably Consumption is higher in times of good economics growth and
#lower in times of less growth or decline. So we wanted to add an indicator into our 
#data that tells us if the economy is in a boom period or if we are in a recession. 
#We use binary encoding of the recession variable. 1 stands for a recessionary period
#and 0 is any other period. We downloaded the Recession dataset from the FRED St. Lous
#in a csv format and joined it to our existing dataset. 

#load in the dataset from our Github Repository:
url <- ("https://raw.githubusercontent.com/GitGetGotUp/Coding-HSG_Project/main/us_recessions.csv")
us_recession<- read.csv(url)

#switch the class of the date column to Date. 
class(us_recession$DATE)
us_recession$DATE <- as.Date(us_recession$DATE, format = "%Y-%m-%d")

#convert to tsibble
us_recession <- as_tsibble(us_recession, index = DATE)

#join it to our original dataset
df_ts <- df_ts %>%
  left_join(us_recession, by = c("date" = "DATE"))

#We now only select the needed columns and drop the rest
df_ts <- df_ts %>%
  select(-CPI)

rm(url, inflation_growth)
#The data cleaning and preparation is now complete. 


#3 Preliminary (exploratory) analysis###########################################

#some simple Time Series graph
ggplot(df_ts) +
  geom_line(mapping = aes(x = date, y = Consumption), colour = "red")+
  labs(title = "Consumption in the US Economy", 
       y = "Consumption expenditure per person", 
       x = "Date")

#We can see there are some swings in the consumption graph.  

ggplot(df_ts) +
  geom_line(mapping = aes(x = date, y = Savingsrate), colour = "red")+
  labs(title = "Savingsrate in the US Economy", 
       y = "Savingsrate (red)", 
       x = "Date")

#Savings seems very volatile
ggplot(df_ts) +
  geom_line(mapping = aes(x = date, y = UnemploymentDuration), colour = "red")+
  labs(title = "unemployment duration in weeks per year (median) in the US Economy", 
       y = "unemployment duration", 
       x = "Date")

#some boom bust pattern over many years, and maybe some seasonality over each year are present

#4 Time Series Decomposition using LOESS#########################################
#We will now try to find patterns in the time series.
#There are three main components:
#Trend
#Season
#Remainder

#For further information to the decomposition refer to our project paper chapter 4. 

#DECOMPOSITION FORMALLY
#Now we will try to decompose our time series into the different components. 
#We are trying to obtain the Time Series component Season, Trend and Remainder.

#STL decomposition using LOESS
df_ts %>%
  model(
    STL(Consumption ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

#In this plot we can see the different components. We have a very strong upwards trend, as 
#economic growth is going higher and higher in the US. There also is some seasonal component in
#consumption, which varies from -400 til 400 approximately each year. 
#In the remainder we can see the part of the data which cannot be explained by season or trend
#and we will try to predict. It is very volatile in the 1980's boom and during the dotcom bubble and
#later the financial crisis of 2008. We have build in the recession binary variables to at least
#somewhat help our model later catch these more volatile phases. 

#Now instead of graphically we decompose the data set into the actual values and make a table. 
STL_dcmp <- df_ts %>%
  model(stl = STL(Consumption))

#this shows the trend, season and remainder in a dable 
components(STL_dcmp)

# Plot all components 
components(STL_dcmp) %>% autoplot()

# Save components under STL_dcmp
STL_dcmp <- components(STL_dcmp)

#STL makes a difference between additive time series and multiplicative.
#The additive adds Season, Remainder and Trend together and the multiplicative
#multiplies them. When the seasonality increases in strength over time it would be multiplicative. 
#In our case the stay rather constant with values between -500 and 500 and we therefore have an
#additive time series. No further transformation needed before using the STL Decomposition. 
#In our case we have a clear upward trend. This can be seen in the time series itself and then
#isolated when just the trend is graphed. 

#Plot the trend alone
STL_dcmp %>% ggplot(aes(x = Month, y = trend)) + geom_line()

# We're creating & plotting the detrended series with simple subtraction (because additive time series). 
# Trend refers to the overall pattern of the time series. In our case clear 
# upward trend visible. Instead of choosing the trend cycles manually in
# above STL decomposition we let the model find the optimal trend window.

STL_dcmp <- STL_dcmp %>% mutate(detrend = Consumption - trend)

#Detrended time series
STL_dcmp %>% ggplot() + geom_line(aes(x = Month, y = detrend)) 


#We plot only the season, without trend and remainder. 
STL_dcmp %>% ggplot() + geom_line(aes(x = Month, y = season_year))

#STL decomposition automatically gives us the season adjusted time series e.g.
#subtracting overall seasonality from the Consumption time series.
#The Column is called season_adjust in the STL_dcmp tibble
STL_dcmp %>% ggplot() + geom_line(aes(x = Month, y = season_adjust))


# Plotting Consumption, trend adjusted and season adjusted
plot1 <- qplot(Month, Consumption, data = STL_dcmp, geom = "line") + ggtitle("Consumption")
plot2 <- qplot(Month, detrend, data = STL_dcmp, geom = "line") + ggtitle("Trend adjusted")
plot3 <- qplot(Month, season_adjust, data = STL_dcmp, geom = "line") + ggtitle("Season adjusted")
grid.arrange(plot1, plot2, plot3, nrow = 3)

rm(plot1, plot2, plot3)


# Decomposition can be used to measure the strength of trend and seasonality in 
# a time series. Following formula gives a measure of strength of trend between 
# 0 and 1 in comparison to remainder. In our case with 0.99 the trend is very strong, and this is the result we
#got from the graphs where the trend makes up almost all of the value in the range and the remainder
#is only varying between -500 and 500. 
a <- var(STL_dcmp$remainder)
b <- var(STL_dcmp$trend + STL_dcmp$remainder)
trend_strength <- 1-(a/b)
trend_strength

# Strength of the seasonality:
#The seasonality is 0.84 strong in comparison to remainder. We have rather strong seasonalities
#too and the unexplained remainder is only a small part. 
a <- var(STL_dcmp$remainder)
b <- var(STL_dcmp$season_year + STL_dcmp$remainder)
season_strength <- 1-(a/b)
season_strength

rm(trend_strength, season_strength, a, b)
#In a final step we add the STL decomposition to our original dataset df_ts
#so we can later use these in our analysis.

df_ts <- df_ts %>%
  left_join(STL_dcmp, by = "Month")


#5 Let's start forecasting######################################################

#5.1 Very Simple Models################################
#Why do we deploy the very simple models first?
#If a company wanted to hire us as data scientists we would have to show that
#we can bring in some value. A very simple model is when you predict the consumption of
#next month with the average or with the value of last month. We have to show that 
#our complex models later can be better than these simple "dumb" models. Otherwise
#there is no need to hire us. 


#We evaluate these dumb models on a first basis.
#We split the data into training and testing data. 
# Set training data from 1992 to 2013 and testing from 2013 to 2015 April. 
#We will train our models on the training data set.
#and then run them on the testing data set and see 
#if they can predict the new values.
#We will use the seasonally adjusted time series. 

train <- df_ts %>%
  filter_index("1967-01-01" ~ "2013-01-01")
test <- df_ts %>%
  filter_index("2013-01-01" ~ "2015-04-01")
# Fit the models
dumb_fit <- train %>%
  model(
    Mean = MEAN(remainder),
    `Naïve` = NAIVE(remainder),
    `Seasonal naïve` = SNAIVE(remainder),
  )
# Generate forecasts for a bit more than two years (the testing dataset)
dumb_fc <- dumb_fit %>% forecast(h = 27)
# Plot forecasts against actual values
dumb_fc %>%
  autoplot(test, level = NULL) +
  labs(
    y = "Consumption",
    title = "Forecasts for remainder of Consumption in the US Economy"
  ) +
  guides(colour = guide_legend(title = "Dumb Models used"))

#Note: this is only the remainder which we are trying to predict. 
#The Series is already detrended and season adjusted. As we can see the mean model is not able 
#to take the swings into account. The Naive model is also not successful. 
#The Seasonal Naive model does have some success as the remainder of our time series still follows some
#pattern. However in january 2014 it was totally wrong in it's prediction, predicting a positive change in consumption
#while it was actually negative. 
#It makes therefore sense for us programmers to do the job;). We have to add that this analysis is only 
#Univariate (predicts Consumption with looking at the consumption value) and does 
#not yet take the other features like unemployment into consideration. 


#Predicting remainder with prediction intervalls and Seasonal naive model:
train %>%
  model(SNAIVE(remainder)) %>%
  forecast(h = 28) %>%
  autoplot(test) +
  labs(title="US Consumption expenditure", y="$US" )

#Evaluating the model in numbers not in graphs.

snaive_model <- snaive(train$remainder, h=28)

autoplot(snaive_model) +
  ggtitle('SNaive - Forecast')+
  xlab('Date') + ylab('Consumption spending')

accuracy(snaive_model, test$remainder)
mean(test$remainder)
#Mean Average erro is 188.88684 for a variable with a mean of -16.55685
#So our prediction is not really good. 

#Same prediction but for the seasonally adjusted time series. 
#Predictiing the seasonally adjusted time series. 
fit_dcmp <- df_ts %>%
  model(stlf = decomposition_model(
    STL(Consumption.x ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_dcmp %>%
  forecast() %>%
  autoplot(df_ts)+
  labs(y = "$",
       title = "US Consumption spending")

snaive_model <- snaive(train$season_adjust, h=28)

autoplot(snaive_model) +
  ggtitle('SNaive - Forecast')+
  xlab('Date') + ylab('Consumption spending')

accuracy(snaive_model, test$season_adjust)

#                MAE
#Training set  104.3240
#Test set      538.0407

mean(test$season_adjust)


#The Absolute error is 538 on average against a mean value of 34002.86. This is pretty good.
#However we think we can do better. We will now start with more "complex" models. 

#Clean up environment:
rm(dumb_fc, dumb_fit, us_recession, fit_dcmp)

#5.2 Time Series linear multivariate regression model:###########################
#These models are by no means the most complex models.
#Linear regressions are a very simple yet powerful tools in machine learning.
#We use linear regressions as a more complex counterpart to the simple models we 
#just ran. Linear models take the other features also into consideration.
#We are therefore running multivariate regressions. The Regression tries to adjust the coefficients 
#in a loss function so that it minimizes the error. 
#We will be doing this using the TSLM function of the fable package. 
#With the TSLM Function we can add the Trend and the Season into the regression variables, so they 
#can help us predict the Consumption. This is why we decomposed the time series. 

linear_model <- df_ts %>%
  model(tslm = TSLM(Consumption.x ~ Population + Savingsrate + UnemploymentDuration + Unemployed + USREC + trend + season_year))
report(linear_model)

#Our model is able to explain a lot of the variation in the data as we have an R^2 value of 0.9995

#Fitted Values:
augment(linear_model) %>%
  ggplot(aes(x = df_ts$date)) +
  geom_line(aes(y = Consumption.x, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

mean(df_ts$Consumption.x)
linear_model_result <- accuracy(linear_model)
linear_model_result
#On a average value of 25784.64 we have an absolute error of 93.73218
#Which is very good. This is already a really good model. 


#5.2.2 Polynomial Multivariate Regression:
#We now use polynomials instead of the linear form for our model and perform the same analysis:
poly_model <- df_ts %>%
  model(tslm = TSLM(Consumption.x ~ poly(Population + Savingsrate + UnemploymentDuration + Unemployed + USREC + trend + season_year, degree = 2)))
report(poly_model)

#Fitted Values:
augment(poly_model) %>%
  ggplot(aes(x = df_ts$date)) +
  geom_line(aes(y = Consumption.x, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

mean(df_ts$Consumption.x)
poly_model_result <- accuracy(poly_model)
poly_model_result
#On a average value of 25784.64 we have an absolute error of 847.6646.
#This increases drastically from the linear model. Polynomials tend to overfit the data.
#This means they fit the pattern of the training data really well and then do not
#perform really well on the testing data. 


#5.3 ARIMA model################################################################
#Now we will apply a ARIMA Model to our data. Arima models are common forecasting
#models in economic and time series forecasting. 
#ARIMA is short for autoregressive integrated moving average. It is often used to understand
#past data and then predict future data. So it looks at the past and it's events and then
#uses them to create the forecast. It is successful if the data follows a pattern and is 
#not just some random events chained together. 

caf_fit <- train %>%
  model(stepwise = ARIMA(Consumption.x),
        search = ARIMA(Consumption.x, stepwise=FALSE))


glance(caf_fit) %>% arrange(AICc) %>% select(.model:BIC)

caf_fit %>%
  select(search) %>%
  gg_tsresiduals()

#Forecast for two years:
caf_fit %>%
  forecast(h=24) %>%
  filter(.model=='search') %>%
  autoplot(test)+
  labs(y = "Consumption",
       title = "Blue forecast vs Black actual value")

arima_frc <- caf_fit %>%
  forecast(h=24) %>%
  filter(.model=='search')

test_arima <- test %>%
  left_join(arima_frc, by = "Month")

test_arima <- test_arima[2:25,]
test_arima <- test_arima %>%
  mutate(arimaMAE = abs(Consumption.y - .mean))

mean(test_arima$arimaMAE)

#On the testing data the error of the arima model is 236.029.
#This is better than the polynomial model, yet still bigger than the linear model. 

#clean up environment
rm(arima_frc, caf_fit, linear_model_result, poly_model_result, test_arima)


#5.4 Facebook's Prophet Model:##################################################
fit <- train %>%
  model(
    prophet = prophet(Consumption.x ~ season(period = 12, order = 1,
                                           type = "additive"))
  )

fc <- fit %>% forecast(h = "2 years 4 months")
fc %>% autoplot(train)

fc %>% autoplot(test)+
  labs(y = "Consumption",
       title = "Blue forecast vs Black actual value")


fc %>% accuracy(test)

# A tibble: 1 x 10
#.model  .type    ME  RMSE   MAE   MPE
#<chr>   <chr> <dbl> <dbl> <dbl> <dbl>
#1 prophet Test 439.  680.  515.  1.27

#MAE of 515 is still above our linear model.



#6 Evaluation of forecasting models#############################################
#As for the metrics we could also have chosen MSE or RMSE or MAPE, but 
#we thought MAE would be the most easy to understand as it is just the mean absolute
#error. 


#The linear model had an MAE of 93.73$
#The seasonal naive model had an MAE of 538.04
#The Polynomial model had an MAE of 847.66
#The ARIMA model had an MAE of 236.02
#The Prophet model had an MAE of 515

#The linear model performed best!


#7 Conclusion###################################################################

#To conclude this Project we wanted to adress the limitations.
#For more robust results we would need to cross-validate all the models. 
#This is difficult and we would have to do a evalutaion on a rolling forecasting origin. 
#Further comparisons between the models is difficult as we have univariate models like
#SNAIVE, ARIMA and multivariate like the linear, prophet and polynomial. 

#In conclusion we really enjoyed working on this project. We furthered our skills in R. 
#We loaded datasets, cleaned and joined them. Further we learned how to code certain 
#forecasting models and learned the conceptual part behind it with the help of the textbook. 


#8 Bibliography#################################################################

#For References for this  project we used the textbook Forecasting: Principles and Practice (3rd ed.)
#by Rob J Hyndman and George Athanasopoulos (https://otexts.com/fpp3/).



