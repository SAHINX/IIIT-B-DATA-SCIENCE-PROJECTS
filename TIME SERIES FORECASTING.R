
############################ Time Series Case study #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Classical Decomposition
#  4.2 Arima
#  4.3 Forecasting

#####################################################################################

# 1. Business Understanding:The objective is to find the two most profitable segments and forecast Sales 
# and Quantity for these two profitable segments
#####################################################################################
#2. Data Understanding 


## Setting working Directory
setwd("C:\\PERSONAL\\PGDDA\\Main\\TimeS\\CaseStudy")

##============ Install packages ===========
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("zoo")
install.packages("dplyr")
#install.packages("plyr")


library(forecast)
library(tseries)
require(graphics)
library(ggplot2)
library(zoo)
library(dplyr)
#library(plyr)

##============ Import all the data to R ===========
global_superstore <- read.csv("Global Superstore.csv",stringsAsFactors = FALSE)

## Verify the data loaded
View(global_superstore)

##============ Data understanding, Data preparation =======

# check for dimension and first few rows of data
str(global_superstore)
head(global_superstore)
dim(global_superstore)
# dimension shows number of attributes is 24 and number of observations 51290


# List of market 
market_list<-unique(global_superstore[,"Market"])
market_list
#The 7 unique market  are US,APAC,AFRICA,EMEA,LATAM,Canada

# List of Segments
segment_list<-unique(global_superstore[,"Segment"])
segment_list
#Consumer,Corporate and HomeOffice

# Different buckets that are to be analysed
buckets<-unique(global_superstore[,c("Market","Segment")])
buckets
#We have 21 different Buckets.


## Check for missing values
sum(is.na(global_superstore)) #41296 records are missing 

# Check for NA values in few very important variables
sum(is.na(global_superstore$Order.Date))
sum(is.na(global_superstore$Segment))
sum(is.na(global_superstore$Market))
sum(is.na(global_superstore$Sales))
sum(is.na(global_superstore$Quantity))
sum(is.na(global_superstore$Profit))
## No NA values in the important variables

# head of the dataframe shows that postal_code will contain NA value
sum(is.na(global_superstore$Postal.Code))

# all the rows from postal_code are missing, can delete this column postal_code
global_superstore <- global_superstore[,-12]

## Check for duplicate records
sum(duplicated(global_superstore))


## Format the date variables
global_superstore$Order.Date <- as.Date(global_superstore$Order.Date,format = "%d-%m-%Y")
global_superstore$Ship.Date <- as.Date(global_superstore$Ship.Date,format = "%d-%m-%Y")

## Convert the required variables to factor type
global_superstore$Segment <- as.factor(global_superstore$Segment)
global_superstore$Market <- as.factor(global_superstore$Market)

levels(global_superstore$Segment)
levels(global_superstore$Market)

## Create a new field from Order_date that contains month and year only 
global_superstore$order.MonYear <- format(global_superstore$Order.Date,"%m-%Y")

# Change the format of the newly added  column order.MonYear
global_superstore$order.MonYear <- as.yearmon(global_superstore$order.MonYear,format = "%m-%Y")

colnames <- c("Months","Sales","Quantity","Profit")


### Method 1
# Aggreagate Sales,Quantity and Profit on Monthly basis for all the companies
group <- group_by(global_superstore,Market,Segment,order.MonYear)
Global_Sales <- dplyr::summarise(group,sum(Sales))
Global_Quanity <- dplyr::summarise(group,sum(Quantity))
Global_Profit <- dplyr::summarise(group,sum(Profit))

# Merge the three dataframes Global_Sales,Global_Quanity and Global_Profit
GlobalMart_Sales <- merge(Global_Sales,Global_Quanity,by.x = c("Market","Segment","order.MonYear"),by.y = c("Market","Segment","order.MonYear"))
GlobalMart_Sales <- merge(GlobalMart_Sales,Global_Profit,by.x = c("Market","Segment","order.MonYear"),by.y = c("Market","Segment","order.MonYear"))

# Rename the new columns
colnames(GlobalMart_Sales)[4] <- "Total_Sales"
colnames(GlobalMart_Sales)[5] <- "Total_Quantity"
colnames(GlobalMart_Sales)[6] <- "Total_Profit"

# Sort the data
GlobalMart_Sales <- arrange(GlobalMart_Sales,Market,Segment,order.MonYear)


# Calculate cofficient of varation of each of 21 segments for the profit column , 7 market and 3 segments 
group2 <- group_by(GlobalMart_Sales,Market,Segment)
Profit <- dplyr::summarise(group2,mean(Total_Profit))
Profit[,c(4,5,6)] <-  dplyr::summarise(group2,sd(Total_Profit))
Profit <- Profit[,-c(4,5)]

# Create a new column CV for cofficient of varation
colnames(Profit)[3] <- "Average_profit"
colnames(Profit)[4] <- "SD_profit"

Profit$CV <- 100*(Profit$SD_profit/Profit$Average_profit)

#Identity the top 2 profitable segments
profitable_segments<-arrange(Profit,CV)
profitable_segments
#top FOUR profitable segments are
#1.) EU with segment "Consumer" followed by 
#2.) apac with segment "Consumer"
#3.)LATAM with segement consumer
#4.) APAC with segment Corporate
### Mothod 1 ends


### Method 2 (run either Method 1 or Method 2)
## Get the sum of "Sales", "Quantity","Profit" for each combination of "Segment","Market" and order.MonYear
#groupColumns = c("Segment","Market","order.MonYear")
#sumColumns = c("Sales", "Quantity","Profit")
#GlobalMart_Sales = ddply(global_superstore, groupColumns, function(x) colSums(x[sumColumns]))
#GlobalMart_Sales <- plyr::rename(GlobalMart_Sales, c("Sales"= "Total_Sales", "Quantity"="Total_Quantity","Profit"="Total_Profit"))
###

#GlobalMart_Sales <- arrange(GlobalMart_Sales,Market,Segment,order.MonYear)
## Verify data
#View(GlobalMart_Sales)

## Calculateing CV
#Profit <- ddply(GlobalMart_Sales, .(Segment,Market), summarize,
#                Average_profit = round(mean(Total_Profit), 2),
#                SD_profit = round(sd(Total_Profit), 2))
#Profit$CV <- 100*(Profit$SD_profit/Profit$Average_profit)

## Verify data
#View(Profit)

##Identity profitable segments
#profitable_segments<-arrange(Profit,CV)
### Mothod 2 ends


## Identity the top 2 profitable segments from CV which means highest value of mean or average with low scatter of the profit data or low
## SD value, lowest value of CV

profitable_segments
View(profitable_segments)

## The results shows top 4 most profitable segments are 
#1.) EU with segment "Consumer" followed by 
#2.) apac with segment "Consumer"
#3.)LATAM with segement consumer
#4.) APAC with segment Corporate

## Lets create a dataframe for top 2 segments containing Sales,Quantity and Profit

EU_CO <- subset(GlobalMart_Sales,Market=="EU" & Segment=="Consumer")
APAC_CO <- subset(GlobalMart_Sales,Market=="APAC" & Segment=="Consumer")

## Model building will be done on the two Data frames EU_CO and APAC_CO

# verify data 
View(EU_CO)
View(APAC_CO)

##============ EDA =========
# Dataframe GlobalMart_Sales

#plot of Sales and Profit for each Market over time 
ggplot(GlobalMart_Sales,aes(x=row.names(GlobalMart_Sales),y=Total_Sales)) + geom_point(col="orange") + facet_grid(~Market)

ggplot(GlobalMart_Sales,aes(x=row.names(GlobalMart_Sales),y=Total_Quantity)) + geom_point(col="blue") + facet_grid(~Market)
#plot shows quantity distribution steeply increasing for APAC and EU segment followed by US and LATAM
ggplot(GlobalMart_Sales,aes(x=row.names(GlobalMart_Sales),y=Total_Profit)) + geom_point(col="green") + facet_grid(~Market)

#plot shows profit distribution steeply increasing for APAC and EU segment
ggplot(Profit,aes(x=factor(Segment),y=CV)) + geom_col(fill="turquoise") + facet_grid(~Market)
#CV value highest(least profitable segment) is EMEA and Africa
#CV value on the lowest side(highest profitable segment) APAC,EU and LATAM
ggplot(Profit,aes(x=factor(Segment),y=Average_profit)) + geom_col(fill="turquoise") + facet_grid(~Market)
ggplot(Profit,aes(x=factor(Segment),y=SD_profit)) + geom_col(fill="turquoise") + facet_grid(~Market)

#plot of the two dataframes EU_CO and APAC_CO used for time series modelling

#plot of Total_Sales with Months value(row number) on X axis for EU_CO

ggplot(EU_CO,aes(x=order.MonYear,y=Total_Sales)) +  geom_line()

#shows both trend and seasonality

#this completes the EDA part

##============ Time Series Smoothing ===========
###########################################################################################
## EU with Consumer segment (EUIndata1.Sales) - Total Sales 
## Divide the data into train and test , After that smoothen the time series
###########################################################################################
#Lets first divide the dataframe EU_CO into train and test data

months <- c(1:41)
months_out <- c(42:48)

#taking first 41 rows as input for EU and Consumer
EUIndata1 <- EU_CO[1:41,]
EUIndata1.Sales <- as.data.frame(cbind(months,EUIndata1$Total_Sales))
EUIndata1.Quantity <- as.data.frame(cbind(months,EUIndata1$Total_Quantity))
colnames(EUIndata1.Sales) <- c("Months","Sales")
colnames(EUIndata1.Quantity) <- c("Months","Quantity")

#lets create the test data for EU
EUoutdata1 <- EU_CO[42:48,]
EUoutdata1.Sales <- as.data.frame(cbind(months_out,EUoutdata1$Total_Sales))
EUoutdata1.Quantity <- as.data.frame(cbind(months_out,EUoutdata1$Total_Quantity))
colnames(EUoutdata1.Sales) <- c("Months","Sales")
colnames(EUoutdata1.Quantity) <- c("Months","Quantity")

#lets create test data for APAC
APACoutdata1 <- APAC_CO[42:48,]
APACoutdata1.Sales <- as.data.frame(cbind(months_out,APACoutdata1$Total_Sales))
APACoutdata1.Quantity <- as.data.frame(cbind(months_out,APACoutdata1$Total_Quantity))
colnames(APACoutdata1.Sales) <- c("Months","Sales")
colnames(APACoutdata1.Quantity) <- c("Months","Quantity")

#time series for Total Sales with input data from EUIndata1.Sales
timeser_EU1 <- ts(EUIndata1.Sales$Sales)
plot(timeser_EU1)

#to decompose time series create time series with frquency 12
timeser_EU1.d <- ts(EUIndata1.Sales$Sales,frequency = 12)
timeser_EU1.decompose <- decompose(timeser_EU1.d)
plot(timeser_EU1.decompose)

# Decomposotion showed that:
# 1. Trend in a non linear positive slope line
# 2. Seasonality is a low wavelength sine curve

#smoothing the time series 
#1.)timeser_EU1 for EU with consumer segment and Total_Sales volume
w <- 1
smoothedseries2 <- stats::filter(timeser_EU1,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)

# Smoothing right end of the time series
n <- length(timeser_EU1)
diff <- smoothedseries2[n-w] - smoothedseries2[n-w-1]
for(i in seq(n-w+1,n)){
  smoothedseries2[i] <- smoothedseries2[i-1] + diff
}

# Smoothing left end of the time series
diff <- smoothedseries2[w+2]-smoothedseries2[w+1]
for(i in seq(w,1,-1)){
  smoothedseries2[i] <- smoothedseries2[i+1] - diff
}
smoothedseries2

# Plot the smoothed time series
plot(timeser_EU1)
lines(smoothedseries2,col="blue",lwd=2)

# Lets convert the smoothed series to a dataframe
EU_CO_Sales_timeser_smoothdf <- as.data.frame(cbind(months,as.vector(smoothedseries2)))
colnames(EU_CO_Sales_timeser_smoothdf)[2] <- "Sales"
EU_CO_Sales_timeser_smoothdf$months <- as.numeric(EU_CO_Sales_timeser_smoothdf$months)
EU_CO_Sales_timeser_smoothdf$Sales <- as.numeric(EU_CO_Sales_timeser_smoothdf$Sales)


###########################################################################################
## EU with Consumer segment EUIndata1.Quantity  - Total Quantity 
## Divide the data into train and test , After that smoothen the time series
###########################################################################################
# Inout training data containing months and quantity
EUIndata1.Quantity

# Time series for Total quantity with input data from EUIndata1.Quantity
timeser_EU2 <- ts(EUIndata1.Quantity$Quantity)
plot(timeser_EU2)

# To decompose time series create time series with frquency 12
timeser_EU2.d <- ts(EUIndata1.Quantity$Quantity,frequency = 12)
timeser_EU2.decompose <- decompose(timeser_EU2.d)
plot(timeser_EU2.decompose)

# Decomposotion showed that:
#1.) non linear trend with positive slope
#2.)low wavelength sine and cosine curve

#smoothing the time series 
#1.)timeser_EU2 for EU and Total_Quantity
w <- 1
smoothedseries1 <- stats::filter(timeser_EU2,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)

# Smoothing right end of the time series
n <- length(timeser_EU2)
diff <- smoothedseries1[n-w] - smoothedseries1[n-w-1]
for(i in seq(n-w+1,n)){
  smoothedseries1[i] <- smoothedseries1[i-1] + diff
}

# Smoothing left end of the time series
diff <- smoothedseries1[w+2]-smoothedseries1[w+1]
for(i in seq(w,1,-1)){
  smoothedseries1[i] <- smoothedseries1[i+1] - diff
}
smoothedseries1

# Plot the smoothed time series
plot(timeser_EU2)
lines(smoothedseries1,col="green",lwd=2)

# Lets convert the smoothed series to a dataframe
EU_CO_Qty_timeser_smoothdf <- as.data.frame(cbind(months,as.vector(smoothedseries1)))
colnames(EU_CO_Qty_timeser_smoothdf)[2] <- "Qty"
EU_CO_Qty_timeser_smoothdf$months <- as.numeric(EU_CO_Qty_timeser_smoothdf$months)
EU_CO_Qty_timeser_smoothdf$Qty <- as.numeric(EU_CO_Qty_timeser_smoothdf$Qty)


###########################################################################################
## APAC with Consumer segment (APACIndata1.Sales) - Total Sales 
## Divide the data into train and test , After that smoothen the time series
###########################################################################################
# Lets first divide the dataframe EU_CO into train and test data
months <- c(1:41)

# Taking first 41 rows as input for EU and Consumer
APACIndata1 <- APAC_CO[1:41,]
APACIndata1.Sales <- as.data.frame(cbind(months,APACIndata1$Total_Sales))
APACIndata1.Quantity <- as.data.frame(cbind(months,APACIndata1$Total_Quantity))
colnames(APACIndata1.Sales) <- c("Months","Sales")
colnames(APACIndata1.Quantity) <- c("Months","Quantity")

# Time series for Total Sales with input data from EUIndata1.Sales
timeser_APAC1 <- ts(APACIndata1.Sales$Sales)
plot(timeser_APAC1)

#to decompose time series create time series with frquency 12
timeser_APAC1.d <- ts(APACIndata1.Sales$Sales,frequency = 12)
timeser_APAC1.decompose <- decompose(timeser_APAC1.d)
plot(timeser_APAC1.decompose)

# Decomposotion showed that:
# 1. Trend in a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

#smoothing the time series 
#1.)timeser_APAC.d for APAC and Total_Sales volume

#3.) timerser_APAC1
w <- 1
smoothedseries3 <- stats::filter(timeser_APAC1,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)

#Smoothing right end of the time series
n <- length(timeser_APAC1)
diff <- smoothedseries3[n-w] - smoothedseries3[n-w-1]
for(i in seq(n-w+1,n)){
  smoothedseries3[i] <- smoothedseries3[i-1] + diff
}

#Smoothing left end of the time series
diff <- smoothedseries3[w+2]-smoothedseries3[w+1]
for(i in seq(w,1,-1)){
  smoothedseries3[i] <- smoothedseries3[i+1] - diff
}
smoothedseries3

#Plot the smoothed time series
plot(timeser_APAC1)
lines(smoothedseries3,col="blue",lwd=2)

#lets convert the smoothed series to a dataframe
APAC_CO_Sales_smoothdf <- as.data.frame(cbind(months,as.vector(smoothedseries3)))
colnames(APAC_CO_Sales_smoothdf)[2] <- "Sales"
APAC_CO_Sales_smoothdf$months <- as.numeric(APAC_CO_Sales_smoothdf$months)
APAC_CO_Sales_smoothdf$Sales <- as.numeric(APAC_CO_Sales_smoothdf$Sales)


###########################################################################################
## APAC with Consumer segment (APACIndata1.Quantity) - Total Sales 
## Divide the data into train and test , After that smoothen the time series
###########################################################################################

# Input training data containing months and quantity
APACIndata1.Quantity

# Time series for Total quantity with input data from EUIndata1.Quantity
timeser_APAC2 <- ts(APACIndata1.Quantity$Quantity)
plot(timeser_APAC2)

# To decompose time series create time series with frquency 12
timeser_APAC2.d <- ts(APACIndata1.Quantity$Quantity,frequency = 12)
timeser_APAC2.decompose <- decompose(timeser_APAC2.d)
plot(timeser_EU2.decompose)

# Decomposotion showed that:
#1.) non linear trend with positive slope
#2.)low wavelength sine and cosine curve

#lets smoothen the time series 
#4.)timeser_APAC2
w <- 1
smoothedseries4 <- stats::filter(timeser_APAC2,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)

#Smoothing right end of the time series
n <- length(timeser_APAC2)
diff <- smoothedseries4[n-w] - smoothedseries4[n-w-1]
for(i in seq(n-w+1,n)){
  smoothedseries4[i] <- smoothedseries4[i-1] + diff
}

#Smoothing left end of the time series
diff <- smoothedseries4[w+2]-smoothedseries4[w+1]
for(i in seq(w,1,-1)){
  smoothedseries4[i] <- smoothedseries4[i+1] - diff
}
smoothedseries4

#Plot the smoothed time series
plot(timeser_APAC2)
lines(smoothedseries4,col="green",lwd=2)

#lets convert the smoothed series to a dataframe
APAC_CO_Qty_smoothdf <- as.data.frame(cbind(months,as.vector(smoothedseries4)))
colnames(APAC_CO_Qty_smoothdf)[2] <- "Qty"
APAC_CO_Qty_smoothdf$months <- as.numeric(APAC_CO_Qty_smoothdf$months)
APAC_CO_Qty_smoothdf$Qty <- as.numeric(APAC_CO_Qty_smoothdf$Qty)


#SO FOUR SMOOOTHED time series has been created and converted to Data.frame
#smoothedseries1 ------- EU_CO_Qty_timeser_smoothdf
#smoothedseries2 ------- EU_CO_Sales_timeser_smoothdf
#smoothedseries3 ------- APAC_CO_Sales_smoothdf
#smoothedseries4 ------- APAC_CO_Qty_smoothdf

###### end of smoothing #####

##============ MODEL BUILDING ===========
###################################################################################################
# 1
# lets build a classical decomposition model one by one on each of the smoothed dataframe 
# starting with EU_CO_Sales_timeser_smoothdf to model the Sales with time
# classical decomposition model

###################################################################################################

EU_CO_Sales_timeser_smoothdf
plot(timeser_EU1)
lines(smoothedseries2,col="blue",lwd=2)

lmfit1 <- lm(Sales ~ sin(0.6*months)*poly(months,3)+ cos(0.6*months)*poly(months,3) + months ,data = EU_CO_Sales_timeser_smoothdf)
             
summary(lmfit1)

trend1 <- predict(lmfit1, Month=EU_CO_Sales_timeser_smoothdf$months)

lines(EU_CO_Sales_timeser_smoothdf$months,trend1, col="red", lwd=2)

# Now, let's look at the locally predictable series
# We will model it as an ARMA series
local_pred1 <- timeser_EU1 - trend1
plot(local_pred1,col="blue",type="l")
acf(local_pred1)
acf(local_pred1,type="partial")
armafit1 <- auto.arima(local_pred1)
tsdiag(armafit1)
armafit1

# We'll check if the residual series is white noise
resi1 <- local_pred1 - fitted(armafit1)
adf.test(resi1,alternative = "stationary")
kpss.test(resi1)

# In both adf and kpss test the p value proofs that residual series is purely white noise

#Now, let's do the prediction for the test data and compare our prediction with the actual values, using MAPE
#EUoutdata1.Sales
EUoutdata1.Sales
months <- EUoutdata1.Sales$Months
global_pred_saleseu <- predict(lmfit1,data.frame(Month = months))
fcast1 <- global_pred_saleseu

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_SALESEU <- accuracy(fcast1,EUoutdata1.Sales[,2])[5]
MAPE_SALESEU
#Mape value is 21.17 

months <- c(49:54)
x <- as.data.frame(months)
#lets forecast for the next 6 months 
forecast_EU_Sales.cd <- predict(lmfit1,data.frame(Month=x$months))
forecast_EU_Sales.cd
#forecast of Sales for next 6 months 
#61408.78 57440.63 51403.10 45486.34 42751.27 46288.51


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
EU_Sales_pred <- c(ts(trend1),ts(forecast_EU_Sales.cd))
plot(timeser_EU1, col = "black")
lines(EU_Sales_pred, col = "red")


#============ Forecasting EU Consumer  ===========
EUConsalesForecast <- HoltWinters(timeser_EU1,  gamma=FALSE)
EUConsalesForecast
plot(EUConsalesForecast)

#=========== Using Forecast function to calculate next six month profit ========
forecast(EUConsalesForecast,h=13)
#forecasting value from month 48 to 54
#34828.64,35047.09,35265.54,35483.99,35702.44,35920.89

##### end  #################

###################################################################################################
# 2
#Model with timeser_EU1 to model the Sales with time
#ARIMA Model
###################################################################################################

autoarima_EU.sales <- auto.arima(timeser_EU1)
tsdiag(autoarima_EU.sales)

summary(autoarima_EU.sales)

#summary shows and arima model of order(2,1,0)
plot(autoarima_EU.sales$x,col="black")
lines(autoarima_EU.sales$fitted,col="red")

#Again, let's check if the residual series is white noise
resi_EUsales <- timeser_EU1 - fitted(autoarima_EU.sales)
adf.test(resi_EUsales)
kpss.test(resi_EUsales)

#the p value in both adf and kpss test suggest that the residual series is purely white noise
#Also, let's evaluate the model created using auto arima using MAPE
fcast_EU_aasales <- predict(autoarima_EU.sales,n.ahead = 7)
fcast_EU_aasales

#26844.11 31029.45 32129.90 29304.11 30401.79 31222.05 30166.80
#shows the forecase value for next 7 months from 42 to 48. 
MAPE_auto_arima <- accuracy(fcast_EU_aasales$pred,EUoutdata1.Sales[,2])[5]
MAPE_auto_arima

#MAPE value 37.44
#Lets forecast for next 6 months from 48,i.e starting from 49 to 54
forecast(autoarima_EU.sales,13)

#forecasted value for the next 6 months from 49 to 54 is 
#30363.95, 30795.98 , 30438.22  ,30425.04 , 30618.80, 30511.16

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima_EU.sales),ts(fcast_EU_aasales$pred))
plot(timeser_EU1, col = "black")
lines(auto_arima_pred, col = "red")


    ##### end  #################

###################################################################################################
# 3
#lets build a classical decomposition model one by one on each of the smoothed dataframe 
# starting with EU_CO_Qty_timeser_smoothdf to model the Sales with time
#classical decomposition model

###################################################################################################

EU_CO_Qty_timeser_smoothdf
plot(timeser_EU2)
lines(smoothedseries1,col="blue",lwd=2)
lmfit2 <- lm(Qty~sin(0.4*months)*poly(months,2)+ cos(0.4*months)*poly(months,2) + months ,data = EU_CO_Qty_timeser_smoothdf)

summary(lmfit2)

trend2 <- predict(lmfit2, Month=EU_CO_Qty_timeser_smoothdf$months)

lines(EU_CO_Qty_timeser_smoothdf$months,trend2, col="red", lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred2 <- timeser_EU2 - trend2
plot(local_pred2,col="blue",type="l")
acf(local_pred2)
acf(local_pred2,type="partial")
armafit2 <- auto.arima(local_pred2)
tsdiag(armafit2)
armafit2

#We'll check if the residual series is white noise
resi2 <- local_pred2 - fitted(armafit2)
adf.test(resi2,alternative = "stationary")
kpss.test(resi2)
# in both adf and kpss test the p value proofs that residual series is purely white noise


#Now, let's do the prediction for the test data and compare our prediction with the actual values, using MAPE
#EUoutdata1.Sales
EUoutdata1.Quantity
months <- EUoutdata1.Quantity$Months
global_pred_qtyeu <- predict(lmfit2,data.frame(Month = months))
fcast2 <- global_pred_qtyeu

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_QTYEU <- accuracy(fcast2,EUoutdata1.Quantity[,2])[5]
MAPE_QTYEU
#Mape value is 25.17
months <- c(49:54)
x <- as.data.frame(months)

#lets forecast for the next 6 months 
forecast_EU_qty.cd <- predict(lmfit2,data.frame(Month=x$months))
forecast_EU_qty.cd
#forecast of Sales for next 6 months 
#1        2        3        4        5        6 
#576.1177 438.2182 294.4079 173.8027 104.8403 109.4954 

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
EU_Qnty_pred <- c(ts(trend2),ts(forecast_EU_qty.cd))
plot(timeser_EU2, col = "black")
lines(EU_Qnty_pred, col = "red")


#============== Forecasting EU Consumer Profit ==========
EUConqtyForecast <- HoltWinters(timeser_EU2,  gamma=FALSE)
EUConqtyForecast
plot(EUConqtyForecast)

#============== Using Forecast function to calculate next six month profit ==========
forecast(EUConqtyForecast,h=13)
#forecasting value from month 48 to 54
#424.13,422.84,421.54,420.25,418.96, 417.66,416.37

##### end  #################

###################################################################################################
# 4
#Model with timeser_EU2 to model the Qty with time
#ARIMA Model

###################################################################################################

autoarima_EU.Qty <- auto.arima(timeser_EU2)
tsdiag(autoarima_EU.Qty)

summary(autoarima_EU.Qty)

#summary shows and arima model of order(0,1,0)
plot(autoarima_EU.Qty$x,col="black")
lines(autoarima_EU.Qty$fitted,col="red")

#Again, let's check if the residual series is white noise
resi_EUqty <- timeser_EU2 - fitted(autoarima_EU.Qty)
adf.test(resi_EUqty)
kpss.test(resi_EUqty)

#the p value in kpss test suggest that the residual series is purely white noise
#Also, let's evaluate the model created using auto arima using MAPE
fcast_EU_aaqty <- predict(autoarima_EU.Qty,n.ahead = 7)
fcast_EU_aaqty

#shows the forecase value for next 7 months from 42 to 48. 
MAPE_auto_arima <- accuracy(fcast_EU_aaqty$pred,EUoutdata1.Quantity[,2])[5]
MAPE_auto_arima

#MAPE value 36.33
#Lets forecast for next 6 months from 48,i.e starting from 49 to 54
forecast(autoarima_EU.Qty,13)
#forecasted value for the next 6 months from 49 to 54 is 
#400.96, 403.27 , 395.71  ,399.93 , 401.34, 397.76


##### end  #################

###################################################################################################
# 5
#lets build a classical decomposition model one by one on each of the smoothed dataframe 
# starting with APAC_CO_Sales_smoothdf to model the Sales with time
#classical decomposition model

###################################################################################################
APAC_CO_Sales_smoothdf
plot(timeser_APAC1)
lines(smoothedseries3,col="blue",lwd=2)

lmfit3 <- lm(Sales ~ sin(0.6*months)*poly(months,2)+ cos(0.6*months)*poly(months,2) + months,data=APAC_CO_Sales_smoothdf)
summary(lmfit3)

trend3 <- predict(lmfit3, Month=APAC_CO_Sales_smoothdf$months)

lines(APAC_CO_Sales_smoothdf$months,trend3, col="red", lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred3 <- smoothedseries3 - trend3
plot(local_pred3,col="blue",type="l")
acf(local_pred3)
acf(local_pred3,type="partial")
armafit3 <- auto.arima(local_pred3)
tsdiag(armafit3)
armafit3

#We'll check if the residual series is white noise
resi3 <- local_pred3 - fitted(armafit3)
adf.test(resi3,alternative = "stationary")
kpss.test(resi3)

# in both adf and kpss test the p value proofs that residual series is purely white noise
#Now, let's do the prediction for the test data and compare our prediction with the actual values, using MAPE
#APACoutdata1.Sales

months <- APACoutdata1.Sales$Months
global_pred_salesapac <- predict(lmfit3,data.frame(Month = months))
fcast3 <- global_pred_salesapac

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_SALESAPAC <- accuracy(fcast3,APACoutdata1.Sales[,2])[5]
#Mape value is 27.26

months <- c(49:54)
x <- as.data.frame(months)

#lets forecast for the next 6 months 
forecast_APAC_sales.cd <- predict(lmfit3,data.frame(Month=x$months))
forecast_APAC_sales.cd
#forecast of Sales for next 6 months 
#1        2        3        4        5        6 
#47467.64 47793.19 49777.23 53071.80 57143.31 61337.73 

APAC_sales_pred <- c(ts(trend3),ts(forecast_APAC_sales.cd))
plot(timeser_APAC1, col = "black")
lines(APAC_sales_pred, col = "red")

#========== Forecasting EU Consumer Profit ============
APACConsalesForecast <- HoltWinters(timeser_APAC1,  gamma=FALSE)
APACConsalesForecast
plot(APACConsalesForecast)

#========= Using Forecast function to calculate next six month profit =======
forecast(APACConsalesForecast,h=13)
#forecasting value from month 49 to 54
#46320.87 46611.20 46901.52  47191.85  47482.17  47772.49

##### end  #################

###################################################################################################
# 6
#Model with APACIndata1.Sales to model the Sales with time
#ARIMA Model
#Note : I have taken timeser_APAC1.d instead of timeser_APAC1 because with the first one 
#we got better forecast whereas with the second one timeser_APAC1 the forecasted and predicted 
#value are all same
###################################################################################################
APACIndata1.Sales
autoarima_APAC.Sales <- auto.arima(timeser_APAC1.d)
tsdiag(autoarima_APAC.Sales)

summary(autoarima_APAC.Sales)

#summary shows and arima model of order(0,1,1)
plot(autoarima_APAC.Sales$x,col="black")
lines(autoarima_APAC.Sales$fitted,col="red")

#Again, let's check if the residual series is white noise

resi_APACsales <- timeser_APAC1.d - fitted(autoarima_APAC.Sales)
adf.test(resi_APACsales)
kpss.test(resi_APACsales)

#the p value in both adf and kpss test suggest that the residual series is purely white noise
#Also, let's evaluate the model created using auto arima using MAPE
fcast_APAC_aasales <- predict(autoarima_APAC.Sales,n.ahead = 7)
fcast_APAC_aasales

#shows the forecase value for next 7 months from 42 to 48.
#Jun      Jul      Aug      Sep      Oct      Nov      Dec
# 64508.71 42647.56 49146.63 52965.98 54146.46 50708.84 53090.65

MAPE_auto_arima <- accuracy(fcast_APAC_aasales$pred,APACoutdata1.Sales[,2])[5]
MAPE_auto_arima
#MAPE value 23.45
#Lets forecast for next 6 months from 48,i.e starting from 49 to 54
forecast(autoarima_APAC.Sales,13)

#forecasted value for the next 6 months from 49 to 54 is 
#Jan 5       44677.23 
#Feb 5       34710.50 
#Mar 5       49810.56 
#Apr 5       41611.04 
#May 5       54205.19 
#Jun 5       60892.77 
##### end  #################

###################################################################################################
# 7
#lets build a classical decomposition model one by one on each of the smoothed dataframe 
# starting with APAC_CO_Qty_smoothdf to model the Quantity with time
#classical decomposition model

###################################################################################################
APAC_CO_Qty_smoothdf
plot(timeser_APAC2)
lines(smoothedseries4,col="blue",lwd=2)

lmfit4 <- lm(Qty ~ sin(0.6*months)*poly(months,3)+ cos(0.6*months)*poly(months,3) + poly(months,2),data=APAC_CO_Qty_smoothdf)
summary(lmfit4)

trend4 <- predict(lmfit4, Month=APAC_CO_Qty_smoothdf$months)

lines(APAC_CO_Qty_smoothdf$months,trend4, col="green", lwd=2)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred4 <- smoothedseries4 - trend4
plot(local_pred4,col="blue",type="l")
acf(local_pred4)
acf(local_pred4,type="partial")
armafit4 <- auto.arima(local_pred4)
tsdiag(armafit4)
armafit4

#We'll check if the residual series is white noise
resi4 <- local_pred4 - fitted(armafit4)
adf.test(resi4,alternative = "stationary")
kpss.test(resi4)

# in both adf and kpss test the p value proofs that residual series is purely white noise
#Now, let's do the prediction for the test data and compare our prediction with the actual values, using MAPE
#APACoutdata1.Quantity


months <- APACoutdata1.Quantity$Months
global_pred_qtyapac <- predict(lmfit4,data.frame(Month = months))
fcast4 <- global_pred_qtyapac

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_QTYAPAC <- accuracy(fcast4,APACoutdata1.Quantity[,2])[5]
#Mape value is 41.844

months <- c(49:54)
x <- as.data.frame(months)

#lets forecast for the next 6 months 
forecast_APAC_qty.cd <- predict(lmfit4,data.frame(Month=x$months))
forecast_APAC_qty.cd

#forecast of Sales for next 6 months 
forecast_APAC_qty.cd
#1          2          3          4          5          6 
#97.93817   96.28774  314.88698  715.98210 1178.50278 1530.99878  


#get a visual feel of the fit
APAC_Qnty_pred <- c(ts(trend4),ts(forecast_APAC_qty.cd))
plot(timeser_APAC2, col = "black")
lines(APAC_Qnty_pred, col = "red")

#========== Forecasting EU Consumer Profit ============
APACConqtyForecast <- HoltWinters(timeser_APAC2,  gamma=FALSE)
APACConqtyForecast
plot(APACConqtyForecast)

#========== Using Forecast function to calculate next six month profit ===========
forecast(APACConqtyForecast,h=13)
#forecasting value from month 49 to 54
#560.29 564.94 569.59 574.23 578.88 583.53

##### end  #################

###################################################################################################
# 8
#lModel with APACIndata1.Quantity to model the QUANTITY with time
#ARIMA Model
#Note : I have taken timeser_APAC1.d instead of timeser_APAC1 because with the first one 
#we got better forecast whereas with the second one timeser_APAC1 the forecasted and predicted 
#value are all same
###################################################################################################
APACIndata1.Quantity
autoarima_APAC.qty <- auto.arima(timeser_APAC2.d)
tsdiag(autoarima_APAC.qty)

summary(autoarima_APAC.qty)
#summary shows and arima model of order(0,1,1)
plot(autoarima_APAC.qty$x,col="black")
lines(autoarima_APAC.qty$fitted,col="red")
#Again, let's check if the residual series is white noise

resi_APACqty <- timeser_APAC2.d - fitted(autoarima_APAC.qty)
adf.test(resi_APACqty)
kpss.test(resi_APACqty)

#the p value in both adf and kpss test suggest that the residual series is purely white noise
#Also, let's evaluate the model created using auto arima using MAPE
fcast_APAC_aaqty <- predict(autoarima_APAC.qty,n.ahead = 7)
fcast_APAC_aaqty

#shows the forecase value for next 7 months from 42 to 48.
#Jun      Jul      Aug      Sep      Oct      Nov      Dec
# 656.6518 457.9277 544.9113 647.9534 551.6023 614.4982 613.8291
MAPE_auto_arima2 <- accuracy(fcast_APAC_aaqty$pred,APACoutdata1.Quantity[,2])[5]
MAPE_auto_arima2
#MAPE value 22.14
#Lets forecast for next 6 months from 48,i.e starting from 49 to 54
forecast(autoarima_APAC.qty,13)
#forecasted value for the next 6 months from 49 to 54 is 
#Jan       488.0374 
#Feb        428.4871 
#Mar        478.0008 
#Apr        473.9862
#May        603.7925 
#Jun        644.3749 

##### end  #################
