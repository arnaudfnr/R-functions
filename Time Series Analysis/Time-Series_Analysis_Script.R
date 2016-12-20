library(xlsx)
library(plotly)

# Importing function that compute the time-series decomposition with additive model
source("TS_ADD_Decomp.R", chdir = TRUE)

# Here the traffic data is 14 days of network traffic of a European Internet Service Provider
ts_data <- read.xlsx2(file = "/Datasets/internet-traffic-data-in-bits-fr.xlsx", startRow = 15, colIndex = c(1,2), sheetIndex = 1, as.data.frame = TRUE, header = TRUE, colClasses = rep("numeric",2))

#####     SUMMARY   #####
  ### PARAMETERS INITIALIZATION
  ### FORECAST RESULTS
  ### CHANGING PART
  ### ERRORS
  ### PLOTS
#####               #####

### PARAMETERS INITIALIZATION   ###
len <- length(ts_data[,1])
posVal <- 2 # the column index of time-series values
season_nb <- 7 # as the number of days in a week
forecast_nb <- 14 # number of days to forecast

# Preparing the forecast data so it can be added a the end of the time-series dataset
# Parameters change depending on the dataset !
# Name of dimension change depending on dimension names of TS
traffic <- data.frame(Traffic = rep("?", forecast_nb)) # TRAFFIC
# Add index of the day
seasons <- data.frame(Date = 1:forecast_nb%%season_nb) # TRAFFIC
# Convert the 0 index to last Index, i.e. 7 for a weekly season
seasons[which(seasons==0),1] = season_nb

###   FORECAST RESULTS   #
# The function returns the decomposition and the forecast for the 8 seasons 
# to come after the given time series as well as a forecast of the past data 
# in order to compare forecast and real data
res <- TS_ADD_Decomp(ts_data, posVal, season_nb, forecast_nb)

CMA <- res[[1]]       # Centered Moving Average
CMAT <- res[[2]]      # Center Moving Average Trend
DtrendTS <- res[[3]]  # Detrented time Series
SI <- res[[4]]        # Seasonal Indexes
IrregTS <- res[[5]]   # Irregulat Time Series
IrregSavg <- res[[6]] # Irregular Seasonal Aerages
CF <- res[[7]]        # Cycle Factors
Forecast <- res[[8]]  # Forecast values

###   CHANGING PART  ###
# creating the time-series forecast data for TRAFFIC
Forecast_data <- cbind.data.frame( seasons, value, Forecast = Forecast[(len+1):(len+forecast_nb),1])
# creating the time-series forecast data for PRACTICE
#Forecast_data <- cbind.data.frame( Year, seasons, value, Forecast = Forecast[(len+1):(len+forecast_nb),1])
# adding the forecasts to the orginal dataset
ts_data <- data.frame(ts_data, Forecast = Forecast[1:len,1])
ts_data$Date <- as.numeric(ts_data$Date) # ONLY TRAFFIC
ts_data <- rbind.data.frame(ts_data, Forecast_data)
ts_data$Traffic <- as.numeric(ts_data$Traffic) # ONLY TRAFFIC

####    ERRORS   ###
absErr <- data.frame(AbsErr = abs(as.numeric(ts_data[1:len,posVal]) - Forecast[1:len,1]))
relErr <- data.frame(RelErr = absErr*100 / as.numeric(ts_data[1:len,posVal]))
meanAbsErr <- mean(absErr[,1])
meanRelErr <- mean(relErr[,1])
Errors <- cbind.data.frame(rbind.data.frame(absErr, mean = meanAbsErr), rbind.data.frame(relErr, mean = meanRelErr))

###   PLOTS   ###
plot_ly(ts_data, type = 'scatter', mode = 'lines', x=1:len, y=~ts_data[1:len,posVal])
plot_ly(CMA, type = 'scatter', mode='lines', x=1:len, y=~CMA)
plot_ly(DtrendTS, type = 'scatter', mode='lines', x=1:len, y=~DTrendTS)
plot_ly(CMAT, type = 'scatter', mode='lines', x=1:(len+forecast_nb), y=~CMAT)
plot_ly(SI, type = 'scatter', mode='lines', x=1:season_nb, y=~SI)
plot_ly(CF, type = 'scatter', mode='lines', x=1:(2*season_nb), y=~CF)
plot_ly(IrregTS, type = 'scatter', mode='lines', x=1:len, y=~IrregTS)
plot_ly(Forecast, type = 'scatter', mode='lines', x=1:(len+forecast_nb), y=~Forecast)
