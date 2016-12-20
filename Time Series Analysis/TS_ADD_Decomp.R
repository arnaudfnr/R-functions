
# Calculate of the moving average of the data set "ts"
calc_MA <- function(ts, posVal, len, nMA, mid, st, end)
{
  MA <- rep(NA, len)
  
  if(nMA > len)
  {
    print("Moving average is too large for this time-series data")
    return(MA)
  }
  
  inf <- mid
  if (nMA%%2 == 0)
    sup <- mid - 1
  else
    sup <- mid
  
  for(i in st:end)
  {
    sum <- 0
    for (j in (i-inf):(i+sup))
      sum = sum + ts[j, posVal]
    
    avg <- sum / nMA
    MA[i] = avg
  }
  
  return(MA)
}

# calculate the centered moving average given the moving average set
calc_CMA <- function(MA, len, st, end)
{
  CMA <- rep(NA, len)
  
  for(i in st:(end-1))
    CMA[i] = (MA[i] + MA[i+1]) / 2
  
  return(CMA)
}

# Calculate the avg values for each season of the time series
calc_Savg <- function(ts, len, season_nb)
{
  Savg <- rep(0, season_nb)
  for(i in 1:season_nb)
  {
    j <- i
    sum <- 0
    count <- 0
    while(j <= len)
    {
      val <- ts[j]
      if(is.na(val) == FALSE)
      {
        sum = sum + val
        count = count + 1
      }
      j = j + season_nb
    }
    Savg[i] = sum / count
  }
  
  return(Savg)
}

#Main function for decomposition
  # ts is the time series
  # season_nb is the number of season (4 if quarters, 12 if months, etc..)
  # posVal is the column number of the values to be computed
  # forecast_seasons is the number of season to forecast using the additive model
TS_ADD_Decomp <- function(ts, posVal, season_nb, forecast_seasons)
{
  
  # Initialize values
  len <- length(ts[,posVal])
  ind <- 1:len # list of indexes for the values (used for linear regression)
  nMa <- season_nb # number of values to be computed in the moving average
  mid <- as.integer(nMa/2) 
  st <- mid+1 # row number of starting position of MA
  if(nMa%%2 == 0)
    end <- len-mid+1 # row number of ending postion of MA
  else
    end <- len-mid
  
  # Calculate Moving average using above function
  MA <- data.frame(MA = calc_MA(ts, posVal, len, nMa, mid, st, end))
  
  # Calculate centered moving average using above function
  CMA <- data.frame(CMA = calc_CMA(MA[,1], len, st, end))
  
  # Compute linear regression of CMA in orde to determine CMA Trend function (CMAT)
  linReg <- lm(as.matrix(CMA) ~ ind)
  coeff <- linReg[[1]] # Retrieve coeff of the linear regression
 
  # Calculat CMA trend from the past data until a 2 years forecast
  ind = as.matrix(1:(len+forecast_seasons))
  CMAT <- data.frame(CMAT = coeff[1] + ind * coeff[2])
  
  # Calculate the cycle factor, which is the ratio between CMA and CMA Trend for past Data
  # To compute this value, we only take the values from the last two season cycles
  cycle_st <- end - 2 * season_nb
  CF <- data.frame(CF = CMA[cycle_st:(end-1),1]/CMAT[cycle_st:(end-1),1])
  
  # Calculate Detrended Time-Series, i.e. the difference between real values and Centered Moving Average
  DtrendTS <- data.frame(DTrendTS = ts[,posVal] - CMA[1:len,1])
  
  # Calculate seasonal index, i.e. the average detrendedTS by quarter/season
  SI <- data.frame(SI = calc_Savg(DtrendTS[,1], len, season_nb))
  
  # Calculate Irregular Time-Series, i.e the remaining difference between real values and DetrendedTS + Seasonal Indexes
  IrregTS <- data.frame(IrregTS = DtrendTS[,1] - SI[,1])
  # Calculate avg Irregular Ts by quarter/season for forecasting
  # The non-linear shape of the function can not be forecasted by linear regression, as well as seasonal Indexes
  IrregSI <- data.frame(IrregSavg = calc_Savg(IrregTS[,1], len, season_nb))
  
  # The forecast data is the sum of trend cycle component, seasonal index and seasonal irregularity
  Forecast <- data.frame(Forecast = CMAT[,1] + SI[,1] + IrregSI[,1] + CF[,1])
  
  res <- list(CMA, CMAT, DtrendTS, SI, IrregTS, IrregSI, CF, Forecast)

  return(res)
}

# NOT USED ANYMORE ! I realized that as the cycle factor could not be associated 
# with a fixed seasonal pattern, it was not smart to compute the averages for each season. 
# Instead, I only take the cycle pattern of the last two seasonal cycles and I use it for the forecasting

# As for the Irregular TS, CF is non-linear so we have to calculate an average for each season, which will be used in the forecasting
# CF_SI <- data.frame(CF_Savg = calc_Savg(CF[,1], (end-cycle_st), season_nb))