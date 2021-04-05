#=====TUTORIALS MATERIAL (NOT MADE BY ME)=====

#prepare R for what it is to estimate:
ug_spec = ugarchspec()
ug_spec

#if we want to change from ARMA (1,1) to 
#ARMA (1,0), i.e. AR(1) model
ug_spec <- ugarchspec(mean.model = list(armaOrder = c(4,2)))
ug_spec

# Model Estimation
# ARGUMENTS - VARIANCE.MODEL (HAS EGARCH!!!!)
ugfit = ugarchfit(spec = ug_spec, data = NA_R_BTC)

ugfit

# (under the estimates column - estimated coefficients)
# ar1 - the AR mean model 
# omega
# alpha1 - coefficient to the squared lagged residuals
# beta1 - lagged variance


names(ugfit@fit)
names(ugfit@model)

ugfit@fit$coef

ug_var <- ugfit@fit$var #save the estimated conditional variances


ug_res2 <- (ugfit@fit$residuals)^2 # save the estimated sqrd residuals

plot(ug_res2, type = "l")
lines(ug_var, col = "green")

# forecasting
ugfore <- ugarchforecast(ugfit, n.ahead = 10)

ugfore

# ugfore has two slots 
# names(ugfore@forecast)
# names(ugfore@model)
# 
# plotting the volatilitly after extracting it from the 
# forecast drawer - $sigmaFor
ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l")

ug_var_t <- c(tail(ug_var, 20), rep(NA,10)) #gets the last 20 obsvtns
ug_res2_t <- c(tail(ug_res2, 20), rep(NA,10)) #gets the last 20 obsvtns
ug_f <- c(rep(NA, 20), (ug_f)^2)

plot(ug_res2_t, type = "l")
lines (ug_f, col = "orange")
lines(ug_var_t, col = "green")

#=====MULTIVARIATE GARCH MODELS

#DYNAMIC CONDITIONAL CORRELATION MODEL

# set up the model 
uspec.n = multispec(replicate(3, ugarchspec(mean.model = list(armaOrder = c(1,0)))))

#estimate the univariate models first - 
#theoretical justifications available for it, rather than combining

# estimate the three univariate garch models

R_3 <- data.frame(R_F_BTC, R_F_LTC, R_F_XRP)
names(R_3)[1] <- "BTC"
names(R_3)[2] <- "LTC"
names(R_3)[3] <- "RIPPLE"

R_3 <- R_3[-1,]
# returns three volatility models 
multf = multifit(uspec.n, R_3)

multf


# provide specification for the correlation part (dcc model)
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1,1), distribution ='mvnorm')

#Model Estimation
fit1 = dccfit(spec1, data = R_3, fit.control = list(eval.se = TRUE), fit = multf)
fit1
#eval.se = TRUE -> LOOKS INTO STANDARD ERRORS


cov1 = rcov(fit1) # extract the covariance matrix
cor1 = rcor(fit1) # extract the correlation matrix 

dim(cor1)

cor1[,,dim(cor1)[3]]

cor_GS <- cor1[2,1,] # for second row, first column, all observations
cor_GS <- as.xts(cor_GS)

str(cor_GS)
plot(cor_GS)

par(mfrow = c(3,1)) # creates 3 windows
plot(as.xts(cor1[2,1,]), main = "gold and silver")
plot(as.xts(cor1[3,1,]), main = "gold and libor")
plot(as.xts(cor1[3,2,]), main = "libor and silver")


#forecasts
dccf1 = dccforecast(fit1, n.ahead = 10)
dccf1

#dig into the object to extract info
names(dccf1@mforecast)

# extract the correlation matrix
Rf <- dccf1@mforecast$R

str(Rf)
# shows that we've created a 3-dimensional element: 
# the first two elements are 3x3 matrix with 
# a third - 10 forecasts


corf_GS <- Rf[[1]][2,1,]

cor_GS

par(mfrow=c(3,1))

# (below) gets the last 20 correlation observations
c_GS <- c(tail(cor1[2,1,], 20), rep(NA,10)) 
# (below) gets the last 10 forecasts
cf_GS <- c(rep(NA,20),corf_GS)

plot(c_GS, type = "l", main = "Correlation gold and silver")
lines(cf_GS,type = "l", col = "orange")

#==========================================================
#30 PUBLICLY TRADED COMPANIES FROM THE THREE CHOSEN INDICES
tickers <- 
  c("CNA.L", "PRU", "GLEN.L", "HSBC", "JE.L", "TUI.L", "LLOY.L", "WTB.L", "TSCO.L", 
    "XOM", "JPM", "JNJ", "NKE", "AAPL", "BA", "DIS", "GOOG", "AMZN", "NFLX", "KO",
    "ALV.DE", "BAYN.DE", "DAI.DE", "DB1.DE", "SAP.DE", "CON.DE", "ADS.DE", "BAS.DE", "SIE.DE")

# 1. EXXON MOBIL 2. JP MORGAN CHASE 3. JOHNSON & JOHNSON 4. NIKE 5. APPLE 6. THE BOEING COMPANY 7. THE WALT DISNEY COMPANY
# 1. ALLIANZ 2. BAYER 3. DAIMLER 4. DEUTSCHE BÖRSE 5. SAP 6. CONTINENTAL 7. ADIDAS 8. BASF 9. SIEMENS

getSymbols(tickers, from = startDate, to = endDate)

qbylka <- getSymbols("YHOO", from = as.Date("2018-01-01"), to = as.Date("2018-01-01"))

tickers_close <- merge( CNA.L[,4], PRU[,4], GLEN.L[,4], JE.L[,4], TUI.L[,4], LLOY.L[,4],  WTB.L[,4], TSCO.L[,4], 
                        XOM[,4], JPM[,4], JNJ[,4], NKE[,4], AAPL[,4], BA[,4], DIS[,4], GOOG[,4], AMZN[,4], NFLX[,4],    
                        ALV.DE[,4], BAYN.DE[,4], DAI.DE[,4], DB1.DE[,4], SAP.DE[,4], CON.DE[,4], ADS.DE[,4],  BAS.DE[,4],
                        SIE.DE[,4], all = FALSE)


for (j in 1:ncol(tickers_close)) {
  for (i in 1:nrow(tickers_close)) {
    if (is.na(tickers_close[i,j])==T) {
      tickers_close[i,j]=tickers_close[i-1,j]
    }
  }  
}