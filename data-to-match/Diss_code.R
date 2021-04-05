rm(list=ls()) # clear environment
rm() #remove any variable

cat("\014") # clear console

install.packages("zoo")
#LOADING THE PACKAGES NEEDED FOR THE ANALYSIS
library(rugarch) #quantmod + rmgarch
library(rmgarch)
library(xts)
library(quantmod)
library(zoo)


# DEFINING THE TIME FRAME

startDate = as.Date("2014-09-17")

endDate = as.Date("2019-11-27")

# CRYPTOCURRENCY PRICES

# BITCOIN 
BTC <- getSymbols("BTC-USD", from = startDate, to = endDate)

#RIPPLE
XRP <- getSymbols("XRP-USD", from = startDate, to = endDate)

#ETHEREUM
ETH <- getSymbols("ETH-USD", from = startDate, to = endDate)
# Ether was released 7 Aug 2015

#LITECOIN
LTC <- getSymbols("LTC-USD", from = startDate, to = endDate)

# PRICE CHART
chartSeries(get(BTC)) #same for the rest



#================================================
#library(ggplot2)
#plot5 <- ggplot(gold, aes(X, Price, group = 1)) +
#   geom_line() +
#   labs(x = "days", y = "price", 
#        title = "Daily prices GOld") +
#   theme(axis.text.x = element_text(color = "black", size = 15),
#         axis.text.y = element_text(color = "black", size = 15)
#         )
# 
# plot5
#========================================================================================
# REGRESSORS DATA

# GOLD FUTURES
GOLD.df = read.csv("GOld.csv", header = T )

# SILVER FUTURES
SILVER.df = read.csv("Silver.csv", header = T)

# LIBOR OVERNIGHT RATES
LIBOR = read.csv("LIBOR-USD-edit.csv", header = T)

# EURO - US DOLLAR
EURUSD <- getSymbols("EURUSD=X", from = startDate, to = endDate)

# POUND - US DOLLAR
GBPUSD <- getSymbols("GBPUSD=X", from = startDate, to = endDate)

# US DOLLAR - SWISS FRANC
USDCHF <- getSymbols("CHF=X", from = startDate, to = endDate)

# S&P 500 
SnP500 <- getSymbols("^GSPC", from = startDate, to = endDate)

# DAX PERFORMANCE INDEX 
DAX <- getSymbols("^GDAXI", from = startDate, to = endDate)


# EXTRACTING THE CLOSING DAILY PRICES 
# ( The "Cl" function does not apply to gold, silver and LIBOR
#   data as only closing prices are 
#   available)

CL_BTC <- Cl(`BTC-USD`)

CL_XRP<- Cl(`XRP-USD`)

CL_ETH <- Cl(`ETH-USD`)

CL_LTC <- Cl(`LTC-USD`)

CL_LIBOR <- LIBOR[,c(1,3)]

CL_EURUSD <- Cl(`EURUSD=X`)

CL_GBPUSD <- Cl(`GBPUSD=X`)

CL_USDCHF <- Cl(`CHF=X`)

CL_SnP500 <- Cl(GSPC)

CL_DAX <- Cl(GDAXI)

#==========================================
# EXPORT THE TIME SERIES DATA INTO CSV 
# FILES FOR FURTHER TRANSFORMATION

# ASSIGN AS DATA FRAMES 
# bitco_close.df <- as.data.frame(CL_BTC)
# ripple_close.df <- as.data.frame(CL_XRP)
# ether_close.df <- as.data.frame(CL_ETH)
# liteco_close.df <- as.data.frame(CL_LTC)
# DAX.df <- as.data.frame(CL_DAX)
# eur_usd_close.df <- as.data.frame(CL_EURUSD)
# gbp_usd_close.df <- as.data.frame(CL_GBPUSD)
# usd_chf_close.df <- as.data.frame(CL_USDCHF)
# SnP500_close.df <- as.data.frame(CL_SnP500)

# WRITE THE FILES
# write.csv(bitco_close.df, file = "bitcoin-export.csv")
# write.csv(DAX.df, file = "DAX-EXPORT.csv")
# write.csv(eur_usd_close.df, file = "EUR-USD-EXPORT.csv")
# write.csv(gbp_usd_close.df, file = "GBP-USD-EXPORT.csv")
# write.csv(usd_chf_close.df, file = "USD-CHF-EXPORT.csv")
# write.csv(SnP500_close.df, file = "SnP500-EXPORT.csv")

# FIRST - SAVE THEM AS EXCEL TO ADD ALL THE MISSING DATES
# USING MATCH FUNCTION ON EXCEL

# SECOND - SAVE THE NEW FILES WITH ALL DATES AS CSV 

# NOW IMPORT ALL THE CSV FILES 
# setwd("C:/Users/rstef/Desktop/DISSERTATION/data-to-match")

# GOLD 
gold_all <- read.csv("Gold-all-csv.csv", header = T)
#SILVER
silver_all <- read.csv("Silver-all-csv.csv", header = T)
# LIBOR OVERNIGHT 
LIBOR_all <- read.csv("LIBOR-USD-all-csv.csv", header = T)
# S&P 500
SnP500_all <- read.csv("SnP500-EXPORT-all-csv.csv", header = T)
# DAX
DAX_all <- read.csv("DAX-EXPORT-all-csv.csv", header = T)
# EUR USD
eur_usd_all <- read.csv("EUR-USD-EXPORT-all-csv.csv", header = T)
# GBP USD
gbp_usd_all <- read.csv("GBP-USD-EXPORT-all-csv.csv", header = T)
# USD CHF
usd_chf_all <- read.csv("USD-CHF-EXPORT-all-csv.csv", header = T)

# TWE USD
TWE <- read.csv("DTWEXM-daily.csv", header = T)
sum(is.na(TWE))

# DELETE UNNECESSARY COLUMNS 
gold_all <- gold_all[,c(1,2)]
silver_all <- silver_all[,c(1,2)]
LIBOR_all <- LIBOR_all[,c(1,2)]
SnP500_all <- SnP500_all[,c(1,2)]
DAX_all <- DAX_all[,c(1,2)]
eur_usd_all <- eur_usd_all[,c(1,2)]
gbp_usd_all <- gbp_usd_all[,c(1,2)]
usd_chf_all <- usd_chf_all[,c(1,2)]

#====================================
# STEPS TO INTERPOLATE THE MISSING DAYS 

# set first column as a date
gold_all$Date <- as.Date(gold_all$Date)
silver_all$Date <- as.Date(silver_all$Date)
LIBOR_all$Date <- as.Date(LIBOR_all$Date)
SnP500_all$Date <- as.Date(SnP500_all$Date)
DAX_all$Date <- as.Date(DAX_all$Date)
eur_usd_all$Date <- as.Date(eur_usd_all$Date)
gbp_usd_all$Date <- as.Date(gbp_usd_all$Date)
usd_chf_all$Date <- as.Date(usd_chf_all$Date)

# TWE 
TWE$Date <- as.Date(TWE$Date)

# second, set them as xts (xts package needed)

install.packages("xts", repos="http://cloud.r-project.org")

gold_all_xts <- xts(gold_all[,-1], order.by = gold_all$Date)
silver_all_xts <- xts(silver_all[,-1], order.by = silver_all$Date)
LIBOR_all_xts <- xts(LIBOR_all[,-1], order.by = LIBOR_all$Date)
SnP500_all_xts <- xts(SnP500_all[,-1], order.by = SnP500_all$Date)
DAX_all_xts <- xts(DAX_all[,-1], order.by = DAX_all$Date)
eur_usd_all_xts <- xts(eur_usd_all[,-1], order.by = eur_usd_all$Date)
gbp_usd_all_xts <- xts(gbp_usd_all[,-1], order.by = gbp_usd_all$Date)
usd_chf_all_xts <- xts(usd_chf_all[,-1], order.by = usd_chf_all$Date)

# TWE 
TWE_xts <- xts(TWE[,-1], order.by = TWE$Date)

# interpolate
F_GOLD <-na.approx(gold_all_xts)
F_SILVER <-na.approx(silver_all_xts)
F_LIBOR <-na.approx(LIBOR_all_xts)
F_SnP500 <-na.approx(SnP500_all_xts)
F_DAX <-na.approx(DAX_all_xts)
F_EURUSD <-na.approx(eur_usd_all_xts)
F_GBPUSD <-na.approx(gbp_usd_all_xts)
F_USDCHF <-na.approx(usd_chf_all_xts)

Crypto_all <- cbind(CL_BTC,CL_XRP,CL_LTC, CL_ETH)
summary(Crypto_all)

# TWE
F_TWE <- na.approx(TWE_xts)

#name columns
names(F_GOLD) <- "GOLD"
names(F_SILVER) <- "SILVER"
names(F_LIBOR) <- "LIBOR OVERNIGHT"
names(F_SnP500) <- "SnP500"
names(F_DAX) <- "DAX"
names(F_EURUSD) <- "EUR-USD"
names(F_GBPUSD) <- "GBP-USD"
names(F_USDCHF) <- "USD-CHF"

names(F_TWE) <- "USD_TWE"

F_all <- F_GOLD
F_all$SILVER <- F_SILVER
F_all$LIBOR <- F_LIBOR
F_all$SnP500 <- F_SnP500
F_all$DAX <- F_DAX
F_all$EURUSD <- F_EURUSD
F_all$GBPUSD <- F_GBPUSD
F_all$USDCHF <- F_USDCHF


write.csv(head(F_all),file = "head(F_all).csv")

# CALCULATING DAILY DIFFERENCED LOG PRICES 

# CRYPTOS
R_F_BTC <- diff(log(CL_BTC))*100
R_F_XRP <- diff(log(CL_XRP))*100
R_F_ETH <- diff(log(CL_ETH))*100
R_F_LTC <- diff(log(CL_LTC))*100

# REGRESSORS
R_F_GOLD <- diff(log(F_GOLD))*100
R_F_SILVER <- diff(log(F_SILVER))*100
R_F_LIBOR <- diff(log(F_LIBOR))*100
R_F_SnP500 <- diff(log(F_SnP500))*100
R_F_DAX <- diff(log(F_DAX))*100
R_F_EURUSD <- diff(log(F_EURUSD))*100
R_F_GBPUSD <- diff(log(F_GBPUSD))*100
R_F_USDCHF <- diff(log(F_USDCHF))*100

# TRADE WEIGHTED USD
R_F_TWE <- diff(log(F_TWE))

# creating the descriptive statistic for all datasets

# PUT THEM IN ONE TABLE
R_F_all <- R_F_BTC

# renaming the first column
names(R_F_all) [names(R_F_all) == "BTC-USD.Close"] <- "BTC"

# then adding the rest: 

# CRYPTOS
R_F_all$XRP <- R_F_XRP
R_F_all$LTC <- R_F_LTC
R_F_all$ETH <- R_F_ETH

# MACRO DATA 
R_F_all$GOLD <- R_F_GOLD
R_F_all$SILVER <- R_F_SILVER
R_F_all$LIBOR <- R_F_LIBOR
R_F_all$SnP500 <- R_F_SnP500
R_F_all$DAX <- R_F_DAX
R_F_all$EURUSD <- R_F_EURUSD
R_F_all$GBPUSD <- R_F_GBPUSD
R_F_all$USDCHF <- R_F_USDCHF
R_F_all$ETH <- R_F_ETH

# extract descriptive statistics
library(psych)

describe(R_F_all)

info_all <- describe(R_F_all)

# save the table that shows up 
write.csv(info_all, file = "Summary-all-final.csv")

head(NA_R_all.df)

cor(NA_R_all.df)
# OBTAINING THE AR(1) COEFFICIENTS =============

# TEMPLATE 
# mod <- arima(RETURNS PER ASSET, order = c(1,0,0))
# mod$coef # EXTRACTING THE AR(1) COEFFICIENT

library(tseries)

mod <- arma(NA_R_all.df$BTC, order = c(1,0))
summary(mod) # EXTRACTING THE AR(1) COEFFICIENT


# BTC = 0.0078302
# XRP = 0.010829
# LTC = 0.015558
# ETH = 0.052550 * (0.01)
# GOLD = -8.289e-02 *** (0)
# SILVER = -1.294e-01 *** (0)
# LIBOR = 0.0290950
# SnP500 = 0.0590835 ** (0.001)
# DAX = 0.0463916 * (0.01)
# EURUSD = 3.076e-02
# GBPUSD = 0.0702351 ** 
# USDCHF = 2.824e-02

# remove the initial NA value 

# set the xts files as numeric
NA_BTC <- as.numeric(R_F_BTC)
NA_ETH <- as.numeric(R_F_ETH)
NA_XRP <- as.numeric(R_F_XRP)
NA_LTC <- as.numeric(R_F_LTC)

NA_GOLD <- as.numeric(R_F_GOLD)
NA_SILVER <- as.numeric(R_F_SILVER)
NA_LIBOR <- as.numeric(R_F_LIBOR)
NA_SnP500 <- as.numeric(R_F_SnP500)
NA_DAX <- as.numeric(R_F_DAX)
NA_EURUSD <- as.numeric(R_F_EURUSD)
NA_GBPUSD <- as.numeric(R_F_GBPUSD)
NA_USDCHF <- as.numeric(R_F_USDCHF)

NA_TWE <- as.numeric(R_F_TWE)

# removing the initial NA row
NA_BTC <- NA_BTC[!is.na(NA_BTC)]
NA_ETH <- NA_ETH[!is.na(NA_ETH)]
NA_XRP <- NA_XRP[!is.na(NA_XRP)]
NA_LTC <- NA_LTC[!is.na(NA_LTC)]

NA_GOLD <- NA_GOLD[!is.na(NA_GOLD)]
NA_SILVER <- NA_SILVER[!is.na(NA_SILVER)]
NA_LIBOR <- NA_LIBOR[!is.na(NA_LIBOR)]
NA_SnP500 <- NA_SnP500[!is.na(NA_SnP500)]
NA_DAX <- NA_DAX[!is.na(NA_DAX)]
NA_EURUSD <- NA_EURUSD[!is.na(NA_EURUSD)]
NA_GBPUSD <- NA_GBPUSD[!is.na(NA_GBPUSD)]
NA_USDCHF <- NA_USDCHF[!is.na(NA_USDCHF)]

NA_TWE <- NA_TWE[!is.na(NA_TWE)]


NA_all_numeric <- cbind(NA_BTC,
                        NA_XRP,
                        NA_LTC,
                        NA_GOLD,
                        NA_SILVER,
                        NA_LIBOR,
                        NA_SnP500,
                        NA_DAX,
                        NA_EURUSD,
                        NA_GBPUSD,
                        NA_USDCHF)
# BACK TO DATA FRAMES ==========
library(zoo)
# CRYPTOS ====
R_F_BTC.df <- data.frame(date=index(R_F_BTC), coredata(R_F_BTC))
R_F_XRP.df <- data.frame(date=index(R_F_XRP), coredata(R_F_XRP))
R_F_ETH.df <- data.frame(date=index(R_F_ETH), coredata(R_F_ETH))
R_F_LTC.df <- data.frame(date=index(R_F_LTC), coredata(R_F_LTC))

# REGRESSORS ===========
R_F_SILVER.df <- data.frame(date=index(R_F_SILVER), coredata(R_F_SILVER))
R_F_GOLD.df <- data.frame(date=index(R_F_GOLD), coredata(R_F_GOLD))
R_F_LIBOR.df <- data.frame(date=index(R_F_LIBOR), coredata(R_F_LIBOR))
R_F_SnP500.df <- data.frame(date=index(R_F_SnP500), coredata(R_F_SnP500))
R_F_DAX.df <- data.frame(date=index(R_F_DAX), coredata(R_F_DAX))
R_F_EURUSD.df <- data.frame(date=index(R_F_EURUSD), coredata(R_F_EURUSD))
R_F_GBPUSD.df <- data.frame(date=index(R_F_GBPUSD), coredata(R_F_GBPUSD))
R_F_USDCHF.df <- data.frame(date=index(R_F_USDCHF), coredata(R_F_USDCHF))

# TWE ===================
R_F_TWE.df <- data.frame(date=index(R_F_TWE), coredata(R_F_TWE))

# PUTTING ALL RETURNS INTO ONE TABLE (DATA FRAME TYPE) ============
R_all.df <- R_F_SILVER.df

R_all.df$BTC <- R_F_BTC.df[,2]
R_all.df$XRP <- R_F_XRP.df[,2]
R_all.df$LTC <- R_F_LTC.df[,2]

R_all.df$ETH <- R_F_ETH.df[,2] 
## ETH NEEDS A SPECIAL TABLE, NOT MATCHING OBSERVATION COUNTTS

R_all.df$GOLD <- R_F_GOLD.df[,2]
R_all.df$LIBOR <- R_F_LIBOR.df[,2]
R_all.df$SnP500 <- R_F_SnP500.df[,2]
R_all.df$DAX <- R_F_DAX.df[,2]
R_all.df$EURUSD <- R_F_EURUSD.df[,2]
R_all.df$GBPUSD <- R_F_GBPUSD.df[,2]
R_all.df$USDCHF <- R_F_USDCHF.df[,2]


write.csv(R_all.df, file = "R_all.df.csv")
write.csv(R_F_ETH.df, file = "R_F_ETH.df.csv")
write.csv(R_F_TWE.df, file = "R_F_TWE.df.csv")

# RENAME THE FIRST COLUMN
names(R_all.df) [names(R_all.df)=="daily.returns"] <- "SILVER"
summary(R_all.df)

# correlation matrix ===============
library(psych)

library(Hmisc)
corr.test(NA_all_numeric)

str(correlation.test)

write.csv(cor(NA_all_numeric), file = "CORRELATIONS.csv")
write.csv(NA_R_all.df, file = "NA_R_all.df")

# REMOVE THE FIRST OBSERVATIONs ==================================================

# FOR THE WHOLE TABLE

NA_R_all.df <- R_all.df[-1,]

# AND FOR THE TWE 
R_F_TWE.df <- R_F_TWE.df[-1,]

# ================= GJR GARCH MODEL SPECIFICATIONS =====================================

# ====== BITCOIN ===========
library(rugarch)

spec <- ugarchspec(variance.model = list(model = "gjrGARCH", 
                                         garchOrder = c(1,1)
                                         ),
                   mean.model = list(armaOrder = c(1,0))
                   )

setbounds(spec)<-list(alpha1=c(-1,1))

garch <- ugarchfit(spec = spec, data = NA_R_all.df[,10])
garch

# ====== RIPPLE ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,11])
garch

# ====== LITECOIN ==========
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,12])
garch

# ====== ETHEREUM ==========
# special case
NA_R_and_ETH.df <- read.csv("finding-eth-csv.csv")
garch <- ugarchfit(spec = spec, data = NA_R_and_ETH.df[,5])
garch

# ====== GOLD ==============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,3])
garch

# ====== SILVER ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,2])
garch

# ====== LIBOR =============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,4])
garch

# ====== SnP500 ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,5])
garch

# ====== DAX ===============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,6])
garch

# ====== EURUSD ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,7])
garch

# ====== GBPUSD ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,8])
garch

# ====== USDCHF ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,9])
garch


# ================= GJR GARCH MODEL SPECIFICATIONS =====================================
# ============ WITH A REGRESSOR - USD TRADE WEIGHTED =======

R_F_TWE.df[,2] # to be used in the mean equation

# ===== SPECIFY THE MODEL TO FIT =============
library(rugarch)

spec <- ugarchspec(variance.model = list(model = "gjrGARCH", 
                                         garchOrder = c(1,1)
),
mean.model = list(armaOrder = c(1,0),
                  external.regressors = matrix(R_F_TWE.df[,2],ncol = 1))
)

setbounds(spec)<-list(alpha1=c(-1,1))

# EXECUTE FOR EVERY ASSET

# ====== BITCOIN ===========

garch <- ugarchfit(spec = spec, data = NA_R_all.df[,10])
garch

# ====== RIPPLE ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,11])
garch

# ====== LITECOIN ==========
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,12])
garch

# ====== ETHEREUM (SPECIAL CASE) ==========
# ====== SPECIFY THE MODEL TO FIT 
library(rugarch)
NA_R_and_ETH.df <- read.csv("finding-eth-csv.csv")
spec_ETH <- ugarchspec(variance.model = list(model = "gjrGARCH", 
                                         garchOrder = c(1,1)
),
mean.model = list(armaOrder = c(1,0),
                  external.regressors = matrix(NA_R_and_ETH.df[,14]))
)

setbounds(spec_ETH)<-list(alpha1=c(-1,1))
garch <- ugarchfit(spec = spec_ETH, data = NA_R_and_ETH.df[,5])
garch

# ====== GOLD ==============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,3])
garch
# ====== SILVER ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,2])
garch

# ====== LIBOR =============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,4])
garch

# ====== SnP500 ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,5])
garch
pacf(R_F_TWE[-1,]*R_F_SnP500[-1,])
length(which((R_F_TWE[-1,]*R_F_SnP500[-1,])<0))/nrow(R_F_TWE[-1,])


# ====== DAX ===============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,6])
garch

# ====== EURUSD ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,7])
garch

# ====== GBPUSD ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,8])
garch

# ====== USDCHF ============
garch <- ugarchfit(spec = spec, data = NA_R_all.df[,9])
garch




# ========== test below ==========================
##Box-Jenkins Methodology for the mean model choice
#ACF and PACF analysis to check possible model patterns
library(quantmod)
test.bitcoin <- dailyReturn(CL_BTC)


par(mfrow=c(1,2))
acf(NA_LTC)
pacf(NA_LTC)
#lag 5 seems important; tests will be runned with ARMA (5,0), (0,5) e (5,5)
library(tseries)

ARMA.X.0 <- arma(NA_LTC, order=c(6,0))
summary(ARMA.X.0)
acf(residuals(ARMA.X.0), na.action=na.remove)
pacf(residuals(ARMA.X.0), na.action=na.remove)
# shows discrete White noise pattern

ARMA0.10 <- arma(NA_XRP, order=c(0,6))
summary(ARMA0.10)
par(mfrow=c(1,2))
acf(residuals(ARMA0.10), na.action=na.remove)
pacf(residuals(ARMA0.10), na.action=na.remove)
# shows discrete White noise pattern

# ARMA (10, 10)
ARMA10.10 <- arma(NA_XRP, order=c(18,18))
summary(ARMA10.10)


#================= Bitcoin =======================

btcfinal.aic <- Inf
btcfinal.order <- c(0,0,0)
for (p in 1:10) for (d in 0:1) for (q in 1:10) {
    btccurrent.aic <- AIC(arima(NA_BTC, order = c(p,d,q)))
    if (btccurrent.aic < btcfinal.aic) {
      btcfinal.aic = btccurrent.aic
      btcfinal.order = c(p,d,q)
      btcfinal.arima = arima(NA_BTC, order = btcfinal.order)
    }

}
btcfinal.aic
btcfinal.order # 5 0 10 order
acf(resid(btcfinal.arima))
acf(resid(btcfinal.arima)^2)
# they observe a strong serial correlation in the square resid.

# Ljung-Box test 
Box.test(resid(btcfinal.arima), lag=20, type = "Ljung-Box")
# the p-value is more than 0.05 so we assume the fit is good (0.996)

# fitting GARCH MODEL
library(tseries)
btc.garch <- garch(NA_BTC, trace=F)

# calling a summary of GARCH MODEL to see the order 
print(btc.garch) # or can use this: 
btc.garch$order # it is GARCH (1,1)

btc.res <- btc.garch$residuals[-1]
acf(btc.res) # there are still some serial correlations, 
# but they might be due to sampling bias.

acf(btc.res^2)
# resemble DWN, no ser. correlation observed

summary(btc.garch) # the Ljung box shows
# p-value above 0.05 => good fit


# ================ ETHEREUM ==========================================
ethfinal.aic <- Inf
ethfinal.order <- c(0,0,0)
for (p in 1:10) for (d in 0:1) for (q in 1:10) {
  ethcurrent.aic <- AIC(arima(NA_ETH, order = c(p,d,q)))
  if (ethcurrent.aic < ethfinal.aic) {
    ethfinal.aic = ethcurrent.aic
    ethfinal.order = c(p,d,q)
    ethfinal.arima = arima(NA_ETH, order = ethfinal.order)
  }
  
}
ethfinal.aic
ethfinal.order # 10 0 9 
acf(resid(ethfinal.arima), na.action = na.omit)
acf(resid(ethfinal.arima)^2) # shows small significance at the 3rd res.

# Ljung-Box test 
Box.test(resid(ethfinal.arima), lag=20, type = "Ljung-Box")
# the p-value is more than 0.05 (0.2979) => good model fit. 

library(tseries)
eth.garch <- garch(NA_ETH, trace=F)
eth.garch$order
# it is GARCH (1,1)

eth.res <- eth.garch$residuals[-1]
head(eth.garch$residuals)

acf(eth.res)

acf(eth.res^2)
# resemble DWN

# cI 
confint(eth.garch)

# calling a summary of GARCH MODEL !!!!!!!!!!1
print(eth.garch)
# the estimates of the GARCH(1,1) ARE WITHIN THE CI

# =============== RIPPLE ===============================================
xrpfinal.aic <- Inf
xrpfinal.order <- c(0,0,0)
for (p in 1:10) for (d in 0:1) for (q in 1:10) {
  xrpcurrent.aic <- AIC(arima(NA_XRP, order = c(p,d,q)))
  if (xrpcurrent.aic < xrpfinal.aic) {
    xrpfinal.aic = xrpcurrent.aic
    xrpfinal.order = c(p,d,q)
    xrpfinal.arima = arima(NA_XRP, order = xrpfinal.order)
  }
  
}
xrpfinal.aic
xrpfinal.order # 7 0 8 
acf(resid(xrpfinal.arima), na.action = na.omit)
acf(resid(xrpfinal.arima)^2)
# observe a strong serial correlation in the square resid.

# Ljung-Box test 
Box.test(resid(xrpfinal.arima), lag=20, type = "Ljung-Box")
# the p-value is more than 0.05 (0.9021) so we assume the fit is good

library(tseries)
xrp.garch <- garch(NA_XRP, trace=F)
xrp.garch$order
# it is GARCH (1,1)

xrp.res <- xrp.garch$residuals[-1]
head(eth.garch$residuals)

acf(xrp.res)

acf(xrp.res^2)
# resemble DWN

# calling a summary of GARCH MODEL !
print(xrp.garch)

# the estimates of the GARCH(1,1) ARE WITHIN THE CI

# ============== LITECOIN ====================================================
ltcfinal.aic <- Inf
ltcfinal.order <- c(0,0,0)
for (p in 1:10) for (d in 0:1) for (q in 1:10) {
  ltccurrent.aic <- AIC(arima(NA_LTC, order = c(p,d,q)))
  if (ltccurrent.aic < ltcfinal.aic) {
    ltcfinal.aic = ltccurrent.aic
    ltcfinal.order = c(p,d,q)
    ltcfinal.arima = arima(NA_LTC, order = ltcfinal.order)
  }
  
}
ltcfinal.aic
ltcfinal.order # 10 0 2
acf(resid(ltcfinal.arima), na.action = na.omit)
# we observe DWN pattern 

acf(resid(ltcfinal.arima)^2)
# observe a strong serial correlation in the square resid.

# Ljung-Box test 
Box.test(resid(ltcfinal.arima), lag=20, type = "Ljung-Box")
# the p-value is more than 0.05 (0.7676) so we assume the fit is good

library(tseries)
ltc.garch <- garch(NA_LTC, trace=F)
ltc.garch$order
# CI
confint(ltc.garch)
# seems like it is garch (1,1)

ltc.res <- ltc.garch$residuals[-1]

acf(ltc.res)

acf(ltc.res^2)
# resemble DWN

# calling a summary of GARCH MODEL !
print(ltc.garch)
# the estimates of the GARCH(1,1) ARE WITHIN THE CI




# SPECIFICATIONS FOR THE GARCH(1,1) MODELS WITH 
# EXTERNAL REGRESSORS


# adding explanatory variables to the garch model 
F.ext.reg <- cbind(NA_R_all.df$SILVER,
               NA_R_all.df$GOLD, 
               NA_R_all.df$LIBOR,
               NA_R_all.df$SnP500,
               NA_R_all.df$DAX,
               NA_R_all.df$EURUSD,
               NA_R_all.df$GBPUSD,
               NA_R_all.df$USDCHF
               )
NA_R_all.df$XRP <- NA_R_XRP.df[,2]
NA_R_all.df$LTC <- NA_R_LTC.df[,2]
NA_R_all.df$ETH <- 


#  ============== BTC ========================================================
library(rugarch)
spec <- ugarchspec(variance.model = list(model = "gjrGARCH", 
                                         garchOrder = c(1,1)
                                        ),
                   mean.model = list(armaOrder = c(1,0), 
                                     external.regressors = matrix(NA_R_F_SnP500.df[,2], ncol = 1)
                                     )
                   )


garch <- ugarchfit(spec = spec, data = NA_R_F_BTC.df[,2])

garch@fit$coef
# BTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTCBTC

garch.btc <- garch(NA_R_all.df$BTC, order(c(1,1)), trace=FALSE)
summary(garch.btc)

head(garch.btc$residuals) # =>
resid <- garch.btc$residuals[!is.na(garch.btc$residuals)]

acf(resid) # three spikes at k = 1, 6 and 10, but
acf(resid^2) # no serial correlation


garch.res <- garch@fit$residuals
garch.res2 <- (garch@fit$residuals)^2 # save the estimated sqrd residuals
acf(garch.res2)

Box.test(garch.res2, lag=20, type = "Ljung-Box")
library(aTSA)
stationary.test(garch.res2, nlag = 13)
# both tests show stationarity in the residuals 

# looking for normality in the residuals 
# library (tseries)
test <- jarque.bera.test(garch.res) 
# not normal, however we can get away with it as given by: 
# The Importance of the Normality Assumption in Large 
# Public Health Data Sets.
str(test)

hist(garch.res)
# account for any arch effects 

arch.test(arima(garch.res,order=c(1,0,0)),output=TRUE)


#=============== XRP ================================================
spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                         garchOrder = c(1,1),
                                         external.regressors = matrix(F.ext.reg, ncol = 8),
                                         submodel = NULL
),
mean.model = list(armaOrder = c(4,3), 
                  external.regressors = matrix(F.ext.reg, ncol = 8)
)
)


garch <- ugarchfit(spec = spec, data = data[,11], solver.control = list(trace = 0))

print(garch)

acf(garch@fit$residuals) # has serial correlation

head(garch@fit$residuals)

garch.res <- garch@fit$residuals
garch.res2 <- (garch@fit$residuals)^2 # save the estimated sqrd residuals
acf(garch.res2)

Box.test(garch.res2, lag=20, type = "Ljung-Box")

library(aTSA)
stationary.test(garch.res2, nlag = 13)
# both tests show stationarity in the residuals 

# looking for normality in the residuals 
# library (tseries)
test <- jarque.bera.test(garch.res) 
# not normal, however we can get away with it as given by: 
# The Importance of the Normality Assumption in Large 
# Public Health Data Sets.
str(test)

hist(garch.res)
# account for any arch effects 

arch.test(arima(garch.res,order=c(1,0,0)),output=TRUE)






