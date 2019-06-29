# Author - David Farrugia
# Description - Analysing the effects of Brexit on the financial markets.
# Date - 2019

# set working directory

setwd("enter/your/folder/path/here") 

# --- LIBRARIES --- #

library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(fBasics)
library(ggplot2)
library(data.table)
library(flipTime)
library(urca)
library(forecast)
library(TSA)
library(FinTS)
library(rugarch)
library(tseries)
library(fGarch)
library(dplyr)
library(CausalImpact)
library(crypto)
library(coinmarketcapr)

# --- FILE PATHS ---

gbp.exchange.data.path <- "gbp.exchange.index.csv"
ukimport.data.path <- "ukimport.csv"
ukexport.data.path <- "ukexport.csv"
cpi.data.path <- "cpi.csv"

# --- FUNCTIONS --- #

# convert xts objects to data.frame
xts2dataframe <- function(xts.data) {
  final.df <- data.frame(year = factor(year(index(xts.data))), value = coredata(xts.data))
  colnames(final.df) <- c( "year", "value")
  final.df
}

# get lost basic stats row names
stats.rownames <- rownames(basicStats(rnorm(10,0,1)))

# generate df from descriptive stats per year
df.stats <- function(df) {
  df1 <- with(df, tapply(value, year, basicStats))
  df2 <- do.call(cbind, df1)
  rownames(df2) <- stats.rownames
  as.data.frame(df2)
}

# plot cpi
draw.cpi <- function(df, cpi, title) {
  ggplot(aes(x = Date, y = df[[grep(cpi, colnames(df))[1]]]), data = df) + 
    geom_line(col = "black") + 
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    xlab("Year") + ylab("CPI Value") +
    geom_vline(aes(xintercept = as.Date("2016-06-23")),
               linetype = 4, colour = "red") +
    geom_text(aes(x = as.Date("2016-06-23"), 
                  label = "Brexit Voting", y = 70), 
              colour = "red") +
    geom_vline(aes(xintercept = as.Date("2018-07-06")),
               linetype = 4, colour = "blue") +
    geom_text(aes(x = as.Date("2018-07-06"), 
                  label = "Resign", y = 70), 
              colour = "blue") +
    geom_vline(aes(xintercept = as.Date("2019-01-15")),
               linetype = 4, colour = "green") +
    geom_text(aes(x = as.Date("2019-01-15"), 
                  label = "Lost", y = 70), 
              colour = "green") +
    geom_vline(aes(xintercept = as.Date("2017-12-08")),
               linetype = 4, colour = "orange") +
    geom_text(aes(x = as.Date("2017-12-08"), 
                  label = "Backstop", y = 70),
              colour = "orange") +
    geom_vline(aes(xintercept = as.Date("2017-06-08")),
               linetype = 4, colour = "violet") +
    geom_text(aes(x = as.Date("2017-06-08"), 
                  label = "Election", y = 70), 
              colour = "violet")
}

# box plot per year and value
draw.box.yearvalue <- function(df, title) {
  p <- ggplot(data = df, aes(x = year, y = value)) + theme_bw() + theme(legend.position = "none") + geom_boxplot(fill = "lightblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(title) + xlab('year') + ylab("log returns")
  p
}

# density plot per year
draw.density.year <- function(df, title) {
  p <- ggplot(data = df, aes(x = value)) + geom_density(fill = "lightblue") + facet_wrap(. ~ year)+ theme_bw() + theme(legend.position = "none") + ggtitle(title)
  p
}

# rename data.frame columns
rename.cols <- function(df, old.name, new.name) {
  setnames(df, old = old.name, new = new.name)
}

# q-q plot per year
draw.qq.year <- function(df, title) {
  p <- ggplot(data = df, aes(sample = value)) + stat_qq(colour = "blue") + stat_qq_line() + facet_wrap(. ~ year) + theme_bw() + theme(legend.position = "none") + ggtitle(title)
  p
}

# shapiro-wilk normality test
shapiro.pvalue <- function (v) {
  shapiro.test(v)$p.value
}

# shapiro-wilk p-value for every year
shapiro <- function(df) {
  result <- with(df, tapply(value, year, shapiro.pvalue))
  as.data.frame(result)
}

# dickey-fuller test for unit roots
dickey <- function(df) {
  dickey.test <- floor(12 * (nrow(df)/100) ^ (1/4))
  summary(ur.df(df, type = "none", lags = dickey.test, selectlags = "BIC"))
}

# calculate inflation percentage between 2 dates for a given index
calc.inflation <- function(data, start, end, cpi) {
  ((data[which(data$Date == AsDate(end)), 
         grep(cpi, colnames(data))[1]] - 
      data[which(data$Date == AsDate(start)), 
           grep(cpi, colnames(data))[1]])/data[which(data$Date == AsDate(start)), 
                                               grep(cpi, colnames(data))[1]]) * 100
}

# --- MAIN --- #

# clear variables
rm(list = ls())

# --- GBP/EUR ANALYSIS --- #

# get GBP/EUR data from yahoo (2014 onwards)
setSymbolLookup(GBP.EUR = list(name = "GBPEUR=X", src = "yahoo"))
getSymbols(c("GBP.EUR"), from = "2014-01-01")

par(mfrow=c(2,2))

# chart plotting with different statistics
chartSeries(GBP.EUR, subset = "2019", TA = NULL, type = "bars")
addMACD() # below signal = time to sell, spike above = going back down soon
addBBands() # shows volatility
addCCI() # > 0 above historic average, > 100 strong upside
addADX()
addEnvelope()
addROC(n = 1)  # bull > 0, bear < 0
addRSI() # uptrend 40-90 (40-50 support), downtrend 10-60 (50-60 resistance)
addSAR() # direction (dot below = uptrend, dot above = downtrend)
addEMA() # moving average
addSMI() # bull > 40, bear < -40
addTRIX() # buy > above 0, sell < 0 - buy/sell on change of direction

# adjusted closing values
gbp.eur.close <- GBP.EUR[,"GBPEUR=X.Adjusted"]
# calculate log returns
gbp.log.return <- CalculateReturns(gbp.eur.close, method = "log")
# omit na values
gbp.log.return <- na.omit(gbp.log.return)

# descriptive stats for log returns
basicStats(gbp.log.return)

# plot log returns
plot(gbp.log.return,
     main = "GBP/EUR closing log returns 2014-2019")

plot(density(gbp.log.return))

# histogram for the log returns
hist(gbp.log.return, prob = TRUE, col = "grey", xlab = "Log Returns", main="Histogram of Log Returns of EUR/GBP Exchange Rate")
xfit <- seq(min(gbp.log.return), max(gbp.log.return), length=40)
yfit <- dnorm(xfit,mean = mean(gbp.log.return), sd = sd(gbp.log.return))
lines(xfit, yfit, col="blue", lwd=2)

# convert xts object to a dataframe by year
gbp.df <- xts2dataframe(gbp.log.return)

# run basic statistics
gbp.stats <- df.stats(gbp.df)

# order year statistics based on variance, skewness, and kurtosis
gbp.stats["Variance", order(gbp.stats["Variance",,])]
gbp.stats["Skewness", order(gbp.stats["Skewness",,])]
gbp.stats["Kurtosis", order(gbp.stats["Kurtosis",,])]
gbp.stats["Mean", order(gbp.stats["Mean",,])]

# plots per year - daily log returns
draw.box.yearvalue(gbp.df, "GBP/EUR daily log-returns box plots 2014-2019")
draw.density.year(gbp.df, "GBP/EUR daily log-returns density plots 2014-2019")

# normality check
draw.qq.year(gbp.df, "GBP/EUR daily log-returns QQ plots 2014-2019")
# qqplots show a strong departure from normality

# statistical test for independence
Box.test(gbp.log.return, lag = 30, type = "Ljung")
# all lags p > 0.05 thus no significant autocorrelation (we can't prove independence)

# run normality tests
jarque.bera.test(gbp.log.return) # normality test on entire data set
shapiro(gbp.df) # null hypothesis of normality rejected for all years

# get weekly log return values
gbp.weekly <- apply.weekly(gbp.log.return, sum)
plot(gbp.weekly, main = "Weekly GBP/EUR log returns") # shows high volatility - multiple spikes

gbp.weekly.df <- xts2dataframe(gbp.weekly)

# plots per year - weekly log returns
draw.box.yearvalue(gbp.weekly.df, "GBP/EUR weekly log-returns box plots 2014-2019")
draw.density.year(gbp.weekly.df, "GBP/EUR weekly log-returns density plots 2014-2019")

# normality check
draw.qq.year(gbp.weekly.df, "GBP/EUR weekly log-returns QQ plots 2014-2019")
shapiro(gbp.weekly.df) # weekly log returns fails to reject null hypothesis

gb.return.outlier <- Return.clean(gbp.log.return, "boudt") # boudt to reduce the magnitude
p <- plot(gbp.log.return, main = 'GBP/EUR log return outlier detection') 
p <- addSeries(gb.return.outlier, col = 'blue', on = 1) # plot the cleaned ts over the full ts
p # anomalies are shown in black

# auto-correlation plots
acf(gbp.log.return)
pacf(gbp.log.return)

dickey(gbp.log.return) # reject null hypothesis of unit roots

# get best AR-MA values for an arima model
auto_model <- auto.arima(gbp.log.return)
summary(auto_model) # ARMA (0,0) is recommended

# sample extended acf
eacf(gbp.log.return) # confirms 0,0 - top-right triangle at (0,0)

# run arch test on arima model residuals
rsd <- residuals(auto_model)
ArchTest(rsd - mean(rsd))

# 2 plots per row
par(mfrow=c(1,2))

# auto-correlation plots for residuals
acf(rsd)
pacf(rsd)

# set specification for garch model
spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE), 
                        variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                        distribution.model = "sstd")

(garch <- ugarchfit(data = gbp.log.return, spec = spec))

# No correlation within standardised residuals + standardised squared residuals is found.

par(mfrow=c(2,2))
plot(garch, which=8)
plot(garch, which=9)
plot(garch, which=10)
plot(garch, which=11)


# volatility plot of log returns using garch model
par(mfrow=c(1,1))
volatility <- sigma(garch)
mean_model <- fitted(garch)
p <- plot(gbp.log.return, col = "grey", main = 'GBP/EUR volatility plot')
p <- addSeries(mean_model, col = 'red', on = 1)
p <- addSeries(volatility, col = 'blue', on = 1)
p

par(mfrow=c(3,2))
p <- lapply(2014:2019, function(x) { plot(volatility[as.character(x)], main = "Log returns volatility")})
p

par(mfrow=c(1,1))
volatility.df <- xts2dataframe(volatility)
draw.box.yearvalue(volatility.df, "GBP/EUR Log Returns Volatility 2014-2018")

# first we determine whether the acf of squared logs show exponential decay
# and pacf shows a cut-off

acf(gbp.log.return^2, main="ACF of Squared GBP/EUR Log Returns")
pacf(gbp.log.return^2, main="PACF of Squared GBP/EUR Log Returns") # cut-off lag 14

fit <- garch(gbp.log.return - mean(gbp.log.return), order = c(1, 1), trac = FALSE)
# (1,1) - simple model due to many coeeficients being significant
# based off the eacf() we can try (0,0), (0,1), (1,1)

summary(fit)
# box-ljung test > 0.05, fail to reject null hypothesis
# Jarque Bera Test < 0.05, reject null hypothesis

# garch model
gfit <- garchFit(~garch(1,1), gbp.log.return, cond.dis="std")
cf <- coef(gfit)
summary(gfit)

# monte carlo simulation for risk value - 20 days
res <- c()
lamb <- sqrt((cf[5]-2)/cf[5]) * predict(gfit, 20)[, 3]
for (i in 1:100000) {
  lretr <- rt(20, cf[5])
  lret <- cf[1] + (lamb * lretr)
  res[i] <- sum(lret)
}

# risk value percentage
quantile(res, 0.05) * 100

# --- EFFECTIVE EXCHANGE RATE INDEX --- #

# load exchange index data
gbp.exchange.index <- read.csv(gbp.exchange.data.path)

# rename columns
rename.cols(gbp.exchange.index, 
            c('Title', 'Sterling.effective.exchange.rate.index..Monthly.average..Jan.2005.100.'), 
            c('Date', 'Value'))

# drop missing months and unwanted descriptions
gbp.exchange.index <- gbp.exchange.index[-c(1:221), ]

# convert date column to correct data type
gbp.exchange.index$Date <- AsDate(gbp.exchange.index$Date)

# subset the data based on a specific timeframe
gbpindex.2years <- 
  gbp.exchange.index[
    c(which
      (gbp.exchange.index$Date == '2016-01-01'):which
      (gbp.exchange.index$Date == '2019-03-01')),]

# exchange index plot with major event markers
ggplot(gbpindex.2years, aes(Date, Value, group = 1)) + geom_line() +
  scale_x_date() + xlab("") + ylab("GBP Effective Exchange Index") +
  ggtitle("GBP Effective Exchange Index: 01/2016 - 03/2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept = as.Date("2016-06-23")),
             linetype = 4, colour = "red") +
  geom_text(aes(x = as.Date("2016-06-23"), 
                label = "Brexit Voting", y = 10), 
            colour = "red") +
  geom_vline(aes(xintercept = as.Date("2018-07-06")),
             linetype = 4, colour = "blue") +
  geom_text(aes(x = as.Date("2018-07-06"), 
                label = "Resign", y = 10), 
            colour = "blue") +
  geom_vline(aes(xintercept = as.Date("2019-01-15")),
             linetype = 4, colour = "green") +
  geom_text(aes(x = as.Date("2019-01-15"), 
                label = "Lost", y = 10), 
            colour = "green") +
  geom_vline(aes(xintercept = as.Date("2017-12-08")),
             linetype = 4, colour = "orange") +
  geom_text(aes(x = as.Date("2017-12-08"), 
                label = "Backstop", y = 10),
            colour = "orange") +
  geom_vline(aes(xintercept = as.Date("2017-06-08")),
             linetype = 4, colour = "violet") +
  geom_text(aes(x = as.Date("2017-06-08"), 
                label = "Election", y = 10), 
            colour = "violet")

# --- UK IMPORT ANALYSIS --- #

# load uk import data
uk.import <- read.csv(ukimport.data.path)

# drop unwanted columns and rows
uk.import <- uk.import[-c(1:3), -c(1, 3:206)]

# filter for EU member states
uk.import.eu <- uk.import[c(1, 3:29),]
# filter for NON-EU member states
uk.import.noneu <- uk.import[-c(2:32),]

# transpose the data.frame
uk.import.eu <- t(uk.import.eu)
uk.import.noneu <- t(uk.import.noneu)

# restructuring the data.frame
colnames(uk.import.eu) <- c('Date', uk.import.eu[1, 2:ncol(uk.import.eu)])
colnames(uk.import.noneu) <- c('Date', uk.import.noneu[1, 2:ncol(uk.import.noneu)])
uk.import.eu <- uk.import.eu[-1, ]
uk.import.noneu <- uk.import.noneu[-1, ]

# transform to data.frame
uk.import.eu <- as.data.frame(uk.import.eu)
uk.import.noneu <- as.data.frame(uk.import.noneu)

# format date type
uk.import.eu$Date <- AsDate(uk.import.eu$Date)
uk.import.noneu$Date <- AsDate(uk.import.noneu$Date)

# check which columns are factors and convert to numeric type
indx <- sapply(uk.import.eu, is.factor)
uk.import.eu[indx] <- lapply(uk.import.eu[indx], function(x) as.numeric(as.character(x)))
indx <- sapply(uk.import.noneu, is.factor)
uk.import.noneu[indx] <- lapply(uk.import.noneu[indx], function(x) as.numeric(as.character(x)))

# calculate total import value per country
total.import.eu <- as.data.frame(t(uk.import.eu %>%
                                     summarize_if(is.numeric, sum, na.rm=TRUE)))
total.import.noneu <- as.data.frame(t(uk.import.noneu %>%
                                        summarize_if(is.numeric, sum, na.rm=TRUE)))

total.import.eu$Country <- rownames(total.import.eu)
total.import.noneu$Country <- rownames(total.import.noneu)
rownames(total.import.eu) <- NULL
rownames(total.import.noneu) <- NULL

# visualise total imports by country
ggplot(data = total.import.eu, aes(x = reorder(Country, -V1), y = V1, fill = Country)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Country") + ylab("Total") +
  ggtitle("UK Total Imports by Country (EU): 01/2015-01/2019") +
  theme(plot.title = element_text(hjust = 0.5))

# run statistics of total import
basicStats(total.import.eu$V1)
basicStats(total.import.noneu$V1)

# drop unwanted rows for non-eu
total.import.noneu <- total.import.noneu[-c(208:214), ]

# drop less than mean
import.top.country <- subset(total.import.eu, total.import.eu$V1 > 
                               basicStats(total.import.eu$V1)[
                                 which(rownames(basicStats(total.import.eu$V1)) == 'Mean'),],
                             select = Country)

# extract top 5 non-eu countries
import.top5.noneu <- head(total.import.noneu[with(total.import.noneu, order(-V1)), ], 5)

# convert data.frame from wide to long
uk.import.eu.melt <- melt(data = uk.import.eu, 
                          id.vars = "Date", 
                          measure.vars = c(2:ncol(uk.import.eu)),
                          value.name = 'Value',
                          variable.name = 'Country')

uk.import.noneu.melt <- melt(data = uk.import.noneu, 
                             id.vars = "Date", 
                             measure.vars = c(2:ncol(uk.import.noneu)),
                             value.name = 'Value',
                             variable.name = 'Country')

# keep only the top countries
uk.import.eu.melt <- uk.import.eu.melt[uk.import.eu.melt$Country 
                                       %in% import.top.country$Country, ]

uk.import.noneu.melt <- uk.import.noneu.melt[uk.import.noneu.melt$Country 
                                             %in% import.top5.noneu$Country, ]

ggplot(uk.import.eu.melt, aes(x = Date, y = Value)) + 
  geom_line(aes(color = Country), size = 1) +
  theme_minimal() +
  geom_vline(aes(xintercept = as.Date("2016-06-23")),
             linetype = 4, colour = "red") +
  geom_text(aes(x = as.Date("2016-06-23"), 
                label = "Brexit Voting", y = 10), 
            colour = "red") +
  geom_vline(aes(xintercept = as.Date("2018-07-06")),
             linetype = 4, colour = "blue") +
  geom_text(aes(x = as.Date("2018-07-06"), 
                label = "Resign", y = 10), 
            colour = "blue") +
  geom_vline(aes(xintercept = as.Date("2019-01-15")),
             linetype = 4, colour = "green") +
  geom_text(aes(x = as.Date("2019-01-15"), 
                label = "Lost", y = 10), 
            colour = "green") +
  geom_vline(aes(xintercept = as.Date("2017-12-08")),
             linetype = 4, colour = "orange") +
  geom_text(aes(x = as.Date("2017-12-08"), 
                label = "Backstop", y = 10),
            colour = "orange") +
  geom_vline(aes(xintercept = as.Date("2017-06-08")),
             linetype = 4, colour = "violet") +
  geom_text(aes(x = as.Date("2017-06-08"), 
                label = "Election", y = 10), 
            colour = "violet") +
  ggtitle('UK Imports from Major Runners (EU): 2015 - 2019')

ggplot(uk.import.noneu.melt, aes(x = Date, y = Value)) + 
  geom_line(aes(color = Country), size = 1) +
  theme_minimal() +
  geom_vline(aes(xintercept = as.Date("2016-06-23")),
             linetype = 4, colour = "red") +
  geom_text(aes(x = as.Date("2016-06-23"), 
                label = "Brexit Voting", y = 10), 
            colour = "red") +
  geom_vline(aes(xintercept = as.Date("2018-07-06")),
             linetype = 4, colour = "blue") +
  geom_text(aes(x = as.Date("2018-07-06"), 
                label = "Resign", y = 10), 
            colour = "blue") +
  geom_vline(aes(xintercept = as.Date("2019-01-15")),
             linetype = 4, colour = "green") +
  geom_text(aes(x = as.Date("2019-01-15"), 
                label = "Lost", y = 10), 
            colour = "green") +
  geom_vline(aes(xintercept = as.Date("2017-12-08")),
             linetype = 4, colour = "orange") +
  geom_text(aes(x = as.Date("2017-12-08"), 
                label = "Backstop", y = 10),
            colour = "orange") +
  geom_vline(aes(xintercept = as.Date("2017-06-08")),
             linetype = 4, colour = "violet") +
  geom_text(aes(x = as.Date("2017-06-08"), 
                label = "Election", y = 10), 
            colour = "violet") +
  ggtitle('UK Imports from Major Runners (Non-EU): 2015 - 2019')

# --- UK EXPORT ANALYSIS --- #

# load uk export data
uk.export <- read.csv(ukexport.data.path)

# drop unwanted columns and rows
uk.export <- uk.export[-c(1:3), -c(1, 3:206)]

# filter for EU member states
uk.export.eu <- uk.export[c(1, 3:29),]
# filter for NON-EU member states
uk.export.noneu <- uk.export[-c(2:32),]

# transpose the data.frame
uk.export.eu <- t(uk.export.eu)
uk.export.noneu <- t(uk.export.noneu)

# restructuring the data.frame
colnames(uk.export.eu) <- c('Date', uk.export.eu[1, 2:ncol(uk.export.eu)])
colnames(uk.export.noneu) <- c('Date', uk.export.noneu[1, 2:ncol(uk.export.noneu)])
uk.export.eu <- uk.export.eu[-1, ]
uk.export.noneu <- uk.export.noneu[-1, ]

# transform to data.frame
uk.export.eu <- as.data.frame(uk.export.eu)
uk.export.noneu <- as.data.frame(uk.export.noneu)

# format date type
uk.export.eu$Date <- AsDate(uk.export.eu$Date)
uk.export.noneu$Date <- AsDate(uk.export.noneu$Date)

# check which indices are factors and convert to numeric type
indx <- sapply(uk.export.eu, is.factor)
uk.export.eu[indx] <- lapply(uk.export.eu[indx], function(x) as.numeric(as.character(x)))
indx <- sapply(uk.export.noneu, is.factor)
uk.export.noneu[indx] <- lapply(uk.export.noneu[indx], function(x) as.numeric(as.character(x)))

# calculate total import value per country
total.export.eu <- as.data.frame(t(uk.export.eu %>%
                                     summarize_if(is.numeric, sum, na.rm=TRUE)))
total.export.noneu <- as.data.frame(t(uk.export.noneu %>%
                                        summarize_if(is.numeric, sum, na.rm=TRUE)))

# move rownames as a column
total.export.eu$Country <- rownames(total.export.eu)
total.export.noneu$Country <- rownames(total.export.noneu)
rownames(total.export.eu) <- NULL
rownames(total.export.noneu) <- NULL

# visualise total imports by country
ggplot(data = total.export.eu, aes(x = reorder(Country, -V1), y = V1, fill = Country)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Country") + ylab("Total") +
  ggtitle("UK Total Exports by Country (EU): 01/2015-01/2019") +
  theme(plot.title = element_text(hjust = 0.5))

# run statistics of total import
basicStats(total.export.eu$V1)
basicStats(total.export.noneu$V1)

# drop unwanted rows for non-eu
total.export.noneu <- total.export.noneu[-c(208:214), ]

# drop less than mean
export.top.country <- subset(total.export.eu, total.export.eu$V1 > 
                               basicStats(total.export.eu$V1)[
                                 which(rownames(basicStats(total.export.eu$V1)) == 'Mean'),],
                             select = Country)

# extract top 5 non-eu countries
export.top5.noneu <- head(total.export.noneu[with(total.export.noneu, order(-V1)), ], 5)

# convert data.frame from wide to long
uk.export.eu.melt <- melt(data = uk.export.eu, 
                          id.vars = "Date", 
                          measure.vars = c(2:ncol(uk.export.eu)),
                          value.name = 'Value',
                          variable.name = 'Country')

uk.export.noneu.melt <- melt(data = uk.export.noneu, 
                             id.vars = "Date", 
                             measure.vars = c(2:ncol(uk.export.noneu)),
                             value.name = 'Value',
                             variable.name = 'Country')

# keep only the top countries
uk.export.eu.melt <- uk.export.eu.melt[uk.export.eu.melt$Country 
                                       %in% export.top.country$Country, ]

uk.export.noneu.melt <- uk.export.noneu.melt[uk.export.noneu.melt$Country 
                                             %in% export.top5.noneu$Country, ]

ggplot(uk.export.eu.melt, aes(x = Date, y = Value)) + 
  geom_line(aes(color = Country), size = 1) +
  theme_minimal() +
  geom_vline(aes(xintercept = as.Date("2016-06-23")),
             linetype = 4, colour = "red") +
  geom_text(aes(x = as.Date("2016-06-23"), 
                label = "Brexit Voting", y = 10), 
            colour = "red") +
  geom_vline(aes(xintercept = as.Date("2018-07-06")),
             linetype = 4, colour = "blue") +
  geom_text(aes(x = as.Date("2018-07-06"), 
                label = "Resign", y = 10), 
            colour = "blue") +
  geom_vline(aes(xintercept = as.Date("2019-01-15")),
             linetype = 4, colour = "green") +
  geom_text(aes(x = as.Date("2019-01-15"), 
                label = "Lost", y = 10), 
            colour = "green") +
  geom_vline(aes(xintercept = as.Date("2017-12-08")),
             linetype = 4, colour = "orange") +
  geom_text(aes(x = as.Date("2017-12-08"), 
                label = "Backstop", y = 10),
            colour = "orange") +
  geom_vline(aes(xintercept = as.Date("2017-06-08")),
             linetype = 4, colour = "violet") +
  geom_text(aes(x = as.Date("2017-06-08"), 
                label = "Election", y = 10), 
            colour = "violet") +
  ggtitle('UK Exports from Major Runners (EU): 2015 - 2019')

ggplot(uk.export.noneu.melt, aes(x = Date, y = Value)) + 
  geom_line(aes(color = Country), size = 1) +
  theme_minimal() +
  geom_vline(aes(xintercept = as.Date("2016-06-23")),
             linetype = 4, colour = "red") +
  geom_text(aes(x = as.Date("2016-06-23"), 
                label = "Brexit Voting", y = 10), 
            colour = "red") +
  geom_vline(aes(xintercept = as.Date("2018-07-06")),
             linetype = 4, colour = "blue") +
  geom_text(aes(x = as.Date("2018-07-06"), 
                label = "Resign", y = 10), 
            colour = "blue") +
  geom_vline(aes(xintercept = as.Date("2019-01-15")),
             linetype = 4, colour = "green") +
  geom_text(aes(x = as.Date("2019-01-15"), 
                label = "Lost", y = 10), 
            colour = "green") +
  geom_vline(aes(xintercept = as.Date("2017-12-08")),
             linetype = 4, colour = "orange") +
  geom_text(aes(x = as.Date("2017-12-08"), 
                label = "Backstop", y = 10),
            colour = "orange") +
  geom_vline(aes(xintercept = as.Date("2017-06-08")),
             linetype = 4, colour = "violet") +
  geom_text(aes(x = as.Date("2017-06-08"), 
                label = "Election", y = 10), 
            colour = "violet") +
  ggtitle('UK Exports from Major Runners (Non-EU): 2015 - 2019')

# --- UK INFLATION ANALYSIS --- #

# read in consumer price indices data
cpi <- read.csv(cpi.data.path)

# rename date column
rename.cols(cpi, 'Title', 'Date')

# transform to date type
cpi$Date <- AsDate(cpi$Date)

# line plot of different indices
draw.cpi(cpi, 'ALL.ITEMS', 'UK Consumer Price Index - All Items')
draw.cpi(cpi, 'INSURANCE', 'UK Consumer Price Index - Insurance')
draw.cpi(cpi, 'FOOD.PRODUCTS', 'UK Consumer Price Index - Food Products')
draw.cpi(cpi, 'ELECTRICITY', 'UK Consumer Price Index - Electricity, Gas, and other Fuels')
draw.cpi(cpi, 'RENTS', 'UK Consumer Price Index - Housing Rents')
draw.cpi(cpi, 'TRANSPORT', 'UK Consumer Price Index - Transport')

# calculate inflation from 2016-06 (start of the BREXIT vote) to 2019-03 (PRESENT)
calc.inflation(cpi, '2016-06', '2019-03', 'ALL.ITEMS')     # 6.36% inflation
calc.inflation(cpi, '2016-06', '2019-03', 'INSURANCE')     # 4.13% inflation
calc.inflation(cpi, '2016-06', '2019-03', 'FOOD.PRODUCTS') # 2.48% inflation
calc.inflation(cpi, '2016-06', '2019-03', 'ELECTRICITY')   # 9.00% inflation
calc.inflation(cpi, '2016-06', '2019-03', 'RENTS')         # 2.17% inflation
calc.inflation(cpi, '2016-06', '2019-03', 'TRANSPORT')     # 9.32% inflation

# --- CAUSAL IMPACT ANALYSIS --- #

# fill missing days using value of the previous day
fill.na <- zoo(order.by = seq.Date(head(index(gbp.eur.close), 1), 
                                   tail(index(gbp.eur.close), 1), 
                                   by = "days"))
gbp.eur.close <- na.locf(merge(gbp.eur.close, fill.na))

# set pre and post event date range
pre.period <- as.Date(c("2017-12-01", "2017-12-07"))
post.period <- as.Date(c("2017-12-09", "2017-12-16"))

# calculate the casual impact by Bayesian Inference modelling
impact <- CausalImpact(gbp.eur.close, 
                       pre.period, 
                       post.period, 
                       model.args = list(niter = 5000, nseasons = 7,  season.duration = 1))

# casual impact plot and summary
plot(impact)

summary(impact)
summary(impact, "report")

# --- CRYPTO ANALYSIS --- #

# get the current top 10 cryptocurrencies
best.crypto <- crypto_prices(limit = 10)

# plot market cap per crypto
p <- ggplot(data=arrange(best.crypto[1:10, ], market_cap_usd), 
       aes(x = reorder(name, -market_cap_usd), y = market_cap_usd)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Currency Name") +
  ylab("Market Cap in USD") +
  ggtitle("Current Top 10 Cryptocurrencies")
p

# scrape market data for the current top 5 crytpocurrencies
data <- crypto_history(coin = c("Bitcoin", "Ethereum", 
                                "Ripple", "Litecoin", 
                                "Bitcoin Cash"), 
                       start_date = "20140101",
                       end_date = "20190612")

# plot closing price of current top 5 crypto
p <- ggplot(data, aes(x=date, y=close, group=name)) +
  geom_line(aes(color=name))+
  geom_point(aes(color=name)) + 
  labs(title="Closing Price of Top 5 Crypto Currencies: 2014-2019", 
       x = "Date", 
       y = "Closing Price in USD") +
  geom_vline(aes(xintercept = as.Date("2016-06-23")),
             linetype = 4, colour = "red") +
  geom_text(aes(x = as.Date("2016-06-23"), 
                label = "Brexit Voting", y = 15000), 
            colour = "red") +
  geom_vline(aes(xintercept = as.Date("2018-07-06")),
             linetype = 4, colour = "blue") +
  geom_text(aes(x = as.Date("2018-07-06"), 
                label = "Resign", y = 15000), 
            colour = "blue") +
  geom_vline(aes(xintercept = as.Date("2019-01-15")),
             linetype = 4, colour = "green") +
  geom_text(aes(x = as.Date("2019-01-15"), 
                label = "Lost", y = 20000), 
            colour = "green") +
  geom_vline(aes(xintercept = as.Date("2017-12-08")),
             linetype = 4, colour = "orange") +
  geom_text(aes(x = as.Date("2017-12-08"), 
                label = "Backstop", y = 15000),
            colour = "orange") +
  geom_vline(aes(xintercept = as.Date("2017-06-08")),
             linetype = 4, colour = "violet") +
  geom_text(aes(x = as.Date("2017-06-08"), 
                label = "Election", y = 15000), 
            colour = "violet")
p

# scrape bitcoin data
btc <- crypto_history(coin = "Bitcoin", 
                      start_date = "20140101",
                      end_date = "20190612")
# drop unwanted columns
btc <- btc[, c("date", "close"), drop = FALSE]

# transform to date type
btc$date <- AsDate(btc$date)

# convert to xts
btc <- xts(btc[,-1], order.by=btc$date)

# interactive candlestick chart for bitcoin
p <- btc %>%
  plot_ly(x = ~date, type="candlestick",
          open = ~open, close = ~close,
          high = ~high, low = ~low) %>%
  layout(title = "Candlestick Chart of BTC: 2014-2019")
p

data.xts <- crypto_xts(data, "week")

df <- data.frame("date" = as.Date(index(data.xts)), 
                 "name" = data.xts$name, 
                 "close" = as.numeric(data.xts$close))
rownames(df) <- NULL


p <- ggplot(data=df[which(df$date > "2018-11-30"),],
            aes(x=date, y=close, colour=name)) + 
  geom_line() + 
  theme(axis.title.x = element_blank()) + 
  ylab("") + xlab("")+ 
  ggtitle("Top 5 Cryptocurrency log closing price over 2019") + 
  scale_x_date(date_breaks = "months", 
               date_labels = "%b%y") +
  scale_y_log10() +
  annotation_logticks()

p

# --- REMOVE VARIABLES & GARBAGE COLLECTOR --- #
rm(list = ls())
gc()
