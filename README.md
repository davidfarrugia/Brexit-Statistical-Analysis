# Understanding the effects of Brexit: A statistical approach

This project uses R to carry out various statistical tests and analysis to discover the extent of Brexit's correlation and causation to different aspects such as the financial markets. This project was built as a practice case-study to help get more comfortable with the R language and also with carrying out statistical tests in practice.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

Below are the libraries used throughout this project

```
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
```

### Datasets

**GBP/EUR Exchange Daily Historic Data** - pulled from quantmod R package (source: Yahoo Finance)
**GBP Effective Exchange Index** - UK National Statistics Office
**UK Trading Data** - UK National Statistics Office
**UK Consumer Price Indices** - UK National Statistics Office
**Crypto Data** - pulled using crypto R package

## Authors

* **David Farrugia**
