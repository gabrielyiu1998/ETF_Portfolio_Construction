library("quantmod")
library("xts")
library("zoo")
library("rmgarch")
library("MASS")
library("ggplot2")

symbols <- c("VCN.TO", "VEE.TO", "VFV.TO", "VVL.TO", "VAB.TO", "VBU.TO", "XSU.TO")

start = "2016-06-22"
test = "2020-01-01"
end = "2021-02-01"

prices <- xts()

for (sym in symbols){
  prices <- cbind(prices,
                  Ad(getSymbols.yahoo(sym, from = start, to = end,auto.assign=FALSE)))
}

colnames(prices) <- symbols
indexClass(prices) <- "Date"

logreturns <- diff(log(prices))[-1]

save.image("price_data.RData")