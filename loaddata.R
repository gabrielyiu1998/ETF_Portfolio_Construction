library("quantmod")
library("xts")
library("zoo")
library("rmgarch")
library("MASS")
library("ggplot2")

symbols <- c("XIC.TO", "XSP.TO", "XCV.TO", "XBB.TO", "XBM.TO", "XGD.TO")

start = "1990-06-01"
test = "2020-01-01"
end = Sys.Date()

rf <- 0.0175
num_port <- 10000 # Simulated Portfolios
nsim <- 10000 # Simulated Time Sereies Paths
prices <- xts()
daily_mus <- c(0.07, 0.1, 0.07, 0.01, 0.1, 0.05)/252

compare <- c("XSP.TO", "XGD.TO", "Port")

for (sym in symbols){
  prices <- cbind(prices,
                  Ad(getSymbols.yahoo(sym, from = start, to = end,auto.assign=FALSE)))
}

prices <- na.omit(prices)

colnames(prices) <- symbols
indexClass(prices) <- "Date"

logreturns <- diff(log(prices))[-1]

lastrow <- nrow(logreturns[paste("/", test, sep = "")])
testdays <- nrow(logreturns[paste(test, "/" , sep = "")])

save.image("price_data.RData")