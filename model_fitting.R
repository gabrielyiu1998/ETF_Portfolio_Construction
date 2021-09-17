library("quantmod")
library("xts")
library("zoo")
library("rmgarch")
library("MASS")
library("ggplot2")
library("mvtnorm")
library("QRM")

load("price_data.RData")

plot(logreturns, legend.loc = "topleft")

# ==== Fit Garch ====

ugarch_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE), 
                          variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                          distribution.model = "std")

dcc_spec <- dccspec(uspec = multispec(replicate(ugarch_spec, n = length(symbols))),
                    VAR = TRUE, lag = 0, model = "DCC", dccOrder = c(1,1),
                    distribution = c("mvt"))

garchdcc_fit <- dccfit(dcc_spec, data = logreturns, out.sample = testdays)

# ==== Forecast ====
unicoefs <- function(symbol, mdl){
  indx <- paste(paste("[",symbol,"].", sep = ""),
                c("mu" , "omega", "alpha1", "beta1"), sep = "")
  coefs <- coef(mdl)[indx]
  names(coefs) <- c("mu" , "omega", "alpha1", "beta1")
  return(coefs)
}

external_returns <- logreturns[lastrow,]
external_returns <- as.matrix(external_returns)

frcst  <- dccforecast(garchdcc_fit, n.roll = testdays, n.ahead = 1)

get_sig <- function(stock_ind, t){
  return(frcst@mforecast$H[[t]][stock_ind, stock_ind, 1])
}

testdata <- data.frame(Date = index(logreturns[paste(test, "/" , sep = "")]))
testdata <- cbind(testdata, data.frame(logreturns[paste(test, "/" , sep = "")]))

for (i in 1:length(symbols)){
  testdata <- cbind(testdata, data.frame(sqrt(sapply(1:nrow(testdata),
                                                     get_sig, stock_ind = i))))
  colnames(testdata)[ncol(testdata)] = paste(symbols[i], ".vol",sep="")
}

if (is.na(daily_mus)){
  mus <- frcst@model$mu[1,]
} else {
  mus <- daily_mus
}
names(mus) <- symbols

# ==== Simulations ====
set.seed(7829)
raw_frcst <- dccforecast(garchdcc_fit, n.ahead = 126)
var_cov <- raw_frcst@mforecast$H[[1]][,,126] * 252
ann_mu <- mus * 252
sharpe_ratio <- function(weights){
  return((weights %*% ann_mu - rf)/sqrt(t(weights) %*% (var_cov %*% weights)))
}

sol <- optim(rep(1/length(symbols),length(symbols)), sharpe_ratio,
             lower = rep(0, length(symbols)), method = "L-BFGS-B",
             control = list(fnscale = -1))
sol <- sol$par / sum(sol$par)

all_wts <- matrix(nrow = num_port, ncol= length(symbols))
port_returns <- vector('numeric', length = num_port)
port_risk <- vector('numeric', length = num_port)
sr <- vector('numeric', length = num_port)
all_wts[1,] <- sol
port_returns[1] <- sum(sol * ann_mu)
port_risk[1] <- sqrt(t(sol) %*% (var_cov  %*% sol))
sr[1] <- sharpe_ratio(sol)

for (i in 2:num_port) {
  
  wts <- runif(length(symbols))
  wts <- wts/sum(wts)
  
  all_wts[i,1:length(symbols)] <- wts
  
  port_returns[i] <- sum(wts * ann_mu)
  port_risk[i] <- sqrt(t(wts) %*% (var_cov  %*% wts))
  sr[i] <- sharpe_ratio(wts)
  
}

portfolio_values <- data.frame(cbind(all_wts, port_returns, port_risk, sr))
colnames(portfolio_values) <- c(paste("Wt.", symbols,sep=""),
                                "Returns", "Risk", "SharpeRatio")

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

print(max_sr)

opt_wts <- as.vector(unlist(
  portfolio_values[which.max(portfolio_values$SharpeRatio),
                   paste("Wt.", symbols,sep="")]))

rts <- 1:nsim
df <- fit.mst(logreturns, method = "BFGS")$df

for (i in 1:nsim){
  rts[i] <- (rmvt(1, var_cov, df) + ann_mu) %*% opt_wts
}

VaR <- function(wt, initial = 10000, p = 0.005, ti = 1){
  return(initial * (1 - wt) * (1 + rf) ^ ti + wt * initial * quantile(exp(rts), p))
}

TVaR <- function(wt, initial = 10000, p = 0.005, ti = 1){
  return(
    initial * (1 - wt) * (1 + rf) ^ ti +
      wt * initial * (exp(mean(rts[which(rts <= quantile(rts, p))]))))
}

ER <- function(wt, initial = 10000, ti = 1){
  return(initial * ((1 - wt) * (1 + rf) ^ ti + wt * exp(opt_wts %*% mus * 252 * ti)))
}

save.image("iShares Manual Returns.Rdata")
