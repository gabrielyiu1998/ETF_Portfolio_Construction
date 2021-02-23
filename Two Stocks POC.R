library("quantmod")
library("xts")
library("zoo")
library("rmgarch")
library("MASS")
library("ggplot2")

symbols <- c("VCN.TO", "VAB.TO")

start = "2015-01-01"
end = "2021-01-01"

prices <- xts()

for (sym in symbols){
  prices <- cbind(prices,
                  Ad(getSymbols.yahoo(sym, from = start, to = end,auto.assign=FALSE)))
}

colnames(prices) <- symbols
indexClass(prices) <- "Date"

logreturns <- diff(log(prices))[-1]

plot(logreturns, legend.loc = "topleft")
logreturns_train <- logreturns["/2020-01-01"]
logreturns_test <- logreturns["2020-01-01/2021-01-01"]

# specify i.i.d. model for the univariate time series
ugarch_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE), 
                          variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                          distribution.model = "std")

VCN_garch_fit <- ugarchfit(ugarch_spec, logreturns_train$VCN.TO)
spec_frcst <- getspec(VCN_garch_fit)
setfixed(spec_frcst) <- as.list(coef(VCN_garch_fit))

VCN_frcst <- ugarchfit(spec_frcst, data = logreturns_test$VCN.TO)

logreturns_test$VCN.TO_pred <- VCN_frcst@filter$sigma
logreturns_test$VCN.TO_dist <- sqrt((logreturns_test$VCN.TO - mean(logreturns$VCN.TO))^2)
logreturnsdf <- data.frame(date = index(logreturns_test), coredata(logreturns_test))
  
ggplot(data = logreturnsdf, aes(x = date)) +
  geom_line(aes(y = Retsq, color = "Actual")) +
  geom_line(aes(y = Pred, color = "Predicted")) +
  labs(x = "Date", y = "Volatility", title = "VCN.TO") +
  scale_color_manual(values = c("steelblue", "darkred")) +
  theme_bw()

# specify DCC model
dcc_spec <- dccspec(uspec = multispec(replicate(ugarch_spec, n = 2)),
                    VAR = TRUE, lag = 3,
                    model = "DCC", dccOrder = c(1,1))

# estimate model
garchdcc_fit <- dccfit(dcc_spec, data = na.omit(logreturns), solver = "nlminb")
garchdcc_fit

# extract time-varying covariance and correlation matrix
dcc_cor <- rcor(garchdcc_fit)
dim(dcc_cor)

#plot
corr_t <- xts(dcc_cor[1, 2, ], order.by = index(logreturns))
colnames(corr_t) <- c("VCN vs VAB")
plot(corr_t, main = "Time-varying correlations", legend.loc = "left")

frcst <- dccforecast(garchdcc_fit, n.ahead = 253)
covs <- rcov(frcst)
vols <- rcor(frcst)
one_yr_cov <- covs$`2021-01-29`
one_yr_cor <- vols$`2021-01-29`

mus <- colMeans(na.omit(logreturns))

x <- rep(0, 7)

for (i in 1:253){
  x <- x + mvrnorm(1, mus, one_yr_cov[,,i])
}
