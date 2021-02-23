library("quantmod")
library("xts")
library("zoo")
library("rmgarch")
library("MASS")
library("ggplot2")

load("price_data.RData")

plot(logreturns, legend.loc = "topleft")

# specify i.i.d. model for the univariate time series
ugarch_spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                          variance.model = list(model = "sGARCH", garchOrder = c(1,1)))

# specify DCC model
dcc_spec <- dccspec(uspec = multispec(replicate(ugarch_spec, n = 7)),
                    VAR = TRUE, lag = 0, model = "DCC", dccOrder = c(1,1))

# estimate model
garchdcc_fit <- dccfit(dcc_spec, data = logreturns[paste("/", test, sep = "")],
                       solver = "nlminb")
garchdcc_fit

# forecast model
dcc_spec_frcst <- dccspec(uspec = multispec(replicate(ugarch_spec, n = 7)),
                          VAR = TRUE, lag = 0, model = "DCC", dccOrder = c(1,1))

garchdcc_frcst <- dccforecast(garchdcc_fit, data = logreturns, n.ahead = 272,
                              solver = "nlminb")

unicoefs <- function(symbol, mdl){
  indx <- paste(paste("[",symbol,"].", sep = ""),
                c("mu" , "omega", "alpha1", "beta1"), sep = "")
  coefs <- coef(mdl)[indx]
  names(coefs) <- c("mu" , "omega", "alpha1", "beta1")
  return(coefs)
}

spec_frcst <- ugarch_spec
setfixed(spec_frcst) <- as.list(unicoefs("VCN.TO", garchdcc_fit))
traind <- logreturns[paste("/",test,sep = ""), "VCN.TO"]
testd <- logreturns[paste(test, "/",sep = ""), "VCN.TO"]
u_frcst <- ugarchfilter(spec_frcst, data = testd)
u_frcst

testd$Retsq <- sqrt((testd$VCN.TO - mean(traind$VCN.TO) ) ^ 2)
testd$Pred <- u_frcst@filter$sigma
logreturnsdf <- data.frame(date = index(testd), coredata(testd))

ggplot(data = logreturnsdf, aes(x = date)) +
  geom_line(aes_string(y = "Retsq"), color = "steelblue") +
  geom_line(aes_string(y = "Pred"), color = "darkred") +
  labs(x = "Date", y = "Volatility", color = c("Actuals", "Predicted")) +
  theme_bw()

# extract time-varying covariance and correlation matrix
dcc_cor <- rcor(garchdcc_fit)
dim(dcc_cor)

frcst <- dccforecast(garchdcc_fit, n.ahead = 272)
covs <- rcov(frcst)
vols <- rcor(frcst)
one_yr_cov <- covs$`2019-12-31`
one_yr_cor <- vols$`2019-12-31`

logreturnsdf <- data.frame(Date = index(logreturns[paste(test, "/", end, sep = "")]))

for (i in 1:7){
  logreturnsdf[,colnames(prices)[i]] = one_yr_cov[i, i,]
}

ggplot(data = logreturnsdf, aes(x = Date)) +
  geom_line(aes(y = sqrt(VCN.TO), color = "VCN.TO")) +
  geom_line(aes(y = sqrt(VAB.TO), color = "VAB.TO")) +
  labs(x = "Date", y = "Volatility") +
  theme_bw()
