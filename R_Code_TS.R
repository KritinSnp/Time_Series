## Load the time series data
load("/Users/dk/Documents/GitHub/Time_Series/pond.RData")
X

## Set the trend and seasonal
par(mfrow = c(1,1))
plot(X, col = 'blue')
tt = 1:length(X)

# Trend equation
par(mfrow = c(2,2))
fit1 = lm(X ~ tt + tt^2)
summary(fit1)

trend1 = fitted(fit1)
trend1 = ts(trend1, start = start(X), end = end(X), frequency = frequency(X))
resid1 = ts(residuals(fit1), start = start(X), end = end(X), frequency = frequency(X))
plot(trend1, ylim = c(0,4), ylab = "Trend", col = 'blue')
plot(resid1, ylab = "Residuals1"); lines(trend1, col='blue')
fit1$coefficients
dev.off()

# Sensonal
n = length(X)
jan = as.numeric((1:n %% 12) == 1)
feb = as.numeric((1:n %% 12) == 2)
mar = as.numeric((1:n %% 12) == 3)
apr = as.numeric((1:n %% 12) == 4)
may = as.numeric((1:n %% 12) == 5)
jun = as.numeric((1:n %% 12) == 6)
jul = as.numeric((1:n %% 12) == 7)
aug = as.numeric((1:n %% 12) == 8)
sep = as.numeric((1:n %% 12) == 9)
oct = as.numeric((1:n %% 12) == 10)
nov = as.numeric((1:n %% 12) == 11)
dec = as.numeric((1:n %% 12) == 0)

fit2 = lm(resid1 ~ 0 + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec)
seasonal = ts(fitted(fit2), start = start(X), end = end(X), frequency = frequency(X))
Y = X - trend1 - seasonal
plot(seasonal, ylab = 'Seasonal', type = 'l', col = 'blue')
plot(Y, ylab = 'Residual2', type = 'l')
fit2$coefficients
## Coefficients of each month
# jan        feb        mar        apr        
# -1.3954436 -0.1478670  0.3502151 -0.7913855
# may        jun        jul        aug        
# -0.9047272 -1.1336213 -2.1532813 -2.1083078
# sep        oct        nov        dec 
# 0.8767831   2.3493167  2.4438542  2.6144646 


## Show the data in time and acf plot of each step (raw, detrend, detrend and deseasonal)
par(mfrow = c(3,2))
plot(X, col = 'blue3')
acf(X)

plot(X - trend1, col = 'blue3')
acf(X - trend1, main = 'Residual1')

plot(X - trend1 - seasonal, col = 'blue3')
acf(X - trend1 - seasonal)


## Check the residuals with acf and pacf
par(mfrow = c(3,1))
plot(X - trend1 - seasonal)
acf(X - trend1 - seasonal)
pacf(X - trend1 - seasonal, col = 'red')


## Fit AR1, AR2, AR3
par(mfcol=c(3,2))
ar1 = ar(Y, method = 'yule-walker', aic = FALSE, order = 1)
ar2 = ar(Y, method = 'yule-walker', aic = FALSE, order = 2)
ar3 = ar(Y, method = 'yule-walker', aic = FALSE, order = 3)

plot(ar1$resid, ylab = "Xt", main = paste("AR(1)"))
plot(ar2$resid, ylab = "Xt", main = paste("AR(2)"))
plot(ar3$resid, ylab = "Xt", main = paste("AR(3)"))

acf(ar1$resid, na.action=na.omit, main = "")
acf(ar2$resid, na.action=na.omit, main = "")
acf(ar3$resid, na.action=na.omit, main = "")

## Check the parameter value of three models

ar1
# (Y[2:600] %*% Y[1:599])/(Y[1:599] %*% Y[1:599])
# co1 = c(0.9025)

ar2
# co2 = c(0.6708, 0.2568)

ar3
# co3 = c(0.6634, 0.2374, 0.0288)


## Check the residuals of each model
mean(ar1$resid, na.rm=TRUE)
var(ar1$resid, na.rm=TRUE)
mean(ar2$resid, na.rm=TRUE)
var(ar2$resid, na.rm=TRUE)
mean(ar3$resid, na.rm=TRUE)
var(ar3$resid, na.rm=TRUE)


## Plot periodograms to see the trend and seasonal in frequency domain
par(mfcol=c(3,1))

dft.X = fft(X)/sqrt(n)
I.X = Mod(dft.X)^2
plot(x=(0:(n/2))/n, y = I.X[1:(n/2+1)], type="h", log = 'y',  
     xlab=expression(f[j]), ylab=expression(I(f[j])), 
     main="Periodogram of X")

# periodogram(X, opt = 1)
dft.Y = fft(Y)/sqrt(n)
I.Y = Mod(dft.Y)^2
plot(x=(0:(n/2))/n, y = I.Y[1:(n/2+1)], type="h", log = 'y',   
     xlab=expression(f[j]), ylab=expression(I(f[j])), 
     main="Periodogram of Y")

Z[is.na(Z)] <- 0

dft.Z = fft(Z)/sqrt(n)
I.Z = Mod(dft.Z)^2
plot(x=(0:(n/2))/n, y = I.Z[1:(n/2+1)], type="h", log = 'y',   
     xlab=expression(f[j]), ylab=expression(I(f[j])), 
     main="Periodogram of Z")


### ------------------------------------ End ------------------------------------ ###

### This part is for visualization in more detial
# par(mfrow = c(1,1))
# plot(X, ylab = "Feet", col = 'blue', main = 'Raw pond data: X')

# plot(trend1, ylim = c(0,4), ylab = "Feet", col = 'red', main = 'Linear trend')
# plot(resid1, ylab = "Feet", col = 'blue', main = 'Residual after removing trend')

# plot(seasonal, ylab = "Feet", col = 'red', main = 'Seasonal effect', xlim = c(1995,2000))
# plot(Y, ylab = "Feet", col = 'blue', main = 'Residual after removing trend and seasonal effect: Y')
# acf(Y, main = 'Y')
# pacf(Y, main = 'Y')

# plot(ar1$resid, ylab = "Feet", col = 'blue', main = 'AR(1)')
# acf(ar1$resid, na.action=na.omit, main = 'AR(1)')

# plot(ar2$resid, ylab = "Feet", col = 'blue', main = 'AR(2)')
# acf(ar2$resid, na.action=na.omit, main = 'AR(2)')

# plot(ar3$resid, ylab = "Feet", col = 'blue', main = 'AR(3)')
# acf(ar3$resid, na.action=na.omit, main = 'AR(3)')

# plot(x=(0:(n/2))/n, y = I.X[1:(n/2+1)], type="h",   
#      xlab=expression(f[j]), ylab=expression(I(f[j])), 
#      main="periodogram of X")
# # dev.off()
# plot(x=(0:(n/2))/n, y = I.Y[1:(n/2+1)], type="h",   
#      xlab=expression(f[j]), ylab=expression(I(f[j])), 
#      main="periodogram of Y")

# plot(x=(0:(n/2))/n, y = I.Z[1:(n/2+1)], type="h",   
#      xlab=expression(f[j]), ylab=expression(I(f[j])), 
#      main="periodogram of Z")

# plot(y=I.X[-1], x=(1:(n-1))/n, type="h", xlab=expression(f[j]), ylab=expression(I(f[j])))
# plot(y=I.Y[-1], x=(1:(n-1))/n, type="h", xlab=expression(f[j]), ylab=expression(I(f[j])))
# plot(y=I.Z[-1], x=(1:(n-1))/n, type="h", xlab=expression(f[j]), ylab=expression(I(f[j])))


# par(mfrow = c(1,1))
# plot(trend1 + seasonal + ar2$resid + ar2_sim)
# mean(na.omit(Z))

# par(mfrow = c(3,1))
# plot(X, main = 'Raw pond data: X', ylab = "Feet")
# plot(trend1, main = 'Trend', ylab = "Feet")
# plot(seasonal, main = 'Seasonal effect', ylab = "Feet")
# plot(Y - Z, main = 'AR(2) (Y-Z)', ylab = "Feet")
# plot(Z, main = 'Z (White noise)', ylab = "Feet")
# dev.off()

# max(X)
# min(X)
# mean(X)
# var(X)
