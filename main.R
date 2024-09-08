library(tseries)
library(vars)

############
# Data preparation
############

data = read.csv("2022-02.csv")[-c((1:2),(255:256)),]
row.names(data) <- c(1:nrow(data))
subset = data[, c(
  "sasdate",
  "GDPC1",
  "CUMFNS",
  "UNRATESTx",
  "CPIAUCSL",
  "FEDFUNDS",
  "M1REAL",
  "S.P.500"
)]

growth = function(feature) {
  g = (feature[2:length(feature)] - feature
       [1:(length(feature) - 1)]) / feature[1:(length(feature) - 1)] * 100
  return(c(NA, g))
}

g_GDPC1 = growth(subset$GDPC1)
g_CPIAUCSL = growth(subset$CPIAUCSL)
g_M1REAL = growth(subset$M1REAL)
g_S.P.500 = growth(subset$S.P.500)

subset = cbind(subset[c("sasdate", "CUMFNS", "UNRATESTx", "FEDFUNDS")], g_GDPC1, g_CPIAUCSL, g_M1REAL, g_S.P.500)

min_date = c(subset$sasdate[which.min(subset$g_GDPC1)],
             subset$sasdate[which.min(subset$g_CPIAUCSL)],
             subset$sasdate[which.min(subset$g_M1REAL)],
             subset$sasdate[which.min(subset$g_S.P.500)],
             subset$sasdate[which.min(subset$CUMFNS)],
             subset$sasdate[which.min(subset$UNRATESTx)],
             subset$sasdate[which.min(subset$FEDFUNDS)])
max_date = c(subset$sasdate[which.max(subset$g_GDPC1)],
             subset$sasdate[which.max(subset$g_CPIAUCSL)],
             subset$sasdate[which.max(subset$g_M1REAL)],
             subset$sasdate[which.max(subset$g_S.P.500)],
             subset$sasdate[which.max(subset$CUMFNS)],
             subset$sasdate[which.max(subset$UNRATESTx)],
             subset$sasdate[which.max(subset$FEDFUNDS)])

s = round(rbind(c(fivenum(subset$g_GDPC1), mean(na.omit(subset$g_GDPC1))),
          c(fivenum(subset$g_CPIAUCSL), mean(na.omit(subset$g_CPIAUCSL))),
          c(fivenum(subset$g_M1REAL), mean(na.omit(subset$g_M1REAL))),
          c(fivenum(subset$g_S.P.500), mean(na.omit(subset$g_S.P.500))),
          c(fivenum(subset$CUMFNS), mean(subset$CUMFNS)),
          c(fivenum(subset$UNRATESTx), mean(subset$UNRATESTx)),
          c(fivenum(subset$FEDFUNDS), mean(subset$FEDFUNDS))), 3)

s = cbind(s[,1], min_date, s[,5], max_date, s[,6])
colnames(s) = c("Min.", "Min. date", "Max.", "Max. date" , "Mean")
rownames(s) = c("%GDPC1", "%CPIAUCSL", "%M1REAL", "%S&P 500", "CUMFNS", "UNRATESTx", "FEDFUNDS")


subset = cbind(subset[c("CUMFNS", "UNRATESTx", "FEDFUNDS")], g_GDPC1, g_CPIAUCSL, g_M1REAL, g_S.P.500)


############
# task a 
############

# plot the 7 variables over 1959Q2–2021Q4
par(mfrow = c(4, 2))
par(mar = c(4,4,2,1))
plot(ts(subset$g_GDPC1[-1], frequency = 4, start = c(1959, 2)), 
     ylab = "%",
     xlab = "",
     main = "(a) %GDPC1")
title(xlab = "Time", line = 2.5)  
plot(ts(subset$g_CPIAUCSL[-1], frequency = 4, start = c(1959, 2)),
     ylab = "%",
     xlab = "",
     main = "(b) %CPIAUCSL")
title(xlab = "Time", line = 2.5)
plot(ts(subset$g_M1REAL[-1], frequency = 4, start = c(1959, 2)),
     ylab = "%",
     xlab = "",
     main = "(c) %M1REAL")
title(xlab = "Time", line = 2.5)
# lines(x = c(1959, 2022), y = c(10,10), lty=3)
plot(ts(subset$g_M1REAL[-1], frequency = 4, start = c(1959, 2)),
     ylim = c(-5,10),
     ylab = "%",
     xlab = "",
     main = "(d) %M1REAL (Enlarge y-axis)")
title(xlab = "Time", line = 2.5)
plot(ts(subset$g_S.P.500[-1], frequency = 4, start = c(1959, 2)),
     ylab = "%",
     xlab = "",
     main = "(e) %S&P 500")
title(xlab = "Time", line = 2.5)
plot(ts(data$CUMFNS[-1], frequency = 4, start = c(1959, 2)), 
     ylab = "%",
     xlab = "",
     main = "(f) CUMFNS")
title(xlab = "Time", line = 2.5)
plot(ts(data$UNRATESTx[-1], frequency = 4, start = c(1959, 2)), 
     ylab = "%",
     xlab = "",
     main = "(g) UNRATESTx")
title(xlab = "Time", line = 2.5)
plot(ts(data$FEDFUNDS[-1], frequency = 4, start = c(1959, 2)), 
     ylab = "%",
     xlab = "",
     main = "(h) FEDFUNDS")
title(xlab = "Time", line = 2.5)

par(mfrow = c(1, 1))
############
# task b
############

AR1_g_GDPC1 = c()

# Fit AR(1) model
for (i in 4:(length(subset$g_GDPC1)-1)) {
  fit = ar.ols(subset$g_GDPC1[2:i], aic = FALSE, order.max = 1, intercept = TRUE, demean = FALSE)
  AR1_g_GDPC1[i + 1] = predict(fit)$pred[1]
}

# plot original and predicted time series in one plot
ts.plot(ts(subset$g_GDPC1, frequency = 4, start = c(1959, 1)),
        ts(AR1_g_GDPC1, frequency = 4, start = c(1959, 1)),
        gpars = list(col = c("black", "red3")),
        ylab = "%GDPC1")
legend("bottomleft", c("original", "AR(1)"),
       bty = "n",
       lty = c(1,1),
       col = c("black", "red3"))

# calculate rooted MSFE
residuals_AR1 = g_GDPC1[-(1:(length(g_GDPC1) - length(na.omit(AR1_g_GDPC1))))] - na.omit(AR1_g_GDPC1)
rooted_MSPE_AR1 = sqrt(mean(diag(residuals_AR1 %*% t(residuals_AR1))))
rooted_MSPE_AR1 = rep(NA, 37)
for (i in 34:248) {
  rooted_MSPE_AR1 = c(rooted_MSPE_AR1,sqrt(mean(diag(residuals_AR1[23:i] %*% t(residuals_AR1[23:i])))) )
}

############
# task c
############


VAR1_g_GDPC1 = c()

# Fit VAR(1) model
for (i in 4:(length(subset$g_GDPC1)-1)) {
  fit = VAR(subset[2:i,], p = 1)
  VAR1_g_GDPC1[i + 1] = predict(fit, n.ahead = 1)$fcst[["g_GDPC1"]][1]
}

# plot original and predicted time series in one plot
ts.plot(ts(subset$g_GDPC1, frequency = 4, start = c(1959, 1)),
        ts(AR1_g_GDPC1, frequency = 4, start = c(1959, 1)),
        ts(VAR1_g_GDPC1, frequency = 4, start = c(1959, 1)),
        gpars = list(col = c("black", "red3", "blue")),
        ylab = "%GDPC1")
legend("bottomleft", c("original", "AR(1)", "VAR(1)"),
       lty = c(1,1,1),
       bty = "n",
       col = c("black", "red3", "blue"))

# calculate MSFE from t = 11 to t = 252
residuals_VAR1 = g_GDPC1[-(1:(length(g_GDPC1) - length(na.omit(VAR1_g_GDPC1))))] - na.omit(VAR1_g_GDPC1)
rooted_MSPE_VAR1 = sqrt(mean(diag(residuals_VAR1 %*% t(residuals_VAR1))))

rooted_MSPE_VAR1 = rep(NA, 37)
for (i in 28:242) {
  rooted_MSPE_VAR1 = c(rooted_MSPE_VAR1,sqrt(mean(diag(residuals_VAR1[17:i] %*% t(residuals_VAR1[17:i])))) )
}

############
# task d
############

fit = VAR(subset[2:length(subset$g_GDPC1),], p = 1)
summary(fit)

############
# task e
############

VARp_g_GDPC1 = c()
VARselect(subset[-1,], lag.max = 4)

# Fit VAR(p) model
for (i in 6:(length(subset$g_GDPC1)-1)) {
  fit = VAR(subset[2:i,], p = 3)
  VARp_g_GDPC1[i + 1] = predict(fit, n.ahead = 1)$fcst[["g_GDPC1"]][1]
}

# plot original and predicted time series in one plot
ts.plot(ts(subset$g_GDPC1, frequency = 4, start = c(1959, 1)),
        ts(AR1_g_GDPC1, frequency = 4, start = c(1959, 1)),
        ts(VAR1_g_GDPC1, frequency = 4, start = c(1959, 1)),
        ts(VARp_g_GDPC1, frequency = 4, start = c(1959, 1)),
        gpars = list(col = c("black", "red3", "blue3", "green3")),
        ylab = "%GDPC1")
legend("bottomleft", c("original", "AR(1)", "VAR(1)", "VAR(3)"),
       lty = c(1,1,1,1),
       bty = "n",
       col = c("black", "red3", "blue3", "green3"))



residuals_VARp = g_GDPC1[-(1:(length(g_GDPC1) - length(na.omit(VARp_g_GDPC1))))] - na.omit(VARp_g_GDPC1)
rooted_MSPE_VARp = sqrt(mean(diag(residuals_VARp %*% t(residuals_VARp))))
rooted_MSPE_VARp = rep(NA, 37)
for (i in 12:226) {
  rooted_MSPE_VARp = c(rooted_MSPE_VARp,sqrt(mean(diag(residuals_VARp[1:i] %*% t(residuals_VARp[1:i])))) )
}

RMSFE_3 = data.frame(AR = rooted_MSPE_AR1,
                     VAR1 = rooted_MSPE_VAR1,
                     VAR3 = rooted_MSPE_VARp)
rownames(RMSFE_3) = data$sasdate

res_1 = data.frame(AR1 = c(rep(NA, 4), residuals_AR1),
                   VAR1 = c(rep(NA, 10), residuals_VAR1),
                   VAR3 = c(rep(NA, 26), residuals_VARp),
                   t1 = c(rep(NA, 12), residuals_t1_),
                   t7 = c(rep(NA, 12), residuals_t7_),
                   f1 = c(rep(NA, 12), residuals_f1_),
                   f7 = c(rep(NA, 12), residuals_f7_))

rownames(res_1) = data$sasdate

save(res_1, file="res.RData")
write.csv(res_1, file="res.csv")


############
# task f
############

t = 1:64

new_1_r_AR1 = g_GDPC1[-t] - AR1_g_GDPC1[-t]
new_1_r_MSPE_AR1 = sqrt(mean(diag(new_1_r_AR1 %*% t(new_1_r_AR1))))
new_1_r_VAR1 = g_GDPC1[-t] - VAR1_g_GDPC1[-t]
new_1_r_MSPE_VAR1 = sqrt(mean(diag(new_1_r_VAR1 %*% t(new_1_r_VAR1))))
new_1_r_MSPE_VARp = rooted_MSPE_VARp

new_2_r_MSPE_AR1 = c()
new_2_r_MSPE_VAR1 = c()
new_2_r_MSPE_VARp = c()

for (i in seq(45, 252, 20)) {
  if ((i+19) > 252 ) {
    t = c(i:252)
  } else {
    t = c(i:(i+19))
  }
  print(1958 + (t+3)%/%4)
  new_2_r_AR1 = g_GDPC1[t] - AR1_g_GDPC1[t]
  new_2_r_MSPE_AR1[i] = sqrt(mean(diag(new_2_r_AR1 %*% t(new_2_r_AR1))))
  new_2_r_VAR1 = g_GDPC1[t] - VAR1_g_GDPC1[t]
  new_2_r_MSPE_VAR1[i] = sqrt(mean(diag(new_2_r_VAR1 %*% t(new_2_r_VAR1))))
  new_2_r_VARp = g_GDPC1[t] - VARp_g_GDPC1[t]
  new_2_r_MSPE_VARp[i] = sqrt(mean(diag(new_2_r_VARp %*% t(new_2_r_VARp))))
}

f = rbind(na.omit(new_2_r_MSPE_AR1), 
      na.omit(new_2_r_MSPE_VAR1), 
      na.omit(new_2_r_MSPE_VARp))
rownames(f) = c("AR(1)", "VAR(1)", "VAR(3)")
colnames(f) = seq(1975, 2021, 5)


#################################
start = (1990 - 1958) * 4 -3
test_start = (1995 - 1958) * 4 -3
test_year = 1995

AR1_1 = c()
VAR1_1 = c()
VAR3_1 = c()

for (i in (start+2):252 ) {
  fit1 = ar.ols(subset$g_GDPC1[start:i], aic = FALSE, order.max = 1, intercept = TRUE, demean = FALSE)
  AR1_1[i + 1] = predict(fit1)$pred[1]
}
for (i in (start+8):252 ) {
  fit2 = VAR(subset[start:i,], p = 1)
  VAR1_1[i + 1] = predict(fit2, n.ahead = 1)$fcst[["g_GDPC1"]][1]
}
for (i in (start+24):252 ) {
  fit3 = VAR(subset[start:i,], p = 3)
  VAR3_1[i + 1] = predict(fit3, n.ahead = 1)$fcst[["g_GDPC1"]][1]
}

new_2_r_MSPE_AR1 = c()
new_2_r_MSPE_VAR1 = c()
new_2_r_MSPE_VARp = c()


for (i in seq(test_start, 252, 20)) { #1995
  if ((i+19) > 252 ) {
    t = c(i:252)
  } else {
    t = c(i:(i+19))
  }
  print(1958 + (t+3)%/%4)
  new_2_r_AR1 = g_GDPC1[t] - AR1_1[t]
  new_2_r_MSPE_AR1[i] = sqrt(mean(diag(new_2_r_AR1 %*% t(new_2_r_AR1))))
  new_2_r_VAR1 = g_GDPC1[t] - VAR1_1[t]
  new_2_r_MSPE_VAR1[i] = sqrt(mean(diag(new_2_r_VAR1 %*% t(new_2_r_VAR1))))
  new_2_r_VARp = g_GDPC1[t] - VAR3_1[t]
  new_2_r_MSPE_VARp[i] = sqrt(mean(diag(new_2_r_VARp %*% t(new_2_r_VARp))))
}

f = rbind(na.omit(new_2_r_MSPE_AR1), 
          na.omit(new_2_r_MSPE_VAR1), 
          na.omit(new_2_r_MSPE_VARp))
rownames(f) = c("AR(1)", "VAR(1)", "VAR(3)")
colnames(f) = seq(test_year, 2021, 5)

ts.plot(ts(subset$g_GDPC1, frequency = 4, start = c(1959, 1)),
        ts(AR1_1, frequency = 4, start = c(1959, 1)),
        ts(VAR1_1, frequency = 4, start = c(1959, 1)),
        ts(VAR3_1, frequency = 4, start = c(1959, 1)),
        gpars = list(col = c("black", "red3" , "blue3", "green3")),
        ylab = "%GDPC1")




