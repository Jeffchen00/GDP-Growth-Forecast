library(xtable)

min_date = c(subset_m$sasdate[which.min(subset_m$CUMFNS)],
             subset_m$sasdate[which.min(subset_m$UNRATE)],
             subset_m$sasdate[which.min(subset_m$FEDFUNDS)],
             subset_m$sasdate[which.min(subset_m$g_CPIAUCSL)],
             subset_m$sasdate[which.min(subset_m$g_M2REAL)],
             subset_m$sasdate[which.min(subset_m$g_S.P.500)])
max_date = c(subset_m$sasdate[which.max(subset_m$CUMFNS)],
             subset_m$sasdate[which.max(subset_m$UNRATE)],
             subset_m$sasdate[which.max(subset_m$FEDFUNDS)],
             subset_m$sasdate[which.max(subset_m$g_CPIAUCSL)],
             subset_m$sasdate[which.max(subset_m$g_M2REAL)],
             subset_m$sasdate[which.max(subset_m$g_S.P.500)])

s = round(rbind(c(fivenum(subset_m$CUMFNS), mean(subset_m$CUMFNS)),
                c(fivenum(subset_m$UNRATE), mean(subset_m$UNRATE)),
                c(fivenum(subset_m$FEDFUNDS), mean(subset_m$FEDFUNDS)),
                c(fivenum(subset_m$g_CPIAUCSL), mean(subset_m$g_CPIAUCSL)),
                c(fivenum(subset_m$g_M2REAL), mean(subset_m$g_M2REAL)),
                c(fivenum(subset_m$g_S.P.500), mean(subset_m$g_S.P.500))),3)

s = cbind(s[,1], min_date, s[,5], max_date, s[,6])
colnames(s) = c("Min.", "Min. date", "Max.", "Max. date" , "Mean")
rownames(s) = c( "CapUti", "UnRate", "FedFunds", "%CPI", "%M2Stock", "%SP500")


xtable(s)




# plot the 6 variables over 1960Q1–2021Q4
par(mfrow = c(3, 1))
par(mar = c(4,4,2,1))
plot(ts(subset_m$CUMFNS[-1], frequency = 12, start = c(1960, 1)), 
     ylab = "%",
     xlab = "",
     main = "CapUti")
title(xlab = "Time", line = 2.5)
plot(ts(subset_m$UNRATE[-1], frequency = 12, start = c(1960, 1)), 
     ylab = "%",
     xlab = "",
     main = "UnRate")
title(xlab = "Time", line = 2.5)  
plot(ts(subset_m$FEDFUNDS[-1], frequency = 12, start = c(1960, 1)), 
     ylab = "%",
     xlab = "",
     main = "FedFunds")
title(xlab = "Time", line = 2.5)  
plot(ts(subset_m$g_CPIAUCSL[-1], frequency = 12, start = c(1960, 1)), 
     ylab = "%",
     xlab = "",
     main = "%CPI")
title(xlab = "Time", line = 2.5)  
plot(ts(subset_m$g_M2REAL[-1], frequency = 12, start = c(1960, 1)), 
     ylab = "%",
     xlab = "",
     main = "%M2Stock")
title(xlab = "Time", line = 2.5)  
plot(ts(subset_m$g_S.P.500[-1], frequency = 12, start = c(1960, 1)), 
     ylab = "%",
     xlab = "",
     main = "%SP500")
title(xlab = "Time", line = 2.5)  


par(mfrow = c(1, 1))



ggplot() + 
  geom_line(data = ,
            mapping = aes(date, data, group = type),
            size = 0.8, 
            alpha = 1) +
  my_theme



plot_data = data.frame(data = unlist(subset_m[,-1]),
                       time = rep(as.Date(subset_m$sasdate, tryFormats = c("%m/%d/%Y")), 
                                  ncol(subset_m[,-1])),
                       type = rep(colnames(subset_m[,-1]), each = nrow(subset_m)))


statistic = data.frame(std1 = sapply(subset_m[,-1], mean) + 1.5 * sapply(subset_m[,-1], sd),
                       srd2 = sapply(subset_m[,-1], mean) - 1.5 * sapply(subset_m[,-1], sd),
                       mean = sapply(subset_m[,-1], mean))
std_data = data.frame(std = rep(unlist(t(statistic)), each = 2),
                      time = rep(as.Date(subset_m$sasdate[c(1,nrow(subset_m))], 
                                         tryFormats = c("%m/%d/%Y")), 
                                 ncol(subset_m[,-1]) * 3),
                      variable = rep(colnames(subset_m[,-1]), each = 6),
                      type = rep(c("std1", "std2", "mean"), 6, each = 2))

plots = list()

for (t in colnames(subset_m[,-1])) {
  plots[[t]] = ggplot(plot_data[which(plot_data$type == t),], aes(time, data, color = type), show.legend = FALSE) +
    geom_line(size = 0.5, alpha = 1, show.legend = FALSE) +
    my_theme + 
    geom_line(data = std_data[which(std_data$variable == t & std_data$type != "mean"),], 
              aes(time, std, group = type, colour = "#e1e1e1"),
              linetype = 2, show.legend = FALSE) + 
    geom_line(data = std_data[which(std_data$variable == t & std_data$type == "mean"),], 
              aes(time, std, group = type, colour = "#e1e1e1"),
              linetype = 1, show.legend = FALSE) +
    ggtitle(t) +
    theme(plot.title = element_text(face = "bold")) +
    ylab("%")
  
}

ggarrange(
  #plots[[1]], plots[[2]], plots[[3]], 
  plots[[4]], plots[[5]], plots[[6]],
  nrow = 3, ncol =1)

