library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)

######### visualization style setting ######################

my_theme <- theme(panel.border = element_rect(fill = NA, colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 10, face = "bold"),
                  axis.title = element_text(size = 14, face = "bold"),
                  strip.text = element_text(size = 15, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 14),
                  legend.title = element_blank(),
                  legend.background = element_blank(),
                  legend.key = element_rect(fill = "white"),
                  legend.position = c(0.75,0.2))

######### data preparation ######################

data = read.csv("2022-02 Q.csv")[-c((1:2), (255:256)),]
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

subset = cbind(
  subset[c("sasdate", "CUMFNS", "UNRATESTx", "FEDFUNDS")],
  growth(subset$GDPC1),
  growth(subset$CPIAUCSL),
  growth(subset$M1REAL),
  growth(subset$S.P.500)
)

colnames(subset) = c(
  "sasdate",
  "CUMFNS",
  "UNRATESTx",
  "FEDFUNDS",
  "g_GDPC1",
  "g_CPIAUCSL",
  "g_M1REAL",
  "g_S.P.500"
)


# create training matrix

lag_matirx_g_GDPC1 = data.frame(g_GDPC1 = subset$g_GDPC1)
for (i in 1:10) {
  lag_matirx_g_GDPC1[sprintf("g_GDP_%d", i)] =
    c(rep(NA, i), subset$g_GDPC1[1:(nrow(subset) - i)])
}

lag_matirx_CUMFNS = data.frame(g_GDPC1 = subset$g_GDPC1)
for (i in 1:10) {
  lag_matirx_CUMFNS[sprintf("CapUti_%d", i)] =
    c(rep(NA, i), subset$CUMFNS[1:(nrow(subset) - i)])
}

lag_matirx_UNRATESTx = data.frame(g_GDPC1 = subset$g_GDPC1)
for (i in 1:10) {
  lag_matirx_UNRATESTx[sprintf("UnRate_%d", i)] =
    c(rep(NA, i), subset$UNRATESTx[1:(nrow(subset) - i)])
}

lag_matirx_FEDFUNDS = data.frame(g_GDPC1 = subset$g_GDPC1)
for (i in 1:10) {
  lag_matirx_FEDFUNDS[sprintf("FedFunds_%d", i)] =
    c(rep(NA, i), subset$FEDFUNDS[1:(nrow(subset) - i)])
}

lag_matirx_g_CPIAUCSL = data.frame(g_GDPC1 = subset$g_GDPC1)
for (i in 1:10) {
  lag_matirx_g_CPIAUCSL[sprintf("g_CPI_%d", i)] =
    c(rep(NA, i), subset$g_CPIAUCSL[1:(nrow(subset) - i)])
}

lag_matirx_g_M1REAL = data.frame(g_GDPC1 = subset$g_GDPC1)
for (i in 1:10) {
  lag_matirx_g_M1REAL[sprintf("g_M1Stock_%d", i)] =
    c(rep(NA, i), subset$g_M1REAL[1:(nrow(subset) - i)])
}

lag_matirx_g_S.P.500 = data.frame(g_GDPC1 = subset$g_GDPC1)
for (i in 1:10) {
  lag_matirx_g_S.P.500[sprintf("g_SP500_%d", i)] =
    c(rep(NA, i), subset$g_S.P.500[1:(nrow(subset) - i)])
}

lag_matirx_ALL = cbind(
  lag_matirx_g_GDPC1,
  lag_matirx_CUMFNS[,-1],
  lag_matirx_UNRATESTx[,-1],
  lag_matirx_FEDFUNDS[,-1],
  lag_matirx_g_CPIAUCSL[,-1],
  lag_matirx_g_M1REAL[,-1],
  lag_matirx_g_S.P.500[,-1]
)

######### task a ##############

t1 = rpart(
  g_GDPC1 ~ .,
  data = lag_matirx_g_GDPC1,
  subset = 12:nrow(lag_matirx_g_GDPC1),
  method = "anova"
)
rpart.plot(t1, digits = 3, tweak = 1, extra = 0, box.palette=0 )

plot_data <- data.frame(
  percent = c(subset$g_GDPC1,
              c(rep(NA, 11), predict(t1))),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c("original", "regression tree forecast"),
    each = length(subset$g_GDPC1)
  )
)

ggplot(plot_data, aes(time, percent, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("%GDPC1") +
  my_theme

######### task b ##############

t7 = rpart(
  g_GDPC1 ~ .,
  data = lag_matirx_ALL,
  subset = 12:nrow(lag_matirx_ALL),
  method = "anova"
)

rpart.plot(t7, digits = 3, tweak = 1.1,extra = 0, box.palette=0)

plot_data <- data.frame(
  percent = c(subset$g_GDPC1,
              c(rep(NA, 11), predict(t7))),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c("original", "regression tree forecast"),
    each = length(subset$g_GDPC1)
  )
)
ggplot(plot_data, aes(time, percent, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("%GDPC1") +
  my_theme

######### task c ##############

# the first 12 observations has no prediction
t7_pred = rep(NA, 12)

for (i in 12:(nrow(subset) - 1)) {
  t7_ = rpart(g_GDPC1 ~ .,
              data = lag_matirx_ALL,
              subset = 12:i,
              method = "anova")
  t7_pred = c(t7_pred, predict(t7_, lag_matirx_ALL[i + 1,]))
}

t1_pred = rep(NA, 12)

for (i in 12:(nrow(subset) - 1)) {
  t1_ = rpart(g_GDPC1 ~ .,
              data = lag_matirx_g_GDPC1,
              subset = 12:i,
              method = "anova")
  t1_pred = c(t1_pred, predict(t1_, lag_matirx_g_GDPC1[i + 1,]))
}


plot_data <- data.frame(
  percent = c(subset$g_GDPC1, t7_pred, t1_pred),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c("original", 
      "regression tree forecast results (all variables with 10 lags)", 
      "regression tree forecast results (%GDP with 10 lags)"),
    each = length(subset$g_GDPC1)
  )
)
ggplot(plot_data, aes(time, percent, color = type)) +
  geom_line(size = 0.5, alpha = 0.75) +
  ylab("%GDP") +
  my_theme+
  theme(legend.position = c(0.4, 0.2))

residuals_t7_ = subset$g_GDPC1[13:252] - na.omit(t7_pred)
rooted_MSPE_t7_ = rep(NA, 37)
for (i in 26:240) {
  rooted_MSPE_t7_ = c(rooted_MSPE_t7_, sqrt(mean(diag(residuals_t7_[15:i] %*% t(residuals_t7_[15:i])))))
}
residuals_t1_ = subset$g_GDPC1[13:252] - na.omit(t1_pred)
rooted_MSPE_t1_ = rep(NA, 37)
for (i in 26:240) {
  rooted_MSPE_t1_ = c(rooted_MSPE_t1_, sqrt(mean(diag(residuals_t1_[15:i] %*% t(residuals_t1_[15:i])))))
}

plot_data <- data.frame(
  res = c(rep(NA, 12), abs(residuals_t7_), rep(NA, 12),  abs(residuals_t1_)),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c("regression tree (all variables with 10 lags)", 
      "regression tree (%GDP with 10 lags)"),
    each = length(subset$g_GDPC1)
  )
)
ggplot(plot_data, aes(time, res, color = type)) +
  geom_line(size = 0.7, alpha = 0.75) +
  ylab("| residuals |") +
  my_theme+
  ylim(-0.1,12)+
  theme(legend.position = c(0.35, 0.8))

######### task d ##############
PVI = rep(0, 70)

for (i in 1:10) {
  f7 = randomForest(g_GDPC1 ~ ., data=lag_matirx_ALL,
                    subset = 12:252,
                    importance=TRUE)
  PVI = PVI + importance(f7, type =1, scale = FALSE)
}
PVI = PVI / 10

temp = data.frame(rownames(PVI), PVI)
temp = data.frame(temp[order(temp$X.IncMSE, decreasing = TRUE),])
rownames(temp) = c(1:70)
colnames(temp) = c("variable", "IncMSE")
a = temp[1:35,]
b = temp[36:70,]
ggplot(b, aes(x = reorder(variable, IncMSE), y =IncMSE)) +
      ylab("average MSE increase") +
      ylim(-0.05, 0.05) + 
      geom_bar(stat = "identity", fill = "black", width = 0.65) +
      coord_flip() +
      theme_light() +
      theme(axis.title.x = element_text( color = "black"),
            axis.title.y = element_blank(),
            axis.text.x  = element_text( color = "black"),
            axis.text.y  = element_text( color = "black"))

plot_data <- data.frame(
  percent = c(subset$g_GDPC1,
              c(
                rep(NA, 11), predict(f7, lag_matirx_ALL[12:nrow(lag_matirx_ALL), -1])
              )),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(c("original", "random forest forecast"),
             each = length(subset$g_GDPC1))
)
ggplot(plot_data, aes(time, percent, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("%GDP") +
  my_theme


######### task e ##############

f7_pred = rep(NA, 12)

for (i in 12:(nrow(subset) - 1)) {
  f7_ = randomForest(g_GDPC1 ~ ., data=lag_matirx_ALL,
                     subset = 12:i)
  f7_pred = c(f7_pred, predict(f7_, lag_matirx_ALL[i + 1,-1]))
}

f1_pred = rep(NA, 12)

for (i in 12:(nrow(subset) - 1)) {
  f1_ = randomForest(g_GDPC1 ~ ., data=lag_matirx_g_GDPC1,
                     subset = 12:i)
  f1_pred = c(f1_pred, predict(f1_, lag_matirx_g_GDPC1[i + 1,-1]))
}

plot_data <- data.frame(
  percent = c(subset$g_GDPC1, f7_pred, f1_pred),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c("original", 
      "random forest forecast results (all variables with 10 lags)", 
      "random forest forecast results (%GDP with 10 lags)"),
    each = length(subset$g_GDPC1)
  )
)
ggplot(plot_data, aes(time, percent, color = type)) +
  geom_line(size = 0.5, alpha = 0.75) +
  ylab("%GDP") +
  my_theme +
  theme(legend.position = c(0.4, 0.2))

residuals_f7_ = subset$g_GDPC1[13:252] - na.omit(f7_pred)
rooted_MSPE_f7_ = rep(NA, 37)
for (i in 26:240) {
  rooted_MSPE_f7_ = c(rooted_MSPE_f7_,sqrt(mean(diag(residuals_f7_[15:i] %*% t(residuals_f7_[15:i])))) )
}

residuals_f1_ = subset$g_GDPC1[13:252] - na.omit(f1_pred)
rooted_MSPE_f1_ = rep(NA, 37)
for (i in 26:240) {
  rooted_MSPE_f1_ = c(rooted_MSPE_f1_,sqrt(mean(diag(residuals_f1_[15:i] %*% t(residuals_f1_[15:i])))) )
}

RMSFE_2 = data.frame(f1 = rooted_MSPE_f1_,
                     f7 = rooted_MSPE_f7_,
                     t1 = rooted_MSPE_t1_,
                     t7 = rooted_MSPE_t7_)

rownames(RMSFE_2) = data$sasdate

plot_data <- data.frame(
  res = c(
          rep(NA, 12), abs(residuals_f7_), rep(NA, 12),  abs(residuals_f1_)),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c(
      "random forest (all variables with 10 lags)", 
      "random forest (%GDP with 10 lags)"),
    each = length(subset$g_GDPC1)
  )
)
ggplot(plot_data, aes(time, res, color = type)) +
  geom_line(size = 0.7, alpha = 0.75) +
  ylab("| residuals |") +
  my_theme+
  ylim(-0.1,12)+
  theme(legend.position = c(0.35, 0.8))


plot_data <- data.frame(
  res = c(
    rep(NA, 4), abs(residuals_AR1), rep(NA, 10),  abs(residuals_VAR1),
    rep(NA, 26),  abs(residuals_VARp)),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c(
      "AR(1)", 
      "VAR(1)",
      "VAR(3)"),
    each = length(subset$g_GDPC1)
  )
)
ggplot(plot_data, aes(time, res, color = type)) +
  geom_line(size = 0.7, alpha = 0.75) +
  ylab("| residuals |") +
  my_theme+
  theme(legend.position = c(0.25, 0.8)) +
  coord_cartesian(ylim = c(0.1, 12))


plot_res <- data.frame(
  res = c(rooted_MSPE_t1_, 
          rooted_MSPE_t7_, 
          rooted_MSPE_f1_,
          rooted_MSPE_f7_
          ),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c("regression tree with %GDP", 
      "regression tree with all variables", 
      "random forest with %GDP",
      "random forest with all variables"
      ),
    each = length(subset$g_GDPC1)
  )
)

ggplot(plot_res, aes(time, res, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylim(0,5)+
  ylab("root MSFE") +
  my_theme +
  theme(legend.position = c(0.5,0.8)) +
  scale_fill_discrete(limits =c("regression tree with %GDP", 
                               "regression tree with all variables", 
                               "random forest with %GDP",
                               "random forest with all variables"
  ))+
  coord_cartesian(ylim = c(0, 3))


plot_res <- data.frame(
  res = c(rooted_MSPE_AR1, 
          rooted_MSPE_VAR1, 
          rooted_MSPE_VARp
  ),
  time = as.Date(subset$sasdate, tryFormats = c("%m/%d/%Y")),
  type = rep(
    c("AR(1)", 
      "VAR(1)", 
      "VAR(3)"
    ),
    each = length(subset$g_GDPC1)
  )
)

ggplot(plot_res, aes(time, res, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylim(0,5)+
  ylab("root MSFE") +
  my_theme +
  theme(legend.position = c(0.5,0.8)) +
  scale_fill_discrete(limits =c("regression tree with %GDP", 
                                "regression tree with all variables", 
                                "random forest with %GDP",
                                "random forest with all variables"
  ))+
  coord_cartesian(ylim = c(0, 3))

