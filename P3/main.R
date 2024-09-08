library(tseries)
library(midasr)
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

############ Data preparation ############

data_q = read.csv("2022-02 Q.csv")[-c((1:2),(255:256)), c(
  "sasdate",
  "GDPC1",
  "CUMFNS",
  "UNRATESTx",
  "CPIAUCSL",
  "FEDFUNDS",
  "M1REAL",
  "S.P.500"
)]
data_m = read.csv("2022-02 M.csv")[-c(1, 758, 759),c(
  "sasdate",
  "CUMFNS",
  "UNRATE",
  "CPIAUCSL",
  "FEDFUNDS",
  "M2REAL",
  "S.P.500"
)]

growth = function(feature) {
  g = (feature[2:length(feature)] - feature
       [1:(length(feature) - 1)]) / feature[1:(length(feature) - 1)] * 100
  return(c(NA, g))
}

subset_q = cbind(
  data_q[c("sasdate", "CUMFNS", "UNRATESTx", "FEDFUNDS")],
  growth(data_q$GDPC1),
  growth(data_q$CPIAUCSL),
  growth(data_q$M1REAL),
  growth(data_q$S.P.500)
)[-(1:4),]

subset_m = cbind(
  data_m[c("sasdate", "CUMFNS", "UNRATE", "FEDFUNDS")],
  growth(data_m$CPIAUCSL),
  growth(data_m$M2REAL),
  growth(data_m$S.P.500)
)[-(1:12),]

row.names(subset_q) <- c(1:nrow(subset_q))
row.names(subset_m) <- c(1:nrow(subset_m))

colnames(subset_q) = c(
  "sasdate",
  "CUMFNS",
  "UNRATESTx",
  "FEDFUNDS",
  "g_GDPC1",
  "g_CPIAUCSL",
  "g_M1REAL",
  "g_S.P.500"
)

colnames(subset_m) = c(
  "sasdate",
  "CUMFNS",
  "UNRATE",
  "FEDFUNDS", 
  "g_CPIAUCSL",
  "g_M2REAL",
  "g_S.P.500"
)


############ task a

create_dataset <- function(subset_q, subset_m, start, k) {
  lag = data.frame(mls(subset_q$g_GDPC1, 0:1, 1),
                     mls(subset_m$CUMFNS, start:(k+1), 3),
                     mls(subset_m$UNRATE, start:(k+1), 3),
                     mls(subset_m$FEDFUNDS, start:(k+1), 3),
                     mls(subset_m$g_CPIAUCSL, start:(k+1), 3),
                     mls(subset_m$g_M2REAL, start:(k+1), 3),
                     mls(subset_m$g_S.P.500, start:(k+1), 3))
  
  colnames(lag) = c(sprintf("g_GDP_%d", 0:1),
                      sprintf("CapUti_%d", start:(k+1)),
                      sprintf("UnRate_%d", start:(k+1)),
                      sprintf("FedFunds_%d", start:(k+1)),
                      sprintf("g_CPI_%d", start:(k+1)),
                      sprintf("g_M2Stock_%d", start:(k+1)),
                      sprintf("g_SP500_%d", start:(k+1)))
  rownames(lag) = subset_q$sasdate
  return(lag)
}

myBIC <- function(model) {
  res = model$residuals
  n_sample = length(model$residuals)
  n_parameter = length(model$coefficients)
  BIC = log(t(res)%*%res / n_sample) + log(n_sample) / n_sample  * n_parameter 
  return(BIC)
}

BIC_score_1 = c()
for (k in 1:3) {
  lag_1 = create_dataset(subset_q, subset_m, 1, k)
  
  um_1 <- midas_u(formula = as.formula(paste("g_GDP_0 ~", paste(names(lag_1)[!names(lag_1) %in% "g_GDP_0"], collapse = " + "))),
                data = lag_1)
  
  BIC_score_1 = c(BIC_score_1, myBIC(um_1))
}

BIC_score_1
which.min(BIC_score_1)

############ task b

BIC_score_2 = c()
for (k in 1:3) {
  lag_2 = create_dataset(subset_q, subset_m, 0, k)
  
  um_2 <- midas_u(formula = as.formula(paste("g_GDP_0 ~", 
                                           paste(names(lag_2)[!names(lag_2) %in% "g_GDP_0"], 
                                                 collapse = " + "))),
                data = lag_2)
  
  BIC_score_2 = c(BIC_score_2, myBIC(um_2))
}
BIC_score_2
which.min(BIC_score_2)

########## task c
lag_1 = create_dataset(subset_q, subset_m, start = 1, k=3)[-1,]

um_1 <- midas_u(formula = as.formula(paste("g_GDP_0 ~", 
                                           paste(names(lag_1)[!names(lag_1) %in% "g_GDP_0"], 
                                                 collapse = " + "))),
                data = lag_1)

lag_1_new = lag_1[,which(summary(um_1)$coefficients[, "Pr(>|t|)"]<0.05)]
um_new_1 = midas_u(formula = as.formula(paste("g_GDP_0 ~", 
                                              paste(names(lag_1_new)[!names(lag_1_new) %in% "g_GDP_0"], 
                                                    collapse = " + "))),
                   data = lag_1_new)
BIC_score_new_1 = myBIC(um_new_1)
########## task d

lag_2 = create_dataset(subset_q, subset_m, start = 0, k=3)
um_2 <- midas_u(formula = as.formula(paste("g_GDP_0 ~", 
                                           paste(names(lag_2)[!names(lag_2) %in% "g_GDP_0"], 
                                                 collapse = " + "))),
                data = lag_2)
lag_2_new = lag_2[,which(summary(um_2)$coefficients[, "Pr(>|t|)"]<0.05)]
um_new_2 = midas_u(formula = as.formula(paste("g_GDP_0 ~", 
                                              paste(names(lag_2_new)[!names(lag_2_new) %in% "g_GDP_0"], 
                                                    collapse = " + "))),
                   data = lag_2_new)

BIC_score_new_2 = myBIC(um_new_2)

########## task e f

library(caret)
library(doParallel)
library(e1071)
library(ranger)
library(dplyr)
registerDoParallel(cores=5)


# create Data Partition
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 3,
                              horizon = 1,
                              fixedWindow = FALSE,
                              allowParallel = TRUE)

#### nVar 

list_mtry = rep(0, 25)
list_mtry_2 = rep(0, 31)
for (i in 1:10) {
  rf.mod_1 <- train(x=na.omit(lag_1)[,2:ncol(lag_1)],
                    y=na.omit(lag_1)[,1],
                    method = "ranger",
                    trControl = myTimeControl,
                    tuneGrid = data.frame(mtry = rep(1:25, 1),
                                          splitrule = rep("variance", 25),
                                          min.node.size = rep(5, 25)),
                    metric='RMSE',
                    num.trees = 500)
  rf.mod_2 <- train(x=na.omit(lag_2)[,2:ncol(lag_2)],
                    y=na.omit(lag_2)[,1],
                    method = "ranger",
                    trControl = myTimeControl,
                    tuneGrid = data.frame(mtry = rep(1:31, 1),
                                          splitrule = rep("variance", 31),
                                          min.node.size = rep(5, 31)),
                    metric='RMSE')
  list_mtry = list_mtry + rf.mod_1$results$RMSE
  list_mtry_2 = list_mtry_2 + rf.mod_2$results$RMSE
}

which.min(list_mtry)
which.min(list_mtry_2)

plot_data <- data.frame(
  RMSE = c(list_mtry/21, list_mtry_2/21),
  mtry = c(rf.mod_1$results$mtry, rf.mod_2$results$mtry),
  type = c(rep("Random Forest 1", nrow(rf.mod_1$results)),  
           rep("Random Forest 2", nrow(rf.mod_2$results)))
)

ggplot(plot_data, aes(mtry, RMSE, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("RMSE") +
  xlab("number of randomly selected variables") + 
  coord_cartesian(ylim = c(0.615, 0.635)) +
  my_theme

#### minNode

list_node_1 = rep(0, 19)
list_node_2 = rep(0, 19)
for (i in 1:10) {
  rf.mod_1_node <- train(x=na.omit(lag_1)[,2:ncol(lag_1)],
                         y=na.omit(lag_1)[,1],
                         method = "ranger",
                         trControl = myTimeControl,
                         tuneGrid = data.frame(mtry = rep(4, 19),
                                               splitrule = rep("variance", 19),
                                               min.node.size = rep(2:20, 1)),
                         metric='RMSE',
                         num.trees = 500)
  rf.mod_2_node <- train(x=na.omit(lag_2)[,2:ncol(lag_2)],
                         y=na.omit(lag_2)[,1],
                         method = "ranger",
                         trControl = myTimeControl,
                         tuneGrid = data.frame(mtry = rep(5, 19),
                                               splitrule = rep("variance", 19),
                                               min.node.size = rep(2:20, 1)),
                         metric='RMSE',
                         num.trees = 500)
  list_node_1 = list_node_1 + rf.mod_1_node$results$RMSE
  list_node_2 = list_node_2 + rf.mod_2_node$results$RMSE
  print(i)
}

which.min(list_node_1)
which.min(list_node_2)

plot_data <- data.frame(
  RMSE = c(list_node_1/20, list_node_2/20),
  node = c(rf.mod_1_node$results$min.node.size, rf.mod_2_node$results$min.node.size),
  type = c(rep("Random Forest 1", nrow(rf.mod_1_node$results)),  
           rep("Random Forest 2", nrow(rf.mod_2_node$results)))
)

ggplot(plot_data, aes(node, RMSE, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("RMSE") +
  xlab("minimal node size") +
  coord_cartesian(ylim = c(0.615, 0.635)) +
  my_theme


#### nTrees

list_1 = list()
list_2 = list()
for (i in 2:10) {
  for (ntree in 20*(1:40)) {
    rf.mod_1_tree <- train(x=na.omit(lag_1)[,2:ncol(lag_1)],
                           y=na.omit(lag_1)[,1],
                           method = "ranger",
                           trControl = myTimeControl,
                           tuneGrid = data.frame(mtry = 4,
                                                 splitrule= "variance",
                                                 min.node.size=2),
                           metric='RMSE',
                           num.trees	= ntree)
    rf.mod_2_tree <- train(x=na.omit(lag_1)[,2:ncol(lag_1)],
                           y=na.omit(lag_1)[,1],
                           method = "ranger",
                           trControl = myTimeControl,
                           tuneGrid = data.frame(mtry = 5,
                                                 splitrule= "variance",
                                                 min.node.size=2),
                           metric='RMSE',
                           num.trees	= ntree)
    list_1[[toString(ntree)]] = list_1[[toString(ntree)]] + rf.mod_1_tree$results$RMSE
    list_2[[toString(ntree)]] = list_2[[toString(ntree)]] + rf.mod_2_tree$results$RMSE
  }
  print(i)
}


plot_data <- data.frame(
  RMSE = c(unlist(list_1)/10, unlist(list_2)/10),
  ntree = rep(20*(1:40),2),
  type = c(rep("Random Forest 1", length(list_1)),  
           rep("Random Forest 2", length(list_2)))
)

ggplot(plot_data, aes(ntree, RMSE, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("RMSE") +
  xlab("number of trees") +
  coord_cartesian(ylim = c(0.615, 0.655)) +
  my_theme +
  theme(legend.position = c(0.75,0.7))

########### task g
pred = data.frame(matrix(nrow = nrow(lag_1), ncol = 6))
colnames(pred) = c("U-MIDAS 1", 
                   "U-MIDAS 2", 
                   "Reduced U-MIDAS 1", 
                   "Reduced U-MIDAS 2", 
                   "Random Forest 1", 
                   "Random Forest 2")
rownames(pred) = rownames(lag_1)

forecast_GDP_um <- function(d, start) {
  temp = rep(NA, start)
  for (i in start:(nrow(d) - 1)) {
    model = midas_u(formula = as.formula(paste("g_GDP_0 ~",
                                               paste(names(d)[!names(d) %in% "g_GDP_0"],
                                                     collapse = " + "))),
                    data = d[start:i, ])
    if (model$df.residual > 0) {
      temp = c(temp, predict(model, d[i+1, -1]))
    } else {
      temp = c(temp, NA)
    }
  }
  return(temp)
}

forecast_GDP_rf <- function(d, start, my_mtry, my_min.node.size, my_num.trees) {
  temp = rep(NA, start)
  for (i in start:(nrow(d) - 1)) {
    rf_1 = ranger(g_GDP_0 ~.,
                  data = d[start:i,],
                  mtry = my_mtry,
                  min.node.size = my_min.node.size,
                  num.trees = my_num.trees)
    temp = c(temp, predict(rf_1 , d[i+1, -1])$predictions)
  }
  return(temp)
}

for (d_i in 1:4) {
  d = list(lag_1, lag_2, lag_1_new, lag_2_new)[[d_i]]
  pred[, d_i] = forecast_GDP_um(d, 2)
}

pred[, "Random Forest 1"] = forecast_GDP_rf(lag_1, start = 2, my_mtry=4, 
                                 my_min.node.size = 2, my_num.trees = 400)
pred[, "Random Forest 2"] = forecast_GDP_rf(lag_2, start = 2, my_mtry=5, 
                                 my_min.node.size = 2, my_num.trees = 400)

plot_data <- data.frame(
  percent = c(lag_1$g_GDP_0, unlist(pred)),
  time = as.Date(rownames(lag_1), tryFormats = c("%m/%d/%Y")),
  type = rep(
    c("original", colnames(pred)),
    each = length(lag_1$g_GDP_0)
  )
)

ggplot(plot_data, aes(time, percent, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("%GDP") +
  coord_cartesian(ylim = c(-5, 8)) +
  my_theme

res = pred - cbind(rep(lag_1$g_GDP_0, 6))


##### plot |res|

res_it = res[,5:6]

plot_data <- data.frame(
  percent = abs(unlist(res_it)),
  time = as.Date(rownames(res_it), tryFormats = c("%m/%d/%Y")),
  type = rep(
    colnames(res_it), each = nrow(res_it)
  )
)

ggplot(plot_data, aes(time, percent, color = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("| residual |") +
  coord_cartesian(ylim = c(0, 10)) +
  my_theme + 
  theme(legend.position = c(0.8, 0.8))



##### plot RMSFE

calculate_RMFSE <- function(start, res, with_na) {
  RMSFE = data.frame(matrix(nrow = nrow(res), 
                            ncol = ncol(res)))
  colnames(RMSFE) = colnames(res)
  rownames(RMSFE) = rownames(res)
  
  for (col_n in colnames(res)) {
    temp = c()
    for (i in start:nrow(res)) {
      
      temp = c(temp, sqrt(mean(diag(res[start:i, col_n] %*% t(res[start:i, col_n])))) )
    }
    RMSFE[, col_n] = c(rep(NA, start-1), temp)
  }
  if (with_na == FALSE) {
    return(RMSFE[-(1:(start-1)),])
  }
  return(RMSFE)
}

load("res.RData")
colnames(res_1) = c("Project 1 - AR(1)",
                    "Project 1 - VAR(1)",
                    "Project 1 - VAR(3)",
                    "Project 2 - Regression Tree with quarterly %GDP",
                    "Project 2 - Regression Tree with 7 quarterly variables",
                    "Project 2 - Random Forest with quarterly %GDP",
                    "Project 2 - Random Forest with 7 quarterly variables"
                    )

RMSFE = calculate_RMFSE(start = 35, res, TRUE) ### 1968Q3
colnames(RMSFE) = c("Project 3 - U-MIDAS 1", 
                    "Project 3 - U-MIDAS 2", 
                    "Project 3 - Reduced U-MIDAS 1", 
                    "Project 3 - Reduced U-MIDAS 2", 
                    "Project 3 - Random Forest 1",
                    "Project 3 - Random Forest 2"
                    )
RMSFE = cbind(RMSFE, calculate_RMFSE(start = 39, res_1, TRUE)[-(1:4),]) ### 1968Q3

# RMSFE = calculate_RMFSE(start = 38, res_1, TRUE)[-(1:4),]



plot_data <- data.frame(
  percent = unlist(RMSFE),
  time = as.Date(rownames(RMSFE), tryFormats = c("%m/%d/%Y")),
  type = rep(colnames(RMSFE), each = nrow(RMSFE))
)

ggplot(plot_data, aes(time, percent, colour = type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  ylab("RMSFE") +
  my_theme + 
  theme(legend.position = c(0.8, 0.7)) +
  coord_cartesian(ylim = c(0, 3))



ggplot() + 
  geom_line(data = data.frame( RMSFE = unlist(RMSFE[,-c(1:4, 7:9)]),
                               time = as.Date(rownames(RMSFE[,-c(1:4, 7:9)]), 
                                              tryFormats = c("%m/%d/%Y")),
                               type = rep(colnames(RMSFE[,-c(1:4, 7:9)]), 
                                          each = nrow(RMSFE[,-c(1:4, 7:9)]))),
            mapping = aes(time, RMSFE, group = type),
            size = 0.8, 
            alpha = 1,
            colour = c("#e2e4e6")) +
  geom_line(data = data.frame( RMSFE = unlist(RMSFE[,c(1:4, 7:9)]),
                               time = as.Date(rownames(RMSFE[,c(1:4, 7:9)]), 
                                              tryFormats = c("%m/%d/%Y")),
                               type = rep(colnames(RMSFE[,c(1:4, 7:9)]), 
                                          each = nrow(RMSFE[,c(1:4, 7:9)]))),
            mapping = aes(time, RMSFE, colour = type),
            size = 0.8, 
            alpha = 1) +
  my_theme +
  theme(legend.position = c(0.8,0.8)) +
  coord_cartesian(ylim = c(0, 2.5))
  
ggplot() + 
  geom_line(data = data.frame( RMSFE = unlist(RMSFE[,c(1:4, 7:9)]),
                               time = as.Date(rownames(RMSFE[,c(1:4, 7:9)]), 
                                              tryFormats = c("%m/%d/%Y")),
                               type = rep(colnames(RMSFE[,c(1:4, 7:9)]), 
                                          each = nrow(RMSFE[,c(1:4, 7:9)]))),
            mapping = aes(time, RMSFE, group = type),
            size = 0.8, 
            alpha = 1,
            colour = c("#e2e4e6")) +
  geom_line(data = data.frame( RMSFE = unlist(RMSFE[,-c(1:4, 7:9)]),
                               time = as.Date(rownames(RMSFE[,-c(1:4, 7:9)]), 
                                              tryFormats = c("%m/%d/%Y")),
                               type = rep(colnames(RMSFE[,-c(1:4, 7:9)]), 
                                          each = nrow(RMSFE[,-c(1:4, 7:9)]))),
            mapping = aes(time, RMSFE, colour = type),
            size = 0.8, 
            alpha = 1) +
  my_theme +
  theme(legend.position = c(0.6,0.8)) +
  coord_cartesian(ylim = c(0, 2.5))

########### task h

