# ======================================================== #
# 				 Leave this part unchanged                 #
# ======================================================== #
source("~/Rconfig.R")
source("functions_master_thesis.R")

my_setup()

# ================================= #
# Load and prepare Data
# ================================= #

inc <- my_load_data()




# ======================================================== #
#							 Sandbox                       #
# ======================================================== #



#==========================================#
# ===== XGBoost ====== #

library(xgboost)
df <- data.frame(date = as.POSIXlt(inc$date), 
				 inc = inc$inc, 
				 inc_next_day = c(inc$inc[-1], NA),
				 weekday = as.POSIXlt(inc$date)$wday, 
				 weekday = as.POSIXlt(inc$date)$month)

splitdate = df$date[round(2/3 * nrow(df))]
df$day <- as.numeric(substr(df$date, 9, 10))
df$month <- as.numeric(substr(df$date, 6, 7))
df$year <- as.numeric(substr(df$date, 1, 4))
head(df)

cols <- c("inc", "weekday", "day", "month", "year")
X_train <- as.matrix(df[df$date <= splitdate, cols])
y_train <- df[df$date <= splitdate, "inc_next_day"]

X_test <- as.matrix(df[df$date > splitdate, cols])
y_test <- df[df$date > splitdate, "inc_next_day"]

trainDMatrix <- xgb.DMatrix(data = X_train, label = y_train)
params <- list(booster = "gbtree"
               , objective = "reg:linear"
               , eta=0.4
               , gamma=0
               )

xgb.tab <- xgb.cv(data = trainDMatrix
                  , param = params
                  , maximize = FALSE, evaluation = "rmse", nrounds = 100
                  , nthreads = 10, nfold = 2, early_stopping_round = 10)



num_iterations = xgb.tab$best_iteration
model <- xgb.train(data = trainDMatrix
                               , param = params
                               , maximize = FALSE, evaluation = 'rmse', nrounds = num_iterations)


importance <- xgb.importance(feature_names = colnames(trainDMatrix), model = model)
xgb.ggplot.importance(importance_matrix = importance)

testDMatrix <- xgb.DMatrix(data = X_test, label = y_test)

pred <- predict(model, testDMatrix)

plot(pred)


# bstSparse <- xgboost::xgboost(data = as.matrix(X_train), label = y_train, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "reg:linear")













#==========================================#
# ===== test scoring of predictions ====== #

## calibration

# true values
yhat <- posterior_draws
ytrue <- inc

# simulated data
n <- 1000
ytrue<- rep(0, n) 
ytrue<- rnorm(n, 0, 1)
yhat <- matrix(rnorm(n * n, 0, 1), nrow = 1000)

ytrue<- rep(10, n) 
yhat <- matrix(rnbinom(n * n, mu = 10, 1), nrow = 1000)

u <- my_PIT(ytrue, yhat)
hist(u, breaks = 100)

# Frage: sind meine Vorhersagen nicht immer overdispersed? in den wahren Werten hab ich ja keine Varianz

## centrality
centr <- my_centrality(u)
centr

## sharpness
hist(my_sharpness(yhat), breaks = 100)

## bias
max(my_bias(ytrue, yhat))

# warum ist bias negativ? 



## this is mostly about sharpness
my_RPS(ytrue, yhat)

my_DSS(ytrue, yhat)



x1 <- rnorm(1000, 5.8, 2.4) # 5.8

x2 <-  rnorm(1000, mean = qnorm(0.6, 1.7 + x1, 3.4), sd = 3.4)
x2 <-  rnorm(1000, mean = qnorm(0.6, 0, 3.4) qnorm(0.6, 1.7 + x1, 3.4), sd = 3.4)

qnorm(0.1, 0, 1)
a <- rnorm(100000, 0, 1)


sum(a > -0.253)/length(a)


fx <- function(x){
	sqrt(3.4^2 + 2.6^2 - 2* x) + (1.7)/qnorm(0.2, 0, 1)
}


plot(y, type = "l")
optimize(fx, interval = c(-100, 100))



y <- fx(seq(-1, 1, 0.00001))

y <- fx()

y <- y[!is.na(y)]
x <- seq(-100, 100, 0.00001)
cov_min <- x[which(y == -min(abs(y)))]
cov <- fx(cov)

sigma <- matrix(c(3.4, cov, cov, 2.6), nrow =2)

y <- mvrnorm(n = 100000, mu=c(7.5, 5.8), Sigma = sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)


sum(y[,1] > y[,2])/nrow(y)


plot(y, type = "l")
optimize(fx, interval = c(-100, 100))



?optim





