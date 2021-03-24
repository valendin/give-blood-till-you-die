# BTYDplus https://github.com/mplatzer/BTYDplus#readme
# installation :
# devtools::install_github("mplatzer/BTYDplus", dependencies=TRUE)

# import BTYD model library
library(BTYDplus)

# load input data
df = read.csv("blood.csv", header=TRUE, stringsAsFactors=FALSE)[, c("cust", "date")]

# convert date column to "date" data type
df$date <- as.Date(df$date)

# plot weekly transactions
weekly_inc_total <- elog2inc(df, by = 7, first = TRUE)
weekly_inc_repeat <- elog2inc(df, by = 7, first = FALSE)
plot(weekly_inc_total, typ = "l", frame = FALSE, main = "Total & Incremental Donations History")
lines(weekly_inc_repeat, col = "red")

# plot individual customer histories
plotTimingPatterns(df, n = 20, T.cal = train_end,
                   headers = c("Calibration", "Holdout"), title = "")

# check the range of dates
range(df$date)

# set the calibration and holdout periods
train_start <- as.Date("2015-01-01")
train_end <- as.Date("2017-12-31")
hold_start <- as.Date("2018-01-01")
hold_end <- as.Date("2018-12-31")

# calculate recency/frequency statistics per customer:
cbs <- elog2cbs(df, T.cal = train_end, T.tot = hold_end)

# cbs - customer-by-sufficient (statistic) table structure:
# x - number of repeat transactions made during calibration period (FREQUENCY)
# t.x - time of the very last calibration period transaction (RECENCY)
# litt (log-inter-transaction-time) - sum over the logs of inter-transaction times (REGULARITY)
# first - first transaction date 
# T.cal - customer calibration lifetime (time between the first and end of calibration) 
# T.star - holdout period length 
# x.star - holdout period transaction sum

# estimate Pareto/NBD model parameters (takes 30 seconds on my laptop)
parameters_pnbd <- pnbd.mcmc.DrawParameters(cbs)

# generate point estimates for the next 52 weeks: this takes maybe 5 minutes
y = as.integer(0)
for (date in seq(1, 52, by=1)) {
  pnbd.xstar.draws <- mcmc.DrawFutureTransactions(cbs, parameters_pnbd, T.star = date)
  cbs[, as.character(y)]  <- apply(pnbd.xstar.draws, 2, mean)
  y <- as.integer(y+1)
}

# calculate metrics 
# (ncol(cbs) refers to the last column in the table) 
E <- cbs$x.star-cbs[,ncol(cbs)]
SE <- E^2
MSE <- mean(SE)
RMSE <- MSE ^(1/2)
predicted_total <- sum(cbs[, ncol(cbs)])
actual_total <- sum(cbs$x.star)
bias <- 100 * (predicted_total - actual_total) / actual_total
paste("P/NBD RMSE:", RMSE, "bias:", bias, "%")

# display aggregate prediction
aggregate_prediction <- colSums(cbs[, seq(9, 9+51)])
for (i in seq(52, 2, by=-1)) {
  aggregate_prediction[i] <- aggregate_prediction[i] - aggregate_prediction[i-1]
}
offset <- length(weekly_inc_total) - length(aggregate_prediction)
aggregate_prediction <- c(rep(NA, offset), aggregate_prediction)
plot(weekly_inc_total[], typ = "l", frame = FALSE, main = "Total Donations with P/NBD prediction")
lines(aggregate_prediction, col = "magenta")

# the magenta line isn't straight because of the sampling noise


# regularity parameter: a return value of close to 1 supports the assumption of exponentially 
# distributed intertransaction times, whereas values significantly larger than 1 reveal 
# the presence of regularity.
estimateRegularity(df, method = "wheat", plot = FALSE, title = "Wheat & Morrison")


# Improving the prediction by accounting for regularity: the Pareto/GGG model

# discard the Pareto/NBD estimated values
cbs_pggg <- cbs[, c("cust", "x", "t.x", "litt", "first", "T.cal", "T.star", "x.star")]

# estimate the Pareto/GGG model (takes a few minutes)
parameters_pggg <- pggg.mcmc.DrawParameters(cbs_pggg)

# generate point estimates (takes about 10 mins)
y = as.integer(0)
for (date in seq(1, 52, by=1)) {
 pnbd.xstar.draws <- mcmc.DrawFutureTransactions(cbs_pggg, parameters_pggg, T.star = date)
 cbs_pggg[, as.character(y)]  <- apply(pnbd.xstar.draws, 2, mean)
 y <- as.integer(y+1)
}

# calculate the error
E <- cbs_pggg$x.star-cbs_pggg[,ncol(cbs)]
SE <- E^2
MSE <- mean(SE)
RMSE <- MSE ^(1/2)

predicted_total <- sum(cbs_pggg[,ncol(cbs)])
actual_total <- sum(cbs_pggg$x.star)
bias <- 100 * (predicted_total - actual_total) / actual_total

paste("P/GGG RMSE:", RMSE, "bias:", bias, "%")

# plot the results
aggregate_prediction_2 <- colSums(cbs_pggg[, seq(9, 9+51)])
for (i in seq(52, 2, by=-1)) {
  aggregate_prediction_2[i] <- aggregate_prediction_2[i] - aggregate_prediction_2[i-1]
}
offset <- length(weekly_inc_total) - length(aggregate_prediction_2)
aggregate_prediction_2 <- c(rep(NA, offset), aggregate_prediction_2)
plot(weekly_inc_total, typ = "l", frame = FALSE, main = "Total Donations with P/NBD and P/GGG predictions")
lines(aggregate_prediction_2, col = "magenta")
lines(aggregate_prediction, col = "magenta")

