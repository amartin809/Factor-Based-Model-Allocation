source("data_process.R")
source("performance_analysis.R")

library("quantmod")
library("timeSeries")
library("data.table")
library("PerformanceAnalytics")

backtest <- function(etf_input, fff_input, start_date, end_date, lambda, beta){
  rf <- fff_input[, 4]
  etf_ret_bt <- cbind(etf_input) - rf
  fff_bt <- fff_input[, 0:3]
  lr_fit <- lm(as.matrix(etf_ret_bt) ~ as.matrix(fff_bt))
  lr_summ <- summary(lr_fit)
  se <- as.numeric(sapply(lr_summ, FUN = "[", n = 6))^2
  bv <- lr_fit$coefficients[-1,]
  omega_fff <- cov(fff_bt)
  d <- diag(se)
  q <- t(bv) %*% omega_fff %*% bv + d
  
  n <- ncol(etf_ret_bt)
  Dmat <- q
  dvec <- matrix(0, nrow=n, ncol=1)
  aeq <- matrix(rep(1, n), n, ncol=1)
  Amat <- cbind(aeq, colMeans(bv), diag(n), -diag(n))
  bvec <- c(1, lambda*beta, rep(-2, n), rep(-2, n))
  
  opt <- solve.QP(Dmat, dvec, Amat, bvec, meq=1, factorized=FALSE)
  opt_sol <- opt$solution
  print(opt_sol)
  
  ticker_list <- c("FXE", "EWJ", "GLD", "QQQ", "SPY", "SHV", "DBA", "USO", "XBI", "ILF", "EPP", "FEZ")
  table_temp <- NULL
  etf_prices <- NULL
  for(i in 1:length(ticker_list)){
    table_temp <- getSymbols(ticker_list[i], from = start_date, to = end_date, auto.assign = F)
    etf_prices <- cbind(etf_prices, table_temp[,6])
  }
  colnames(etf_prices) <- ticker_list
  
  weighted_price <- (opt_sol[1]*etf_prices[, 1] + opt_sol[2]*etf_prices[, 2] + 
                       opt_sol[3]*etf_prices[, 3] + opt_sol[4]*etf_prices[, 4] +
                       opt_sol[5]*etf_prices[, 5] + opt_sol[6]*etf_prices[, 6] +
                       opt_sol[7]*etf_prices[, 7] + opt_sol[8]*etf_prices[, 8] +
                       opt_sol[9]*etf_prices[, 9] + opt_sol[10]*etf_prices[, 10] +
                       opt_sol[11]*etf_prices[, 11] + opt_sol[12]*etf_prices[, 12])
  
  colnames(weighted_price) <- "Weighted Price"
  print(weighted_price)
  print(plot(weighted_price, main = paste0("Cumulated Daily PnL with Beta = ", beta)))
  
  spy <- getSymbols("SPY", from = start_date, to = end_date, auto.assign = F)
  print(plot(cbind(weighted_price, spy[, 6]), main = paste0("Cumulated Daily PnL: Portfolio (Black) Compared to SPY (Red) with Beta = ", beta)))
  
  weighted_return <- (opt_sol[1]*etf_input[1] + opt_sol[2]*etf_input[2] + 
                       opt_sol[3]*etf_input[3] + opt_sol[4]*etf_input[4] +
                       opt_sol[5]*etf_input[5] + opt_sol[6]*etf_input[6] +
                       opt_sol[7]*etf_input[7] + opt_sol[8]*etf_input[8] +
                       opt_sol[9]*etf_input[9] + opt_sol[10]*etf_input[10] +
                       opt_sol[11]*etf_input[11] + opt_sol[12]*etf_input[12])
  
  colnames(weighted_return) <- "Weighted Return"
  print(weighted_return)
  
  weighted_return <- as.xts(weighted_return)
  print(hist(weighted_return, main = paste0("Histogram of Daily Returns with Beta = ", beta)))
  port_perf <- perf_analyze(weighted_return))
  
  colnames(port_perf) <- c(paste0("Beta = ", beta))
  
  return(port_perf)
}

# Before crisis:
spy_bc <- getSymbols("SPY", from = '2007-03-01', to = '2007-11-30', auto.assign = F)
spy_ret_bc <- dailyReturn(spy_bc, type = "arithmetic")
spy_perf_bc <- perf_analyze(spy_ret_bc)
colnames(spy_perf_bc) <- c("SPY")

bc_negone <- backtest(etf_bc, fff_bc, '2007-03-01', '2007-11-30', 60, 90, .01, -1)
bc_zero <- backtest(etf_bc, fff_bc, '2007-03-01', '2007-11-30', 60, 90, .01, 0)
bc_one <- backtest(etf_bc, fff_bc, '2007-03-01', '2007-11-30', 60, 90, .01, 1)

bc_performance <- cbind(bc_negone, bc_zero, bc_one, spy_perf_bc)
print(bc_performance)

# During crisis:
spy_dc <- getSymbols("SPY", from = '2007-12-01', to = '2009-06-30', auto.assign = F)
spy_ret_dc <- dailyReturn(spy_dc, type = "arithmetic")
spy_perf_dc <- perf_analyze(spy_ret_dc)
colnames(spy_perf_dc) <- c("SPY")

dc_negone <- backtest(etf_dc, fff_dc, '2007-12-01', '2009-06-30', .01, -1)
dc_zero <- backtest(etf_dc, fff_dc, '2007-12-01', '2009-06-30', .01, 0)
dc_one <- backtest(etf_dc, fff_dc, '2007-12-01', '2009-06-30', .01, 1)

dc_performance <- cbind(dc_negone, dc_zero, dc_one, spy_perf_dc)
print(dc_performance)

# After crisis:
spy_ac <- getSymbols("SPY", from = '2009-07-01', to = '2019-11-29', auto.assign = F)
spy_ret_ac <- dailyReturn(spy_ac, type = "arithmetic")
spy_perf_ac <- perf_analyze(spy_ret_ac)
colnames(spy_perf_ac) <- c("SPY")

ac_negone <- backtest(etf_ac, fff_ac, '2009-07-01', '2019-11-29', .01, -1)
ac_zero <- backtest(etf_ac, fff_ac, '2009-07-01', '2019-11-29', .01, 0)
ac_one <- backtest(etf_ac, fff_ac, '2009-07-01', '2019-11-29', .01, 1)

ac_performance <- cbind(ac_negone, ac_zero, ac_one, spy_perf_ac)
print(ac_performance)

# Whole period:
spy_wp <- getSymbols("SPY", from = '2007-03-01', to = '2019-11-29', auto.assign = F)
spy_ret_wp <- dailyReturn(spy_wp, type = "arithmetic")
spy_perf_wp <- perf_analyze(spy_ret_wp)
colnames(spy_perf_wp) <- c("SPY")

wp_negone <- backtest(etf_wp, fff_wp, '2007-03-01', '2019-11-29', .01, -1)
wp_zero <- backtest(etf_wp, fff_wp, '2007-03-01', '2019-11-29', .01, 0)
wp_one <- backtest(etf_wp, fff_wp, '2007-03-01', '2019-11-29', .01, 1)

wp_performance <- cbind(wp_negone, wp_zero, wp_one, spy_perf_wp)
print(wp_performance)
