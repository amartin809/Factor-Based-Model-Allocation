library("PerformanceAnalytics")


perf_analyze <- function(port_ret){
  perf_table <- NULL
  ret_cum <- Return.cumulative(port_ret, geometric=FALSE)*250
  perf_table <- c(perf_table, ret_cum)
  mean_arith <- mean(port_ret)*250
  perf_table <- c(perf_table, mean_arith)
  mean_geom <- mean.geometric(port_ret)*250
  perf_table <- c(perf_table, mean_geom)
  min_ret <- min(port_ret)*250
  perf_table <- c(perf_table, min_ret)
  max_10_dd <- min(findDrawdowns(port_ret)$return)*250
  perf_table <- c(perf_table, max_10_dd)
  vol <- StdDev(port_ret)*250
  perf_table <- c(perf_table, vol)
  sr <- SharpeRatio(port_ret)[1]
  perf_table <- c(perf_table, sr)
  skew <- skewness(port_ret)
  perf_table <- c(perf_table, skew)
  kurt <- kurtosis(port_ret)
  perf_table <- c(perf_table, kurt)
  mod_var <- VaR(port_ret, p=.95, method="modified")*250
  perf_table <- c(perf_table, mod_var)
  mod_cvar <- ETL(port_ret, p=.95, method="modified")*250
  perf_table <- c(perf_table, mod_cvar)
  perf_table <- data.frame(perf_table)
  rownames(perf_table) <- c("Cumulated Return", "Daily Mean Arithmetic",
                            "Daily Mean Geometric", "Daily Min Return",
                            "Max 10 days drawdown", "Volatility",
                            "Sharpe Ratio", "Skewness",
                            "Kurtosis", "Modified VaR",
                            "Modified CVaR")
  return(perf_table)
}
