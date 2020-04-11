library("quantmod")
library("timeSeries")
library("data.table")


ticker_list <- c("FXE", "EWJ", "GLD", "QQQ", "SPY", "SHV", "DBA", "USO", "XBI", "ILF", "EPP", "FEZ")
temp_return <- NULL
etf_return <- NULL
for(i in 1:length(ticker_list)){
  table_temp <- getSymbols(ticker_list[i], from = '2007-03-01', to = '2019-11-29', auto.assign = F)
  temp_return <- dailyReturn(table_temp, type = "arithmetic")
  etf_return <- cbind(etf_return, temp_return)
}
colnames(etf_return) <- ticker_list

etf_return <- as.data.frame(etf_return)
setDT(etf_return, keep.rownames="Date")
rownames(etf_return) <- etf_return$Date

fff <- read.csv("F-F_Research_Data_Factors_daily.csv") ## Ensure that your working directory contains this file
names(fff)[1] <- "Date"
fff$Date <- as.Date(as.character(fff$Date), format="%Y%m%d")
fff$Date <- as.character(fff$Date)
head(fff)

total_df <- merge(etf_return, fff, by="Date", all=FALSE)
total_df <- as.data.frame(total_df)
rownames(total_df) <- total_df$Date
total_df$Date <- NULL

# Note: We can modify the dates later

# Before crisis:
# From 3/1/2007 to 11/30/2007:
etf_bc <- total_df[, 1:12][rownames(total_df) >= "2007-03-01" & rownames(total_df) <= "2007-11-30", ]
fff_bc <- total_df[, 13:16][rownames(total_df) >= "2007-03-01" & rownames(total_df) <= "2007-11-30", ]

# During crisis:
# From 12/1/2007 To 6/30/2009:
etf_dc <- total_df[, 1:12][rownames(total_df) >= "2007-12-01" & rownames(total_df) <= "2009-06-30", ]
fff_dc <- total_df[, 13:16][rownames(total_df) >= "2007-12-01" & rownames(total_df) <= "2009-06-30", ]

# After crisis:
# From 7/1/2009 to end of data:
etf_ac <- total_df[, 1:12][rownames(total_df) >= "2009-07-01", ]
fff_ac <- total_df[, 13:16][rownames(total_df) >= "2009-07-01", ]

# Whole period:
etf_wp <- total_df[, 1:12]
fff_wp <- total_df[, 13:16]
