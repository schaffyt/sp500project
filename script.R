#PART 1: "Can I forecast future stock direction based on past closing price?"
rm(list = ls())
setwd("C:/Users/adams/Documents/MATH 370/sp500")
load("symbols3.rda")
load("stocks_list.rda")
library(forecast)
library(ggplot2)
library(quantmod)
library(BatchGetSymbols)


########################## PART 2: Import Data Sources into R ###################################
current_stocks <- read.csv("sp-500-index-12-02-2018.csv", header =T)
#https://www.barchart.com/stocks/indices/sp/sp500?viewName=main

#df.SP500 <- GetSP500Stocks()#from wiki   ### does not work!!! used dataset above
symbols1 <- current_stocks$Symbol
symbols2 <- as.vector(symbols1)[-504] #last one is not a ticker
symbols3 <- gsub(".", replacement = "-", x = symbols2, fixed = T)   #BRK.B -> BRK-B    BF.B -> BF-B 
save(symbols3, file = "symbols3.rda")
sp.df <- getSymbols(Symbols = symbols3, from = "2016-10-08", to = "2018-11-30")#Missing values in "BHF", "DXC", "JEF", "LIN"
sp.index.df <- getSymbols(Symbols = "^GSPC", from = "2016-10-08", to = "2018-11-30")
########################## PART 3: Clean and Combine Data ###################################
# sp_batch <- BatchGetSymbols(tickers = symbols3, first.date = "2016-10-08", last.date = "2018-11-30")
symbols3 <- c(symbols3, sp.index.df)
stocks_list <- lapply(symbols3, function(x) get(x))
names(stocks_list) <- symbols3

#EVRG didn't emerge as an error until checking lengths
clean_tickers <- symbols3[-which(symbols3 %in% dirty_tickers)]
stocks_list_clean <- stocks_list[clean_tickers]
v <- lapply(stocks_list_clean, function(x) index(x)) 
which(!sapply(v, function(x) identical(v[[1]],x)))#checks that all stocks in cleanstocks are same
v1 =  as.character(v[[1]])
dimnames2 = list(v1,c("open", "high", "low", "close", "volume", "adjusted"), clean_tickers)
#s is a 3D array with dim = (540,6,499) = (num days, num variables, num stocks)

########################## PART 4: Build Time-Series###################################
s <- array(0, dim = c(dim(stocks_list_clean[[1]]),length(stocks_list_clean)), dimnames = dimnames2)
for(i in 1:length(stocks_list_clean)){
  s[,,i] = as.matrix(stocks_list_clean[[i]])
}
save(s, file = "s.rda")



#############################################################################################
############################# start scripting here ##########################################
#############################################################################################

rm(list = ls())
setwd("C:/Users/adams/Documents/MATH 370/sp500")
library(forecast)
library(ggplot2)
library(quantmod)
library(BatchGetSymbols)
load("s.rda")

window_length = 20
n = dim(s)[1] - window_length # n = number of times the window will go over a ts (number of training sets)
#4 = price.close
L <- lapply(1:(dim(s)[3]), function(i) s[,4,i])
names(L) = dimnames(s)[[3]]
L2 <- lapply(L, function(x) lapply(1:n, function(i) x[(0:(window_length-1))+i]))
names(L2) = dimnames(s)[[3]]

create_arima_list <- function(stock_windows){
  lapply(stock_windows, function(x) auto.arima(ts(x)))
}
get_pdq_matrix <- function(arima_list){
  t(sapply(arima_list, function(x) arimaorder(x)))
}
get_forecast_list <- function(arima_list){
  sapply(arima_list, function(x) forecast(x)$mean[1])
}

arima_lists = list() # a list containing the ARIMA models of every observed window for each stock
names(arima_lists) = dimnames(s)[[3]]
pdq_matrices = list()# a list containing the p,d,q values of every observed window for each stock
prediction_matrices = list()# a list containing the predicted_values actual_values previous_values
                            #of every observed window for each stock
good_percentages = c()# a vector of the % of non (0,1,0) models for each stock 

########################## PART 5: Train Predictive Model ###################################
# this will take forever, it only needs to be run once!
# for(i in 1:(dim(s)[3])){
#   currstock <- lapply(1:n, function(x) s[(0:(window_length-1))+x,4,i])
#   
#   arima_lists[[i]] <- create_arima_list(currstock)
#   
#   print(paste("stock #",i, "complete"))
# }
# save(arima_lists, file = "arima_lists.rda")
load("arima_lists.rda")
for(i in 1:(dim(s)[3])){
  pdq_matrices[[i]] <- get_pdq_matrix(arima_lists[[i]])
  num_good_arimas <- dim(pdq_matrices[[i]][(pdq_matrices[[i]][,1] != 0) | (pdq_matrices[[i]][,3] != 0), ])[1]
  good_percentages[i] <- num_good_arimas/n
  print(paste("stock #",i, "complete"))
}

#this function is copy/pasted directly from stack overflow: gets indices of 5 largest in an array
whichpart <- function(x, n=30) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}

range(good_percentages)
good_percentages[499]
good_indices <- whichpart(good_percentages, 5)
good_stocks <- s[,4,good_indices]
good_arima_lists <- list()
names(good_arima_lists) = colnames(good_stocks)

#fill my prediction matrices for top 5 stocks
for(i in 1:(dim(good_stocks)[2])){
  currstock <- lapply(1:n, function(x) good_stocks[(0:(window_length-1))+x,i])
  good_arima_lists[[i]] <- create_arima_list(stock_windows = currstock)
  forecast_mean <- get_forecast_list(good_arima_lists[[i]])
  actual_values <- good_stocks[(window_length+1):dim(good_stocks)[1],i]
  previous_values <- good_stocks[(window_length):(dim(good_stocks)[1] - 1),i]
  forecast_upper_80 <- sapply(good_arima_lists[[i]], function(x) forecast(x)$upper[1])
  forecast_upper_95 <- sapply(good_arima_lists[[i]], function(x) forecast(x)$upper[1,2])
  forecast_lower_80 <- sapply(good_arima_lists[[i]], function(x) forecast(x)$lower[1])
  forecast_lower_95 <- sapply(good_arima_lists[[i]], function(x) forecast(x)$lower[1,2])
  prediction_matrices[[i]] <- cbind(actual_values,
                                    previous_values, forecast_mean,forecast_lower_80,forecast_upper_80
                                    ,forecast_lower_95,forecast_upper_95)
}
names(prediction_matrices) = colnames(good_stocks)

########################## PART 6: Test Predictive Model###################################
getAccuracy <- function(predMatrix)
{
  accuracy_vector = c()
  for(i in 1:(dim(prediction_matrices[[1]])[1])){
    if((predMatrix[i,"actual_values"]-predMatrix[i,"previous_values"] >= 0)  & (predMatrix[i,"forecast_mean"]-predMatrix[i,"previous_values"] >= 0)){
      accuracy_vector[i] = 1
    }
    else if((predMatrix[i,"actual_values"]-predMatrix[i,"previous_values"] <= 0)  & (predMatrix[i,"forecast_mean"]-predMatrix[i,"previous_values"] <= 0)){
      accuracy_vector[i] = 1
    }
    else{
      accuracy_vector[i] = 0
    }
  }
  return(accuracy_vector)
}
accuracy_lists <- lapply(prediction_matrices, getAccuracy)

#calculate accuracy
accuracies <- sapply(accuracy_lists, mean)
names(accuracies) = colnames(good_stocks)
accuracies
cumulative_accuracy <- mean(accuracies)
cumulative_accuracy #this includes all ARIMA(0,1,0) models

########################## PART 7: Graphing ###################################
good_tickers <- colnames(good_stocks)
#"AES" = AES Corporation, energy 
#"JKHY" = Jack Henry & Associates, Inc., tech, finance
#"MAC" = Macerich, real estate investment trust
#"NEE"  = NextEra Energy
#"ULTA" = Ulta Beauty
custom_plot <- function(ticker)
{
  plot_title <- paste(ticker, "vs Time")
  
  plot(ts(scale(prediction_matrices[[ticker]][,"actual_values"])), main = plot_title, 
       xlab = "trading days:", ylab = "price (scaled)")
  lines(ts(scale(prediction_matrices[[ticker]][,"forecast_upper_95"])), col = "Red")
  lines(ts(scale(prediction_matrices[[ticker]][,"forecast_lower_95"])), col = "Blue")
  lines(ts(scale(L[[ticker]][window_length:length(L[["GSPC"]])])), col = "Green")
  legend("bottomright", legend = c(ticker, "95% CI Upper Bound", "95% CI Lower Bound", "S&P500 Index"),
         col = c("Black", "Red", "Blue", "Green"), lty=1:2, cex=0.8,
         title="Line types", text.font=4, bg='lightblue')
}

#Choose your stock!
custom_plot("ULTA")

custom_plot2 <- function(ticker)
{
  plot_title <- paste("Forecasts for: ", ticker)
  arima2plot <- auto.arima(good_stocks[,ticker])
  future <- forecast(arima2plot)
  plot(future, main = plot_title, xlab = "trading days:",
       ylab = "price (USD)", col = "Dark Green", xlim = c(200, 550))
  par(bg = "Gray")
  legend("bottomleft", legend = c(ticker),
         col = c("Dark Green"), lty=1:2, cex=0.8,
         title="Line types", text.font=4, bg='lightblue')
}
custom_plot2("JKHY")


#forecast package function: auto.arima, box.test on residuals
# tsDiff <- diff(sp500_training)
# plot_time_series(tsDiff, 'First Difference')
#lines() instead of just grabbing forecast$mean, grab whole row, use lines on row, add to prediction_matrix