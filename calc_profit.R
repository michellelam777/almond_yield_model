#' calc profit 
#' @param  market_price ($/ton) (default $5,000)
#' @param  year (when was anomaly obtained)
#' @param  discount rate (default 0.12)
#' @param  area (acres) (default 10)   
#' @param  calc_almond_yield_results (yield list results)
#' @return data frame with estimate of profit
calc_profit = function(market_price = 5000, discount=0.12, area = 10, calc_almond_yield_results) {
  
  # calculate baseline profit
  baseline_profit = market_price * area
  
  yield_df <- calc_almond_yield_results$yield
  
  # generate dataframe of yearly profit values
  # yield_df <- yield |>
  #   data.frame(year=year, anomaly_value=anomaly_value)
  yield_df$anomaly_profit = yield_df$anomaly_value * market_price * area #returns dollars
  yield_df$total_profit = yield_df$anomaly_profit + baseline_profit
  
  # discount is passed through to this function
  year = yield_df$year
  yield_df$adjusted_profit = calc_NPV(value = yield_df$total_profit, time=year-year[1], discount=discount) 
    
    # yield_df$total_profit / (1 + discount)**calc_almond_yield_results$year-calc_almond_yield_results$year[1]
  return(list(yield_df = yield_df[,c("year", "adjusted_profit")], mean=mean(yield_df$adjusted_profit)))
  
}
