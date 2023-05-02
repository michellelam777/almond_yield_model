#' calc profit 
#' 
#' @param  baseline_yield (ton/acre) default is 1.02
#' @param  market_price ($/ton) default $5,000
#' @param  area (acres) default 10   
#' @param  calc_almond_yield_results (output from calc_almond_yield function)
#' @param  discount rate default 0.12
#' @return data frame with estimate of profit
#' 
#' @references 
#' baseline almond yield based on 2022 values from this resource
#' https://www.nass.usda.gov/Statistics_by_State/California/Publications/Specialty_and_Other_Releases/Almond/Forecast/202205almpd.pdf
#' 
calc_profit = function(baseline_yield = 1.02, market_price = 5000, area = 10, calc_almond_yield_results, discount = 0.12) {
  
  # calculate baseline profit
  baseline_profit = baseline_yield * market_price * area
  
  yield_df <- calc_almond_yield_results$yield
  
  # add column to yield_df that calculates profit over/under baseline for each year
  yield_df$anomaly_profit = yield_df$anomaly_value * market_price * area #returns dollars
  
  # add column to yield_df that calculates total profit (baseline + anomaly profit)
  yield_df$total_profit = yield_df$anomaly_profit + baseline_profit
  
  # define year variable for the time parameter in the calc_NPV function
  year = yield_df$year
  
  # add column to yield_df that calculates adjusted profit (taking into account NPV)
  # discount is passed through to this function
  yield_df$adjusted_profit = calc_NPV(value = yield_df$total_profit, time=year-year[1], discount=discount) 
    
  # return a list containing the year and adjusted_profit columns from yield_df and a single mean value for average adjusted_profit
  return(list(yield_df = yield_df[,c("year", "adjusted_profit")], mean=mean(yield_df$adjusted_profit)))
  
}
