#' Almond Anomaly Yield Calculation
#'
#' @param clim_data climate data file in .txt format, contains daily temperatures (°C) and daily precipitation (mm)
#' @param Tmincoeff1 First Temperature Minimum Coefficient for February default -0.015
#' @param Tmincoeff2 Second Temperature Minimum Coefficient for February default -0.0046
#' @param Pcoeff1 Precipitation Coefficient for January default -0.07
#' @param Pcoeff2 Precipitation Coefficient for January default 0.0043
#' @param intercept Intercept default 0.28
#' @return maximum, minimum, and mean almond anomaly yield (ton/acre)
#' @examples 
#' almond_model("/Assignments/assignment2/almond_yield_model/clim.txt")
#' @references 
#' https://www.sciencedirect.com/science/article/pii/S016819230600308X

calc_almond_yield <- function(clim_data, Tmincoeff1 = -0.015, Tmincoeff2 = -0.0046, Pcoeff1 = -0.07, Pcoeff2 = 0.0043, intercept = 0.28) {
  
  # read in climate data
  clim_df <- read.table(clim_data, header = TRUE)
  
  # calculate min temp for February and precip sum for January
  yearly_tmin_feb <- clim_df |>
    group_by(year, month) |>
    summarize(min_temp_2 = min(tmin_c)) |>
    filter(month == 2) |>
    select(-month)
  
  yearly_precip_jan <- clim_df |>
    group_by(year, month) |>
    summarize(precip_sum_1 = sum(precip)) |>
    filter(month == 1) |>
    select(-month)
  
  # join the two dataframes
  tmin_precip_df <- left_join(yearly_tmin_feb, yearly_precip_jan)
  
  # calculate the anomaly values
  anomaly_value <- Tmincoeff1 * tmin_precip_df$min_temp_2 + Tmincoeff2 * (tmin_precip_df$min_temp_2**2) + Pcoeff1 * tmin_precip_df$precip_sum_1 + Pcoeff2 * (tmin_precip_df$precip_sum_1**2) + intercept
  
  #add anomaly values to tmin_precip_df
  yield_df <- tmin_precip_df |>
    cbind(anomaly_value = anomaly_value) 

  # return a list containing the anomaly vector, minimum value, maximum value, and mean value
  
  min_val = min(yield_df$anomaly_value)
  max_val = max(yield_df$anomaly_value)
  mean_val = mean(yield_df$anomaly_value)
  
  results_df <- data.frame(min_val, max_val, mean_val)
  
  return(list(results = results_df, yield = yield_df))
  
}

