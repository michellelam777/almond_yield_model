#' Almond Anomaly Yield Calculation
#'
#' @param file_path file_path to the location of climate data file in .txt format
#' @return maximum, minimum, and mean almond anomaly yield (ton/acre)
#' @examples 
#' almond_model("/Assignments/assignment2/almond_yield_model/clim.txt")
#' @references 
#' https://www.sciencedirect.com/science/article/pii/S016819230600308X

almond_model <- function(file_path) {
  
  # read in climate data
  clim_df <- read.table(file_path, header = TRUE)
  
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
  anomaly_list <- list()
  
  for (year in 1:nrow(tmin_precip_df)) {
    anomaly_value <- -0.015 * tmin_precip_df$min_temp_2[year] - 0.0046 * (tmin_precip_df$min_temp_2[year]**2) - 0.07 * tmin_precip_df$precip_sum_1[year] +0.0043 * (tmin_precip_df$precip_sum_1[year]**2) + 0.28
    anomaly_list[[year]] <- anomaly_value
  }
  
  anomaly_vect <- unlist(anomaly_list)
  
  # return a list containing the anomaly vector, minimum value, maximum value, and mean value
 min_val = min(anomaly_vect)
 max_val = max(anomaly_vect)
 mean_val = mean(anomaly_vect)
 
 results_df <- data.frame(min_val, max_val, mean_val)
 
 return(results_df)
 
}

