#' Almond Yield Anomaly Model
#' 
#' This function models almond yield anomaly in response to climate
#' @param clim_data is the numeric data frame of climate data,
#' @param yr is the time series year to pull the temp and precipitation data,
#' @return almond yield (ton acre^-1)
#'
# function definition
almond_modelR = function(clim_data, tmincoeff1 = 0.015, tmincoeff2 = 0.0046, pcoeff1 = 0.07, pcoeff2 = 0.0043) {
  
  clim_data <- clim_data %>% 
    group_by(month, year) %>% 
    summarize(temp_min = min(tmin_c),
              total_precip = sum(precip))
  
  feb_temp <- clim_data %>% 
    filter(month == 2) %>% 
    select(temp_min)
  
  jan_precip <- clim_data %>% 
    select(total_precip) %>% 
    filter(month == 1)
  
 yield = (tmincoeff1*feb_temp$temp_min - tmincoeff2*feb_temp$temp_min^2 - pcoeff1*jan_precip$total_precip + pcoeff2*jan_precip$total_precip^2 + 0.28)
  
 # max_yield = (-0.015*min(feb_temp$temp_min)) - (0.0046*min(feb_temp$temp_min)^2) - (0.07*jan_precip) + (0.0043*jan_precip^2) + 0.28
 # min_yield = (-0.015*max(feb_temp$temp_min)) - (0.0046*max(feb_temp$temp_min)^2) - (0.07*jan_precip) + (0.0043*jan_precip^2) + 0.28
 # mean_yield = (-0.015*mean(feb_temp$temp_min)) - (0.0046*mean(feb_temp$temp_min)^2) - (0.07*jan_precip) + (0.0043*jan_precip^2) + 0.28
 
  return(list(max(yield), min(yield), mean(yield)))
}

