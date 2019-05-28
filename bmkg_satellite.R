library(readxl)
library(hydroGOF)
library(dplyr)

rain_data <- read_excel('/run/media/erikaris/DATA/mb_helda/bmkg_satellite_sample.xlsx')

compute_param <- function(data1, data2){
  rain_me <- me(data1, data2, na.rm = TRUE)
  
  avg_data1 <- mean(data1, na.rm = TRUE)
  
  avg_data2 <- mean(data2, na.rm = TRUE)
  
  data_bias <- rain_me/avg_data1*100
  
  rain_mae <- mae(data1, data2, na.rm = TRUE)
  
  rain_rmse <- rmse(data1, data2, na.rm = TRUE)
  
  rain_nrmse <- rain_rmse/avg_data1
  
  rain_nmae <- rain_mae/avg_data1
  
  return(c(rain_me, avg_data1, avg_data2, data_bias, rain_mae, rain_rmse, rain_nrmse, rain_nmae))
}

pairwise_rain <- function(data) {
  data1 <- data %>%
    select(starts_with('B'))
  
  data2 <- data %>%
    select(-Year, -Month) %>%
    select(matches("^[0-9]")) 
  
  # compute_param(data1, data2)
  
  as.data.frame(mapply(compute_param, data1, data2))
}

pair_result <- pairwise_rain(rain_data)

.rowNamesDF(pair_result, make.names=TRUE) <- c('me', 'avg_bmkg', 'bias', 'mae', 'rmse', 'nrmse', 'nmae')

pair_result