# ##### Directories 
root <- '/home/joebrew/Documents/bcndata'
data_dir <- paste0(root, '/data/weather')
code_dir <- paste0(root, '/code')

##### Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(ggmap)
library(weatherData)
# require(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

##### Theme
source(paste0(root, '/lib/theme.R'))
source(paste0(root, '/lib/helpers.R'))

##### Get weather data
if('weather.csv' %in% dir(data_dir)){
  weather <- read_csv(paste0(data_dir, '/weather.csv'))
} else {
  this_year <- as.numeric(format(Sys.Date(), '%Y'))
  years <- 1996:this_year
  results_list <- list()
  for (y in 1:length(years)){
    weath <- getWeatherForDate(station_id = 'BCN',
                               start_date = paste0(years[y], '-01-01'),
                               end_date = ifelse(years[y] < this_year,
                                                 paste0(years[y], '-12-31'),
                                                 paste0(years[y], format(Sys.Date(), '-%m-%d'))),
                               opt_all_columns = TRUE)
    message(paste0('Done with ', years[y], '\n'))
    results_list[[y]] <- weath
  }
  weather <- do.call('rbind', results_list)
  write_csv(weather, paste0(data_dir, '/weather.csv'))
}


##### MAKE SOME NEW COLUMNS
weather$month <- as.numeric(format(weather$Date, '%m'))
weather$day <- as.numeric(format(weather$Date, '%d'))
weather$year <- as.numeric(format(weather$Date, '%Y'))
weather$month_day <- paste0(format(weather$Date, '%m-%d'))
weather$dow <- format(weather$Date, '%u')  #weekdays(weather$Date)
weather$week <- format(weather$Date, '%U')

##### GET CELCIUS TEMPERATURE COLUMNS
for (j in 1:ncol(weather)){
  if(grepl('F', names(weather)[j])){
    column_name <- names(weather)[j]
    new_val <- f2c(weather[,j])
    new_column_name <- gsub('F', 'C', column_name)
    weather[,new_column_name] <- new_val
  }
}

##### SOME LANGUAGE DICTS



##### AVERAGE WEATHER BY MONTH
# temp <- weather %>%
#   group_by(month) %>%
#   summarise(c010 = quantile(Mean_TemperatureC, 0.10, na.rm = TRUE),
#             c020 = quantile(Mean_TemperatureC, 0.20, na.rm = TRUE),
#             c030 = quantile(Mean_TemperatureC, 0.30, na.rm = TRUE),
#             c040 = quantile(Mean_TemperatureC, 0.40, na.rm = TRUE),
#             c050 = quantile(Mean_TemperatureC, 0.50, na.rm = TRUE),
#             c060 = quantile(Mean_TemperatureC, 0.60, na.rm = TRUE),
#             c070 = quantile(Mean_TemperatureC, 0.70, na.rm = TRUE),
#             c080 = quantile(Mean_TemperatureC, 0.80, na.rm = TRUE),
#             c090 = quantile(Mean_TemperatureC, 0.90, na.rm = TRUE))
# 
# ggplot(data = temp, 
#        aes(x = month, y = c050)) +
#   geom_ribbon(aes(ymax = c090, ymin = c010), alpha = 0.2) +
#   geom_ribbon(aes(ymax = c080, ymin = c020), alpha = 0.2) +
#   geom_ribbon(aes(ymax = c070, ymin = c030), alpha = 0.2) +
#   geom_ribbon(aes(ymax = c060, ymin = c040), alpha = 0.2)

##### TEMPERATURE OVER ALL TIME
ggplot(data = weather,
       aes(x = Date, y = Mean_TemperatureC)) +
  # geom_line() +
  geom_ribbon(aes(ymax = Max_TemperatureC,
                  ymin = Min_TemperatureC)) +
#   geom_line(aes(x = Date, y = Max_TemperatureC), color = 'red') +
#   geom_line(aes(x = Date, y = Min_TemperatureC), color = 'blue') +
  ylim(-5, 36)




