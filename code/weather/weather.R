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

# Define function for getting city's weather since 1996
get_weather <- function(city = 'BCN'){
  file_name <- paste0('weather_', tolower(city), '.csv')
  if(file_name %in% dir(data_dir)){
    assign(paste0('weather_', tolower(city)),
           read_csv(paste0(data_dir, '/', file_name)),
           envir = .GlobalEnv)
  } else {
    this_year <- as.numeric(format(Sys.Date(), '%Y'))
    years <- 1996:this_year
    results_list <- list()
    for (y in 1:length(years)){
      try({
        attempt <- 0
        weath <- getWeatherForDate(station_id = city,
                                   start_date = paste0(years[y], '-01-01'),
                                   end_date = ifelse(years[y] < this_year,
                                                     paste0(years[y], '-12-31'),
                                                     paste0(years[y], format(Sys.Date() - 1, '-%m-%d'))),
                                   opt_all_columns = TRUE)
        message(paste0('Done with ', years[y], '\n'))
        while((is.null(weath)  & years[y] != this_year) & attempt < 5){
          message(paste0('Attempt number: ', attempt + 1))
          attempt <- attempt + 1
          weath <- getWeatherForDate(station_id = city,
                                     start_date = paste0(years[y], '-01-01'),
                                     end_date = ifelse(years[y] < this_year,
                                                       paste0(years[y], '-12-31'),
                                                       paste0(years[y], format(Sys.Date() - 1, '-%m-%d'))),
                                     opt_all_columns = TRUE)
          message(paste0('Done with ', years[y], '\n'))
          Sys.sleep(attempt * 0.2)
        }
        results_list[[y]] <- weath
        rm(weath)
      }
      )}
    weather <- do.call('rbind', results_list)
    
    # MAKE SOME NEW COLUMNS
    weather$month <- as.numeric(format(weather$Date, '%m'))
    weather$day <- as.numeric(format(weather$Date, '%d'))
    weather$year <- as.numeric(format(weather$Date, '%Y'))
    weather$month_day <- paste0(format(weather$Date, '%m-%d'))
    weather$dow <- format(weather$Date, '%u')  #weekdays(weather$Date)
    weather$week <- format(weather$Date, '%U')
    
    # GET CELCIUS TEMPERATURE COLUMNS
    for (j in 1:ncol(weather)){
      if(grepl('F', names(weather)[j])){
        column_name <- names(weather)[j]
        new_val <- f2c(weather[,j])
        new_column_name <- gsub('F', 'C', column_name)
        weather[,new_column_name] <- new_val
      }
    }
    # Save a record
    write_csv(weather, paste0(data_dir, '/', file_name))
    # Assign name in global environment
    assign(paste0('weather_', tolower(city)),
           weather,
           envir = .GlobalEnv)
  }
}

# Get weather for different locations
get_weather('BCN')
get_weather('FCO') # ROME
get_weather('CDG') # PARIS
get_weather('PAR') # PARIS
get_weather('JFK') #NYC
get_weather('MAD')
get_weather('GRX')
get_weather('LPA') # GRAN CANARIAS
get_weather('ATH') # ATHENS

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




