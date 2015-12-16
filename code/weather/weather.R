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
get_weather <- function(city = 'BCN', description = NULL){
  if(is.null(description)){
    description <- city
  }
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
    weather$date <- as.Date(weather$Date)
    weather$Date <- NULL
    weather$city <- description
    
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
get_weather('BCN', description = 'Barcelona')
get_weather('FCO', description = 'Rome') # ROME
get_weather('CDG', description = 'Paris') # PARIS
# get_weather('PAR') # PARIS
get_weather('JFK', description = 'New York') #NYC
get_weather('MAD', description = 'Madrid')
get_weather('GRX', description = 'Granada')
get_weather('LPA', description = 'Gran Canarias') # GRAN CANARIAS
get_weather('ATH', description = 'Athens') # ATHENS

##### COMBINE ALL WEATHER TOGETHER
dfs <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
results_list <- list()
for (i in 1:length(dfs)){
  temp <-  get(dfs[i])
  names(temp)[1] <- 'x'
  results_list[[i]] <- temp
  rm(list = dfs[i])
}
weather <- do.call('rbind', results_list)
rm(results_list, temp)

##### PLOT ALL YEARS
temp <- 
  weather %>%
  filter(city %in% c(#'Athens', 
                     'Barcelona',
                     'Granada', 'Madrid'#, 
                     #'New York', 'Paris',
                     #'Rome'
                     ),
         Max_TemperatureF <= 120,
         Min_TemperatureF >= -20,
         year >= 2013)

ggplot(data = temp) +
  geom_line(aes(x = date, 
                y = Max_TemperatureF),
            col = 'red', 
            alpha = 0.5) +
  geom_line(aes(x = date, 
                y = Min_TemperatureF),
            col = 'blue',
            alpha = 0.5) +
  xlab('Year') +
  ylab('Daily maximum and minimum temperatures') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ city)


##### PLOT TYPICAL YEAR
temp <- 
  weather %>%
  filter(city %in% c('Athens', 'Barcelona',
                     'Granada', 'Madrid', 
                     'New York', 'Paris',
                     'Rome'),
         Max_TemperatureF <= 120,
         Min_TemperatureF >= -20) %>%
  group_by(city, month_day) %>%
  summarise(max_avg = mean(Max_TemperatureC),
            min_avg = mean(Min_TemperatureC))
# Make a labelling vector
temp$label_vec <- 
  ifelse(substr(temp$month_day, 3, 5) == '-01',
         substr(temp$month_day, 1, 2),
         NA)

# Make a plotting function
typical_year <- function(language = 'en'){
  if(language == 'es'){
    xx <- 'Año típico (enero-diciembre)'
    yy <- 'Temperatura alta y baja cotidiana'
  }
  if(language == 'ca'){
    xx <- 'Any típic (gener-desembre)'
    yy <- 'Temperatura alta y baixa cotidiana'
  }
  if(language == 'en'){
    xx <- 'Typical year'
    yy <- 'Daily high and low temperatures'
  }
  x = ggplot(data = temp) +
    geom_ribbon(aes(x = month_day,
                    ymax = max_avg,
                    ymin = min_avg,
                    group = 1),
                fill = 'grey') +
    geom_line(aes(x = month_day,
                  y = max_avg,
                  group = 1),
              color = 'red') +
    geom_line(aes(x = month_day,
                  y = min_avg,
                  group = 1),
              color = 'blue') +
    theme_bw() +
    xlab(xx) +
    ylab(yy) +
    theme(axis.text.x = element_blank()) + # element_text(angle = 90, hjust = 1, size = 4)) +
    # scale_x_discrete(aes(labels = label_vec)) + 
    #   geom_text(data = temp, aes(x = temp$month_day, y = min(min_avg, na.rm = TRUE)* 0.8, 
    #                              label = temp$label_vec),
    #             size = 2) +
    facet_grid(. ~ city) +
    bcn_data_theme +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  print(x)
}

# Plot and save
typical_year(language = 'en')
ggsave(paste0('../bcndata.github.io/img/2015-12-07-Weather/typical_year_en.JPG'))
typical_year(language = 'es')
ggsave(paste0('../bcndata.github.io/img/2015-12-07-Weather/typical_year_es.JPG'))
typical_year(language = 'ca')
ggsave(paste0('../bcndata.github.io/img/2015-12-07-Weather/typical_year_ca.JPG'))

##### RAIN



##### AVERAGE WEATHER BY MONTH
temp <- weather %>%
  filter(city == 'Barcelona') %>%
  group_by(month) %>%
  summarise(c010 = quantile(Mean_TemperatureC, 0.10, na.rm = TRUE),
            c020 = quantile(Mean_TemperatureC, 0.20, na.rm = TRUE),
            c030 = quantile(Mean_TemperatureC, 0.30, na.rm = TRUE),
            c040 = quantile(Mean_TemperatureC, 0.40, na.rm = TRUE),
            c050 = quantile(Mean_TemperatureC, 0.50, na.rm = TRUE),
            c060 = quantile(Mean_TemperatureC, 0.60, na.rm = TRUE),
            c070 = quantile(Mean_TemperatureC, 0.70, na.rm = TRUE),
            c080 = quantile(Mean_TemperatureC, 0.80, na.rm = TRUE),
            c090 = quantile(Mean_TemperatureC, 0.90, na.rm = TRUE))

ggplot(data = temp, 
       aes(x = month, y = c050)) +
  geom_ribbon(aes(ymax = c090, ymin = c010), alpha = 0.2) +
  geom_ribbon(aes(ymax = c080, ymin = c020), alpha = 0.2) +
  geom_ribbon(aes(ymax = c070, ymin = c030), alpha = 0.2) +
  geom_ribbon(aes(ymax = c060, ymin = c040), alpha = 0.2)



