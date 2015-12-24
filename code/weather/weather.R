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
source(paste0(root, '/lib/dicts.R'))


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
get_weather('AGP', description = 'Málaga') # ATHENS
get_weather('BIO', description = 'Bilbao') # ATHENS
get_weather('ORD', description = 'Chicago') # ATHENS
get_weather('YYZ', description = 'Toronto') # ATHENS


##### COMBINE ALL WEATHER TOGETHER
dfs <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))
dfs <- dfs[which(grepl('weather_', dfs))]
results_list <- list()
for (i in 1:length(dfs)){
  temp <-  get(dfs[i])
  names(temp)[1] <- 'x'
  results_list[[i]] <- temp
  rm(list = dfs[i])
}
weather <- do.call('rbind', results_list)
rm(results_list, temp)


######## -------------------------------------------------------
######## -------------------------------------------------------
######## -------------------------------------------------------
######## -------------------------------------------------------
######## -------------------------------------------------------
######## -------------------------------------------------------
######## -------------------------------------------------------


##### PLOT ALL YEARS
spain_plot <- function(language = 'es'){
  if(language == 'es'){
    xx <- 'Fecha'
    yy <- 'Temperaturas cotidianas máximas y mínimas'
  }
  if(language == 'ca'){
    xx <- 'Data'
    yy <- 'Temperatures cotidianas màximes y mínimes'
  }
  if(language == 'en'){
    xx <- 'Date'
    yy <- 'Daily maximum and minimum temperatures'
  }
   temp <- 
   weather %>%
   filter(city %in% c(#'Athens', 
     'Barcelona',
     'Bilbao', 
     'Málaga', 
     'Granada', 'Madrid'#, 
     #'New York', 'Paris',
     #'Rome'
   ),
   Max_TemperatureF <= 120,
   Min_TemperatureF >= -20,
   year >= 2013)
 
   x <- 
     ggplot(data = temp) +
   geom_line(aes(x = date, 
                 y = Max_TemperatureC),
             col = 'red', 
             alpha = 0.5) +
   geom_line(aes(x = date, 
                 y = Min_TemperatureC),
             col = 'blue',
             alpha = 0.5) +
   xlab(xx) +
   ylab(yy) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   facet_grid(. ~ city)
 print(x)
}

spain_plot('en')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/spain_plot_en.JPG'))
spain_plot('es')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/spain_plot_es.JPG'))
spain_plot('ca')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/spain_plot_ca.JPG'))

##### PLOT TYPICAL YEAR

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
  
  temp <- 
    weather %>%
    filter(city %in% c('Athens', 'Barcelona',
                       # 'Granada', 
                       'Madrid', 
                       'Chicago',
                       'Toronto', 
                       'New York', 
                       'Paris',
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
  
  x = ggplot(data = temp) +
    geom_ribbon(aes(x = month_day,
                    ymax = max_avg,
                    ymin = min_avg,
                    group = 1),
                fill = 'gold') +
    geom_line(aes(x = month_day,
                  y = max_avg,
                  group = 1),
              color = 'grey') +
    geom_line(aes(x = month_day,
                  y = min_avg,
                  group = 1),
              color = 'grey') +
    theme_bw() +
    geom_hline(yintercept=c(0, 10, 20, 30), alpha = 0.2) +
    xlab(xx) +
    ylab(yy) +
    theme(axis.text.x = element_blank()) + # element_text(angle = 90, hjust = 1, size = 4)) +
    # scale_x_discrete(aes(labels = label_vec)) + 
    #   geom_text(data = temp, aes(x = temp$month_day, y = min(min_avg, na.rm = TRUE)* 0.8, 
    #                              label = temp$label_vec),
    #             size = 2) +
    facet_grid(. ~ city) +
    bcn_data_theme +
    theme(panel.background = element_rect(fill = NA, color = "black")) + 
    theme(panel.border = element_rect(fill = NA, colour = "black")) +
    theme(strip.background = element_rect(fill = 'white')) + 
    theme(strip.text.x = element_text(colour = 'black', size = 15)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())#, 
          # panel.background = element_blank(), axis.line = element_line(colour = "black"))
  print(x)
}

# Plot and save
typical_year(language = 'en')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/typical_year_en.JPG'))
typical_year(language = 'es')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/typical_year_es.JPG'))
typical_year(language = 'ca')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/typical_year_ca.JPG'))

##### DAY BY DAY IN BARCELONA

# First, need to make some additional language dicts
lab_dict <- data.frame(location = rep(c('x', 'y'), 3),
                       language = rep(c('ca', 'en', 'es'), each = 2),
                       label = c('Dia de l\'any',
                                 'Temperatura',
                                 'Day of the year',
                                 'Temperature',
                                 'Día del año',
                                 'Temperatura'))

title_dict <- data.frame(location = rep(c('left', 'right'), 3),
                         language = rep(c('ca', 'en', 'es'), each = 2),
                         label = c('Temperatura media',
                                   'Temperatures max/min',
                                   'Average temperature',
                                   'Max/min temperature',
                                   'Temperatura media',
                                   'Temperaturas max/min'))

# Define function for animation of full year
full_year <- function(language = 'es'){
  # Get Barcelona only
  weather_bcn <- weather[weather$city == 'Barcelona',]
  # Get a numeric x-lab for month day
  weather_bcn$x <- as.numeric(factor(weather_bcn$month_day))
  xs <- unique(sort(weather_bcn$x))
  
  setwd(paste0('../bcndata.github.io/img/2015-12-16-Weather/full_year_', language))
  for (i in 1:length(xs)){
    file_name <- paste0(i)
    while(nchar(file_name) < 3){
      file_name <- paste0('0', file_name)
    }
    file_name <- paste0(file_name, '.png')
    png(file_name,
        width = 650, height = 480)
    par(mfrow = c(1,2))
    sub_data <- weather_bcn[which(weather_bcn$x == xs[i]),]
    
    #-----
    plot(weather_bcn$x, 
         weather_bcn$Mean_TemperatureC,
         pch = 1,
         col = ifelse(weather_bcn$x == xs[i],
                      adjustcolor('darkgreen', alpha.f = 0.6),
                      adjustcolor('gold', alpha.f = 0.2)),
         xlab = as.character(lab_dict$label[lab_dict$language == language & lab_dict$location == 'x']),
         ylab = as.character(lab_dict$label[lab_dict$language == language & lab_dict$location == 'y']),
         ylim = c(0,40))
    abline(v = xs[i], col = adjustcolor('gold', alpha.f = 0.6))
    abline(h = mean(sub_data$Mean_TemperatureC),
           col = adjustcolor('gold', alpha.f = 0.6))
    today <- format(sub_data$date[1], '%B %d')
    title(main = as.character(title_dict$label[title_dict$language == language & title_dict$location == 'left']))
    
    #-----
    plot(weather_bcn$x, 
         weather_bcn$Max_TemperatureC,
         pch = 1,
         col = ifelse(weather_bcn$x == xs[i],
                      adjustcolor('darkred', alpha.f = 0.8),
                      adjustcolor('darkred', alpha.f = 0.1)),
         xlab = as.character(lab_dict$label[lab_dict$language == language & lab_dict$location == 'x']),
         ylab = as.character(lab_dict$label[lab_dict$language == language & lab_dict$location == 'y']),
         ylim = c(0, 40))
    points(weather_bcn$x, 
           weather_bcn$Min_TemperatureC,
           pch = 1,
           col = ifelse(weather_bcn$x == xs[i],
                        adjustcolor('blue', alpha.f = 0.8),
                        adjustcolor('blue', alpha.f = 0.1)))
    abline(v = xs[i], col = adjustcolor('darkorange', alpha.f = 0.6))
    abline(h = mean(sub_data$Max_TemperatureC),
           col = adjustcolor('darkred', alpha.f = 0.6))
    abline(h = mean(sub_data$Min_TemperatureC),
           col = adjustcolor('darkblue', alpha.f = 0.6))
    title(main = as.character(title_dict$label[title_dict$language == language & title_dict$location == 'right']))
    
    this_month <- as.numeric(format(sub_data$date[1], '%m'))
    this_month_number <- this_month
    this_month <- 
      as.character(month_dict$month[month_dict$language == language & month_dict$number == this_month_number])
    this_day <- as.numeric(format(sub_data$date[1], '%d'))
    today <- paste0(this_day, ' ', this_month)
    
    title(main = today, outer = TRUE, line = -1)
    par(mfrow = c(1,1))
    dev.off()
    message(paste0(i, ' of ', length(xs)))
  }
  setwd(root)
}

# ##### CREATE HUNDREDS OF PNGS FOR THE PURPOSES OF GIF
# full_year('en')
# full_year('es')
# full_year('ca')

##### GET MONTHLY WEATHER FOR ALL CITIES
temp_dict <- lab_dict
temp_dict$label <- as.character(temp_dict$label)
temp_dict$label[temp_dict$location == 'x'] <- 
  c('Més', 'Month', 'Mes')
comparison_plot <- function(language = 'ca'){
  
  

  temp <- weather %>%
    filter(city %in% c('Athens', 'Barcelona',
                       # 'Granada', 
                       'Madrid', 
                       'Chicago',
                       'Toronto',
                       'New York', 
                       'Paris',
                       'Rome'),
           Max_TemperatureF <= 120,
           Min_TemperatureF >= -20) %>%
    group_by(city, month) %>%
    summarise(high = mean(Max_TemperatureC, na.rm = TRUE),
              low = mean(Min_TemperatureC, na.rm = TRUE),
              avg = mean(Mean_TemperatureC, na.rm = TRUE)) %>%
    mutate(month = factor(month,
                          levels = as.character(1:12)))
  
  city_cols <- brewer.pal(length(unique(temp$city)), 'Set2')
  city_cols <- adjustcolor(city_cols, alpha.f = 0.6)
  city_cols[2] <- 'red'
  
  x <- 
    ggplot(data = temp,
         aes(x = factor(month), y = avg, group = city, color = city)) +
    geom_line(size = 2) +
    scale_color_manual(values=city_cols) +
#     geom_line(data = temp %>% filter(city == 'Barcelona'),
#               aes(x = factor(month), y = avg),
#               color = 'red', size = 2, alpha = 0.6) +
    xlab('Month') +
    scale_x_discrete(labels=month_dict$month[month_dict$language == language]) +
    bcn_data_theme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11)) +
    theme(
      # legend.position="top",
      # legend.direction="horizontal",
      legend.title = element_blank()
    ) +
    # annotate("text", x = 3, y = 10, label = 'Barcelona') +
    ylab(as.character(lab_dict$label[lab_dict$language == language & lab_dict$location == 'y'])) +
    xlab(as.character(temp_dict$label[temp_dict$language == language & temp_dict$location == 'x']))
  
  print(x)
}

comparison_plot(language = 'ca')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/comparison_ca.JPG'))
comparison_plot(language = 'en')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/comparison_en.JPG'))
comparison_plot(language = 'es')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/comparison_es.JPG'))

##### DAILY VARIANCE
temp <- weather %>%
  filter(city != 'Gran Canarias',
         Max_TemperatureF <= 120,
         Min_TemperatureF >= -20) %>%
  group_by(city, date) %>%
  summarise(variance = Max_TemperatureC - Min_TemperatureC) %>%
  group_by(city) %>%
  summarise(variance = mean(variance, na.rm = TRUE)) %>%
  arrange(variance)

temp <- weather %>%
  mutate(variance = Max_TemperatureC - Min_TemperatureC) %>%
  filter(city != 'Gran Canarias',
         Max_TemperatureC <= 40,
         Min_TemperatureF >= -20) 

temp_avg <- temp %>%
  filter(variance != 0) %>%
  group_by(city) %>%
  summarise(variance = mean(variance, na.rm = TRUE)) %>%
  arrange(variance) %>%
  mutate(city = factor(city, levels = city))

temp$city <- factor(temp$city, levels = temp_avg$city)

temp_dict <- data.frame(xx = c('City', 'Ciutat', 'Ciudad'),
                        yy = c('Change in daily temperatures', 
                               'Diferencia en temperatura cotidiana', 
                               'Varianza en temperatura cotidiana'),
                        language = c('en', 'ca', 'es'))

variance_plot <- function(language = 'en'){
#   ggplot(data = temp,
#          aes(x = city, y = variance)) +
#     geom_jitter(alpha = 0.2) +
#     geom_violin(alpha = 0.4, fill = 'gold') +
#     xlab(as.character(temp_dict$xx[temp_dict$language == language])) +
#     ylab(as.character(temp_dict$yy[temp_dict$language == language])) +
#     geom_point(data = temp_avg,
#              aes(x = city, y = variance),
#              # stat = 'identity',
#              fill = NA,
#              color = 'darkred',
#              pch = '---', 
#              size = 10)
  ggplot(data = temp_avg,
         aes(x = city, y = variance)) +
    geom_bar(stat = 'identity', fill = 'gold', alpha = 0.6) +
        xlab(as.character(temp_dict$xx[temp_dict$language == language])) +
        ylab(as.character(temp_dict$yy[temp_dict$language == language])) 
}

variance_plot('en')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/variance_en.JPG'))
variance_plot('es')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/variance_es.JPG'))
variance_plot('ca')
ggsave(paste0('../bcndata.github.io/img/2015-12-16-Weather/variance_ca.JPG'))

