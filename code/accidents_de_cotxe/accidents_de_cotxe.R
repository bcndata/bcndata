# ##### Directories 
root <- '/home/joebrew/Documents/bcndata'
data_dir <- paste0(root, '/data/accidents_de_cotxe')
code_dir <- paste0(root, '/code')

##### Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(ggmap)
library(weatherData)
# require(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

##### Theme
source(paste0(root, '/lib/theme.R'))
source(paste0(root, '/lib/helpers.R'))

##### Data read in
csvs <- dir(data_dir)
csvs <- csvs[grepl('.csv', csvs)]
csvs <- paste0(data_dir, '/', csvs)
csvs <- csvs[grepl('ACCIDENTS', csvs)]
results_list <- list()
hh <- c()
for (i in 1:length(csvs)){
  if(i == 5){
    temp <- read.csv(csvs[i], sep = ';', stringsAsFactors=FALSE, skip = 3, header = FALSE,  fileEncoding="latin1")
    names(temp) <- hh
  } else {
    temp <- read.csv(csvs[i], stringsAsFactors=FALSE, fileEncoding="latin1")
    if(i == i){
      hh <- names(temp)
    }
  }
  temp$year <- as.numeric(gsub('ACCIDENTS_GU_BCN_|.csv', '', csvs[i]))
  results_list[[i]] <- temp
}
accidents <- do.call('rbind', results_list)
rm(temp, results_list, csvs, hh, i)

# Get a data
accidents$date <-
  as.Date(paste(accidents$NK.Any,
                 accidents$Mes.de.any,
                 accidents$Dia.de.mes,
                 sep = '-'))

# Numeric coordiantes
accidents$y <- as.numeric(gsub(',', '.', accidents$Coordenada.UTM..Y.))
accidents$x <- as.numeric(gsub(',', '.', accidents$Coordenada.UTM..X. ))

# Remove those non-geocoded accidents
accidents <- accidents[accidents$x != -1 & accidents$y != -1,]

# Get year, month, etc
accidents$year <- as.numeric(format(accidents$date, '%Y'))
accidents$year_month <- paste0(format(accidents$date, '%Y'),
                                      '-',
                                      format(accidents$date, '%m'))
accidents$month <- as.numeric(format(accidents$date, '%m'))

##### Geographic data
mapa <- readOGR(paste0(root, '/data/geo/barris'), 'BCN_Barri_SHP')

##### Convert both accidents and mapa to lat/lng

# Accidents
coordinates(accidents) <- ~x+y
proj4string(accidents) <- CRS(proj4string(mapa))
accidents <- spTransform(accidents, CRS("+proj=longlat +datum=WGS84"))

# Mapa
mapa <- spTransform(mapa, CRS("+proj=longlat +datum=WGS84"))

##### WEATHER
if(!'weather.csv' %in% dir(data_dir)){
  years <- 2010:2014
  results_list <- list()
  for (y in 1:length(years)){
    weath <- getWeatherForDate(station_id = 'BCN',
                               start_date = paste0(years[y], '-01-01'),
                               end_date = paste0(years[y], '-12-31'),
                               opt_all_columns = TRUE)
    results_list[[y]] <- weath
    }
  weather <- do.call('rbind', results_list)
  write_csv(weather, paste0(data_dir, '/weather.csv'))
} else {
  weather <- read_csv(paste0(data_dir, '/weather.csv'))
}

# Join weather to accidents
temp <- accidents %>%
  data.frame %>%
  group_by(date) %>%
  summarise(accidents = n()) %>%
  left_join(weather %>%
              mutate(date = as.Date(Date))) %>%
  mutate(max_temp_bin = round(Max_TemperatureF, digits = -1),
         rainy = PrecipitationIn > 0)

# Temperature
ggplot(data = temp,
       aes(x= factor(max_temp_bin),
           y = accidents)) +
  geom_jitter(alpha = 0.2) +
  geom_violin(fill = 'darkgreen', alpha = 0.2) +
  ylab('Accidents') +
  xlab('Maximum temperature (F)') +
  ggtitle('Daily car accidents in Barcelona by temperature')
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/weather.JPG')

temp$rainy <- ifelse(temp$rainy,
                     'Some rain', 
                     'No rain')
ggplot(data = temp,
       aes(x = rainy,
           y = accidents)) +
  geom_jitter(alpha = 0.2) +
  geom_violin(fill = 'darkgreen', alpha = 0.2) +
  ylab('Accidents') +
  xlab('Raininess') +
  ggtitle('Daily car accidents in Barcelona by raininess')
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/rainy.JPG')


# GIF
# source(paste0(root_dir, '/code/accidents_de_cotxe/make_gif.R'))

##### GRID APPROACH TO GETTING WORST INTERSECTION
temp <- expand.grid(
  x = seq(min(accidents$x), max(accidents$x), length = 100),
  y = seq(min(accidents$y), max(accidents$y), length = 100)
)

# Placeholders for accidents within 10, 100, 1000 meters
temp$within_0010 <- temp$within_0100 <- temp$within_1000 <- NA

coordinates(temp) <- ~x+y
proj4string(temp) <- proj4string(mapa)

# Loop through each grid
for (i in 1:nrow(temp)){
  this_point <- temp[i,]
  distance <- get_distance(lon1 = this_point$x,
                           lat1 = this_point$y, 
                           lon2 = accidents$x,
                           lat2 = accidents$y)
  temp$within_0010[i] <- length(which(distance <= 0.01))
  temp$within_0100[i] <- length(which(distance <= 0.1))
  temp$within_1000[i] <- length(which(distance <= 1))
  cat(paste0('Finished ', i, ' of ', nrow(temp), '\n'))
}

# Organize by within_0010
temp <- temp[order(temp$within_0010),]

# Visualize
cols <- rev(colorRampPalette(brewer.pal(9, 'Spectral'))(max(temp$within_0010)))
plot(mapa)
points(temp, col = adjustcolor(cols[temp$within_0010], alpha.f = 0.2), 
       pch = 16, cex = 0.6)

##### GET WORST INTERSECTION
temp <- accidents %>%
  data.frame %>%
  # round
  mutate(x = round(x, digits = 3),
         y = round(y, digits = 3)) %>%
  group_by(x,y) %>%
  summarise(n = n(),
            deaths = sum(Número.de.morts),
            victims = sum(Número.de.víctimes),
            vehicles = sum(Número.de.vehicles.implicats)) 
temp <- temp[rev(order(temp$n)),]
worst <- temp[1,]
paste0(worst$y, ', ', worst$x)

# See worst intersection
x = get_map(location = c(lon = worst$x - 0.0004, lat = worst$y + 0.0005),
            zoom = 18,
            maptype = 'toner',
            source = 'stamen',
            color = 'bw')
ggmap(x)

# Expand temp to get all points in Barcelona
temp_ex <- expand.grid(x = seq(min(temp$x) - 0.05,
                               max(temp$x) + 0.05,
                               by = 0.001),
                       y = seq(min(temp$y) - 0.05,
                               max(temp$y) + 0.05,
                               by = 0.001))

# Join temp_ex to temp
temp_ex <- left_join(temp_ex, temp)
temp_ex$n[is.na(temp_ex$n)] <- 0

# Define color
# cols <- rev(colorRampPalette(brewer.pal(9, 'Spectral'))(max(temp$n)))
cols <- colorRampPalette(brewer.pal(9, 'Oranges'))(max(temp$n))
temp_ex$color <- ifelse(temp_ex$n == 0,
                        'blue',
                        cols[temp_ex$n])

# Make spatial
coordinates(temp_ex) <- ~x+y
proj4string(temp_ex) <- proj4string(mapa)

# Keep only those within the Barcelona polygons
temp_ex <- temp_ex[!is.na(over(temp_ex, polygons(mapa))),]

# Visualize
plot(mapa, border = NA)
points(temp_ex, col = temp_ex$color, pch = 16, cex = 0.4)

#####
# NUMBER OF ACCIDENTS BY YEAR / MONTH
#####
temp <- accidents %>%
  data.frame %>%
  group_by(Date = year) %>%
  summarise(n = n(),
            deaths = sum(Número.de.morts),
            victims = sum(Número.de.víctimes),
            vehicles = sum(Número.de.vehicles.implicats))

ggplot(data = temp, aes(x = Date, y = n)) +
  geom_bar(stat = 'identity', alpha = 0.5, fill = 'darkgreen') +
  # geom_point() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab('') +
  ylab('Accidents') +
  ggtitle('Accidents per year')
    # geom_bar(stat = 'identity', data = temp, aes(x = Date, y = deaths))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/by_year.JPG')


#####
# DEATHS ONLY
#####
temp <- accidents %>%
  data.frame %>%
  filter(Número.de.morts > 0) %>%
  group_by(Hora.de.dia) %>% tally

plot(mapa)  
points(temp$x, temp$y, col = 'red')

#####
# LOCATION
#####

# Red for any death, orange for any serious injury
accidents$color <- 
  ifelse(accidents$Número.de.morts > 0, 
         adjustcolor('red', alpha.f = 0.8),
         ifelse(accidents$Número.de.lesionats.greus > 0,
                adjustcolor('darkorange', alpha.f = 0.5),
                ifelse(accidents$Número.de.lesionats.lleus > 0,
                       adjustcolor('blue', alpha.f = 0.3),
                       adjustcolor('green', alpha.f = 0.2))))

# Size
accidents$size <-
  ifelse(accidents$Número.de.morts > 0, 
         0.6,
         ifelse(accidents$Número.de.lesionats.greus > 0,
                0.4,
                ifelse(accidents$Número.de.lesionats.lleus > 0,
                       0.2, 0.1)))

# remove points outside of bbox
accidents <- 
  accidents[which(
    accidents$x >= data.frame(bbox(mapa))['min'][1,] &
      accidents$x <= data.frame(bbox(mapa))['max'][1,] &
      accidents$y >= data.frame(bbox(mapa))['min'][2,] &
      accidents$y <= data.frame(bbox(mapa))['max'][2,]),]

# Order so that green comes first, red last

par(mar = c(0,0,0,0))
jpeg('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/points2.JPG',
     width = 700,
     height = 500)
plot(mapa, 
     col = adjustcolor('beige', alpha.f = 0.9),
     border = NA)#adjustcolor('black', alpha.f = 0.2))
points(accidents$x, accidents$y, pch = 16,
       cex = accidents$size,
       col = accidents$color)
dev.off()
par(mar = c(4.1, 2.1, 2.1, 1.1))

mapa_f <- fortify(mapa)

ggplot(mapa_f, aes(long, lat,group=group)) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  geom_point(data = accidents, 
             aes(x = x, y = y, color = color, size = 0.2 * size, group = NA), alpha = 0.2)
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/points.JPG')

ggplot(mapa_f, aes(long, lat,group=group)) + 
  geom_polygon(fill = 'grey') +
  geom_path(color="white") +
  coord_equal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  #   geom_point(data = accidents, 
  #               aes(x = x, y = y, color = color, size = 0.2 * size, group = NA), alpha = 0.2) +
  stat_density2d(aes(alpha=..level..), geom="polygon", fill = 'red') +
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/hotspots.JPG')

#####
# TIME OF DAY
#####


# TIME OF DAY
tod <- 
  accidents %>%
  group_by(Hora.de.dia) %>%
  summarise(n = n())
ggplot(data = tod, aes(x = Hora.de.dia, y = n / 5)) +
  geom_point() +
  geom_smooth() +
  xlab('Time of day') +
  ylab('Accidents') +
  bcn_data_theme +
  ggtitle('Don\'t drive at 3pm')
rm(tod)
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/hora.JPG')

#####
# DAY OF WEEK
#####

dow <- 
  accidents %>%
  group_by(Descripció.dia.setmana) %>%
  summarise(n = n())
dow$Descripció.dia.setmana <- 
  factor(as.character(dow$Descripció.dia.setmana),
         levels = c('Dilluns', 'Dimarts', 'Dimecres', 'Dijous',
                    'Divendres', 'Dissabte', 'Diumenge'))
ggplot(data = dow, aes(x = Descripció.dia.setmana, y = n)) +
  geom_bar(stat = 'identity', fill = 'red', alpha = 0.6) +
  bcn_data_theme +
  ggtitle('Stay off the road on Friday') +
  xlab('Day of the week') +
  ylab('Accidents') +
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-11-30-Car-accidents-in-Barcelona/dia.JPG')


#####
# MONTH OF YEAR
#####


##### 
# BARCA GAMES
#####