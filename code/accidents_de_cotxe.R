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
# require(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

##### Theme
source(paste0(root, '/lib/theme.R'))

##### Data read in
csvs <- dir(data_dir)
csvs <- csvs[grepl('.csv', csvs)]
csvs <- paste0(data_dir, '/', csvs)
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

##### Geographic data
mapa <- readOGR(paste0(root, '/data/geo/barris'), 'BCN_Barri_SHP')

##### Convert both accidents and mapa to lat/lng

# Accidents
coordinates(accidents) <- ~x+y
proj4string(accidents) <- CRS(proj4string(mapa))
accidents <- spTransform(accidents, CRS("+proj=longlat +datum=WGS84"))
accidents$x

# Mapa
mapa <- spTransform(mapa, CRS("+proj=longlat +datum=WGS84"))

# ##### GIF OF ACCIDENTS OVER TIME
# dates <- unique(sort(accidents$date))
# dates <- dates[dates >= '2014-01-01']
# for (i in 1:length(dates)){
#   year <- format(dates[i], '%Y')
#   file_name <- paste0('temp/', year, '/', as.character(dates[i]), '.png')
#   sub_data <- accidents[accidents$date == dates[i],]
#   for (r in 1:5){
#     if(r == 1){ results_list <- list()}
#     residuals <- data.frame(accidents[accidents$date < dates[i] &
#                              accidents$date >= (dates[i] - r),])
#     residuals$color <- adjustcolor('darkred', alpha.f = (6-r) * 0.2)
#     residuals$size <- (6-r) * 0.2
#     results_list[[r]] <- residuals
#   }
#   residuals <- do.call('rbind', results_list)
#   
#   png(file_name)
#   plot(mapa, col = 'darkgrey', border = 'white')
#   points(residuals$x, residuals$y, pch = 16, col = residuals$color,
#          cex = residuals$size)
#   points(sub_data, pch = 16, 
#          col = adjustcolor('darkred', alpha.f = 0.99),
#          cex = 1)
#   title(main = format(dates[i], '%B %d, %Y'))
#   dev.off()
#   print(i)
# }

##### GET WORST INTERSECTION
temp <- accidents %>%
  data.frame %>%
  group_by(x,y) %>%
  tally 
temp <- temp[rev(order(temp$n)),]
worst <- temp[1,]

plot(mapa)
points(worst, pch = 16, col = 'red')

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