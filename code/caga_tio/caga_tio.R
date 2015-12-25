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
library(gtrendsR)

##### Theme
source(paste0(root, '/lib/theme.R'))
source(paste0(root, '/lib/helpers.R'))

##### Set up Google Trends
usr <- "<Google account email>"
psw <- "<Google account password>"
ch <- gconnect(usr, psw) 
# caga <- gtrends(c("data is", "data are"), res="day")
caga <- gtrends(query = 'caga tio',  
                      res="week")

##### Get the trend into a dataframeag
trend <- caga$trend

##### Make a date object from start
trend$date <- as.Date(trend$start)

##### Get a year column
trend$year <- format(trend$date, '%Y')

##### Group by year and replot
temp <- 
  trend %>%
  group_by(year) %>%
  summarise(avg = mean(caga.tio))

## Make year numeric
temp$year <- as.numeric(temp$year)



##### Function for multilingual plot
xing_plot <- function(language = "en") {
  ##define labs as in accordance with language
  if(language == "en"){
    xx <- "Year"
    yy <- "Search volume"
  }
  if(language == "ca"){
    xx <- "Any"
    yy <- 'Volume de bùsquedes'
  }
  if(language == "es"){
    xx <- "Año"
    yy <- "Volumen de búsquedas"
  }
 
  ##### Xing's plot of temp
  ggplot(data = temp,
         aes(x = year, y = avg)) +
    geom_point(alpha = 0.5) +
    geom_smooth(alpha = 0.2, color = "gold", size = 2) +
    xlab(xx) + ylab(yy) +
    ylim(0,20)
}
  
# run plot function
g1_ca <- xing_plot(language = "ca")
xing_plot(language = "ca")
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/trend_ca.png')
g1_en <- xing_plot(language = "en")
xing_plot(language = "en")
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/trend_en.png')
g1_es <- xing_plot(language = "es")
xing_plot(language = "es")
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/trend_es.png')

###############################
# Santa claus
santa <- gtrends(query = 'papa noel',  
                res="week")

##### Get the trend into a dataframeag
trend <- santa$trend

##### Make a date object from start
trend$date <- as.Date(trend$start)

##### Get a year column
trend$year <- format(trend$date, '%Y')

##### Group by year and replot
temp <- 
  trend %>%
  group_by(year) %>%
  summarise(avg = mean(papa.noel))

## Make year numeric
temp$year <- as.numeric(temp$year)

# run plot function
g2_ca <- xing_plot(language = "ca")
xing_plot(language = "ca")
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/trend_santa_ca.png')
g2_en <- xing_plot(language = "en")
xing_plot(language = "en")
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/trend_santa_en.png')
g2_es <- xing_plot(language = "es")
xing_plot(language = "es")
ggsave('/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/trend_santa_es.png')

#####
png(filename = '/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/combined_en.png',
    width = 720)
multiplot(g1_en + ggtitle('Poop log'), 
          g2_en + ggtitle('Santa Claus'),
          cols = 2)
dev.off()

png(filename = '/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/combined_ca.png',
    width = 720)
multiplot(g1_ca + ggtitle('Caga tió'), 
          g2_ca + ggtitle('Papa noel'),
          cols = 2)
dev.off()

png(filename = '/home/joebrew/Documents/bcndata.github.io/img/2015-12-25-caga-tio/combined_es.png',
    width = 720)
multiplot(g1_es + ggtitle('Caga tio'), 
          g2_es + ggtitle('Papá noel'),
          cols = 2)
dev.off()