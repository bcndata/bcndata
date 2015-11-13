##### Directories 
root <- getwd()
data_dir <- paste0(root, '/data')
code_dir <- paste0(root, '/code')

##### Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
# require(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

##### Data read in
setwd(data_dir)
csvs <- dir()
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

# TIME OF DAY
tod <- 
  accidents %>%
  group_by(Hora.de.dia) %>%
  summarise(n = n())
ggplot(data = tod, aes(x = Hora.de.dia, y = n / 5)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  xlab('Hora del dia') +
  ylab('Accidents per any')
rm(tod)

# DAY OF WEEK
dow <- 
  accidents %>%
  group_by(Descripció.dia.setmana) %>%
  summarise(n = n())
dow$Descripció.dia.setmana <- 
  factor(as.character(dow$Descripció.dia.setmana),
         levels = c('Dilluns', 'Dimarts', 'Dimecres', 'Dijous',
                    'Divendres', 'Dissabte', 'Diumenge'))
ggplot(data = dow, aes(x = Descripció.dia.setmana, y = n)) +
  geom_bar(stat = 'identity') +
  theme_economist()
