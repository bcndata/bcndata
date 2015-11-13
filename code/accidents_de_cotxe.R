# ##### Directories 
root <- '/home/joebrew/Documents/bcn_data'
data_dir <- paste0(root, '/data/accidents_de_cotxe')
code_dir <- paste0(root, '/code')

##### Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
# require(RCurl)
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

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
