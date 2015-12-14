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
lang_trend <- gtrends(c("data is", "data are"), res="week")
lang_trend <- gtrends(query = c(#"santa", 
                                "papa+noel",
                                "caga+tio"), 
                      res="week")

plot(lang_trend)
