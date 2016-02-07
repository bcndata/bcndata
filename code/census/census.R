# ##### Directories 
root <- '/home/joebrew/Documents/bcndata'
data_dir <- paste0(root, '/data/census')
code_dir <- paste0(root, '/code')

##### Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(ggmap)
library(weatherData)
library(BcnDataAccess)
library(rgdal)
library(maptools)

##### Theme
source(paste0(root, '/lib/theme.R'))
source(paste0(root, '/lib/helpers.R'))
source(paste0(root, '/lib/dicts.R'))

##### Get data
setwd(data_dir)
if('all_data.RData' %in% dir()){
  load('all_data.RData')
} else {
  # Get data using BCN analytics' code
  
  # Nationality
  # https://github.com/BCNAnalytics/BcnDataAccess/blob/master/R/inAtlas_Nacionalidad.R
  nacionalidad <- read.csv('https://docs.google.com/uc?authuser=0&id=0ByVQHmO-WjuVc25WM0tfNDNzV0U&export=download', 
                           header = TRUE, sep =";", encoding = "UTF-8")
  colnames( nacionalidad )[ 1 ] <- "COD_MUNICIPIO"
  nacionalidad$X <- NULL
  
  # Catalan
  # https://github.com/BCNAnalytics/BcnDataAccess/blob/master/R/Idescat_Catala_2001.R
  catala <- read.table('https://docs.google.com/uc?authuser=0&id=0ByVQHmO-WjuVOGx4QnRNVmpLbm8&export=download')
  indexSenseTotals <- rep( c(TRUE, TRUE, FALSE), nrow( catala )/3 )
  catala <- catala[ indexSenseTotals, ]
  colnames( catala ) <- c("SeccioCensal", "Enten", "Parla", "Llegeix", "Escriu", "NoEnten", "Poblacio")
  catala$Sexe <- c( 'home', 'dona' )
  
  # Professional activity
  # https://github.com/BCNAnalytics/BcnDataAccess/blob/master/R/Idescat_Activitat_2001.R
  fieldDescription <- c(
    "PoblacioMes16", "Població de 16 anys i més",
    "Ocupats", "Ocupats",
    "DesocPrimFeina", "Desocupats que busquen la seva primera feina", 
    "Desoc", "Desocupats que han treballat abans",
    "ServMilPresSoc", "Fa el servei militar o la prestació social",
    "JubPen", "Jubilats o pensionistes",
    "Incap", "Incapacitat permanent per treballar",
    "Estudiant", "Estudiant,escolar o preescolar",
    "Llar", "Fa feines de la llar no remunerades",
    "Altres", "Altres situacions",
    "Poblacio", "Poblacio"
  )
  fieldDescription <- matrix( ncol = 2, data = fieldDescription, byrow = TRUE)
  
  
  getDataFunction = function(){
    activitat <- read.table('https://docs.google.com/uc?authuser=0&id=0ByVQHmO-WjuVM1lIQkp2c3JuLUk&export=download')
    indexSenseTotals <- rep( c(TRUE, TRUE, FALSE), nrow( activitat )/3 )
    activitat <- activitat[indexSenseTotals, ]
    selCol <- c(1, 2:5, 7:12, 14)
    activitat <- activitat[, selCol]
    colnames( activitat ) <- c( "SeccioCensal", fieldDescription[, 1])
    activitat$Sexe <- c( 'home', 'dona' )
    
    return( activitat )
  }
  activitat <- getDataFunction()
  
  # Genero
  # https://github.com/BCNAnalytics/BcnDataAccess/blob/master/R/inAtlas_Genero.R
  genero <- read.csv('https://docs.google.com/uc?authuser=0&id=0ByVQHmO-WjuVVnhzRlEwWjRFS3c&export=download', header = TRUE, sep =";")
  colnames( genero )[ 1 ] <- "COD_MUNICIPIO"
  genero$X <- NULL
  
  # Profession
  fieldDescription <- c(
    "Directius", "Personal directiu de les empreses i administracions publiques",
    "CientIntel", "Tecnics i professionals,cientifics i intelectuals", 
    "Tecnics", "Tecnics i professionals de suport",
    "Administratius", "Empleats administratius",
    "Comerc", "Treballadors de serveis i venedors de comerç",
    "AgrarPesca", "Treballadors qualificats en activitats agraries i pesqueres",
    "Artesans", "Artesans i treballadors qualificats de les industries i la construccio",
    "Operadors", "Operadors d'instalacions,maquinaria i muntadors",
    "NoQual", "Treballadors no qualificats",
    "ForcArm", "Forces armades"
  )
  fieldDescription <- matrix( ncol = 2, data = fieldDescription, byrow = TRUE)
  
  
  getDataFunction = function(){
    professio <- read.table('https://docs.google.com/uc?authuser=0&id=0ByVQHmO-WjuVdTd4dDY3ZnJuaWM&export=download')
    indexSenseTotals <- rep( c(TRUE, TRUE, FALSE), nrow( professio )/3 )
    professio <- professio[indexSenseTotals, ]
    professio <- professio[, -ncol( professio )]
    colnames( professio ) <- c( "SeccioCensal", fieldDescription[, 1])
    professio$Sexe <- c( 'home', 'dona' )
    
    return( professio )
  }
  profession <- getDataFunction()
  
  # Census seccio demographics
  # https://github.com/BCNAnalytics/BcnDataAccess/blob/master/R/DemoSeccioCensal.R
  demo <- read.csv('http://opendata.bcn.cat/opendata/ca/descarrega-fitxer?url=http%3a%2f%2fbismartopendata.blob.core.windows.net%2fopendata%2fopendata%2fmap_scensal_0_opendata_map_scensal.csv&name=MAP_SCENSAL.csv', sep = ";")
  
#   # Business data
#   # https://github.com/BCNAnalytics/BcnDataAccess/blob/master/R/inAtlas_Economicas.R
#   economicas <- read.csv('http://arcvi.io/wp-content/uploads/2015/02/BCN_ANALYTICS_BBDD_ECONOMICAS.csv', 
#                          header = TRUE, sep =";", encoding = "UTF-8")
#   colnames( economicas )[ 1 ] <- "SECTOR"
#   economicas$X <- NULL
  
  # In Atlas age
  # https://github.com/BCNAnalytics/BcnDataAccess/blob/master/R/inAtlas_Edad.R
  edad <- read.csv('https://docs.google.com/uc?authuser=0&id=0ByVQHmO-WjuVcWdBUElRS25JRXM&export=download', header = TRUE, sep =";")
  colnames( edad )[ 1 ] <- "COD_MUNICIPIO"
  edadRange <- t( sapply( as.character( edad$EDAD ), function( edadVal ){
    if( edadVal != "100 y mas" ){
      return( as.numeric( strsplit( edadVal, " " )[[1]][c(1, 3)] ) )      
    }else{
      return( c(100, 150) )
    }
  }, USE.NAMES = FALSE, simplify = TRUE ) )
  
  edad$EDAD_RANGO_INF <- edadRange[, 1]
  edad$EDAD_RANGO_SUP <- edadRange[, 2]
  edad$X <- NULL
  edad$EDAD <- NULL
  
  # Save
  save('activitat',
       'catala',
       'demo',
       'edad',
       'edadRange',
       'genero',
       'nacionalidad',
       'profession',
       file = 'all_data.RData')
}

##### LOAD SPATIAL DATA
# downloaded from https://github.com/martgnz/bcn-geodata/tree/master/src
if('spatial_data.RData' %in% dir()){
  load('spatial_data.RData')
} else {
  area_estadistica <- readOGR('spatial', 'BCN_Àrea_Estadística_Bàsica_ED50_SHP')
  area_interes <- readOGR('spatial', 'BCN_Àrea_Interès_ED50_SHP')
  barri <- readOGR('spatial', 'BCN_Barri_ED50_SHP')
  districte <- readOGR('spatial', 'BCN_Districte_ED50_SHP')
  seccio_censal <- readOGR('spatial', 'BCN_Secció_Censal_ED50_SHP')
  # Alternative seccio censal
  sec <- readOGR('spatial', 'SECC_CPV_E_20111101_01_R_INE')
  # subset to only include barcelona
  sec <- sec[sec@data$NMUN == 'Barcelona', ]
  save('area_estadistica', 
       'area_interes',
       'barri',
       'districte',
       'seccio_censal',
       'sec',
       file = 'spatial_data.RData')
}

##### CATALA

# Join the seccio_censal spatial data
# to the catalan language data

# Spatial data
left <- 
  sec@data %>%
  mutate(sec = as.character(CUSEC)) %>%
  mutate(sec = substr(sec, 2, nchar(sec)))


# # Language data
# right <- catala %>%
#   mutate(sec = as.character(SeccioCensal)) %>%
#   group_by(sec) %>%
#   summarise(Enten = sum(Enten),
#             Parla = sum(Parla),
#             Llegeix = sum(Llegeix),
#             Escriu = sum(Escriu),
#             NoEnten = sum(NoEnten),
#             Poblacio = sum(Poblacio))

# Gender data
right <- genero %>%
    mutate(sec = as.character(COD_SSCC)) %>%
    group_by(sec) %>%
    summarise(m = sum(TOTAL_POB[GENERO == 'HOMBRES']),
              f = sum(TOTAL_POB[GENERO == 'MUJERES']))
  
# Collapse into only one map
collapse_map <- function(x){
  boundary <- unionSpatialPolygons(x, rep(1, length(x@polygons)))
  return(boundary)
}

# Get a boundary of BCN
boundary <- collapse_map(sec)

# Bring all the data back into the spatial
sec@data <- left_join(left, right); rm(left, right)
  
# Calculate ratio of females to males
sec@data$fm <- round(sec@data$f / sec@data$m * 100)
# and males to females
sec@data$mf <- round(sec@data$m / sec@data$f * 100)

# Create palettes for men and women
max_color <- max(c(sec@data$mf, sec@data$fm)) - 100
colors_f <- colorRampPalette(brewer.pal(9, 'Reds'))(max_color)
colors_m <- colorRampPalette(brewer.pal(9, 'Blues'))(max_color)


colors_vector <- rep('white', nrow(sec))
colors_vector[sec@data$mf > 100] <- colors_m[sec@data$mf[sec@data$mf > 100] - 100]
colors_vector[sec@data$fm > 100] <- colors_f[sec@data$fm[sec@data$fm > 100] - 100]

# Make a function for gender plotting
plot_gender <- function(language = 'en'){
  if(language == 'en'){
    title_text <- 'Gender by census tract'
    male_text <- ' more men'
    female_text <- ' more women'
  }
  if(language == 'es'){
    title_text <- 'Género por sección censal'
    male_text <- ' más hombres'
    female_text <- ' más mujeres'
  }
  if(language == 'ca'){
    title_text <- 'Gènere per secció censal'
    male_text <- ' més homes'
    female_text <- ' més dones'
  }
  plot(sec, col = colors_vector, border = NA)
  plot(boundary, add = T)
  
  title(main = title_text, cex.main = 2)
  
  legend_vector <- c(rev(colors_f), colors_m)
  legend_vector <- legend_vector[seq(10, 130, 10)]
  legend_label <- abs(70 - seq(10, 130, 10))
  legend_label[seq(2,12,2)] <- NA
  legend_label[!is.na(legend_label)] <- 
    paste0(legend_label[!is.na(legend_label)], '%')
  legend_label[1] <- paste0(legend_label[1], female_text)
  legend_label[13] <- paste0(legend_label[13], male_text)
  
  legend(x = 'bottomleft',
         fill = legend_vector,
         legend = legend_label,
         cex = 2,
         border = NA,
         ncol = 1,
         y.intersp = 0.5,
         bty = 'n',
         text.col = adjustcolor(legend_vector, alpha.f = 0.95))
}
h = 600 
w = 900
par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
img_dir <- '/home/joebrew/Documents/bcndata.github.io/img/2016-02-06-gender/'
png(filename = paste0(img_dir, 'map_en.png'),
    height = h, width = w)
plot_gender('en')
dev.off()
png(filename = paste0(img_dir, 'map_ca.png'),
    height = h, width = w)
plot_gender('ca')
dev.off()
png(filename = paste0(img_dir, 'map_es.png'),
    height = h, width = w)
plot_gender('es')
dev.off()


