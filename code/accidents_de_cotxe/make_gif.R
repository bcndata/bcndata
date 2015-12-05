
##### GIF OF ACCIDENTS OVER TIME
dates <- unique(sort(accidents$date))
dates <- dates[dates >= '2014-01-01']
for (i in 1:length(dates)){
  year <- format(dates[i], '%Y')
  file_name <- paste0('temp/', year, '/', as.character(dates[i]), '.png')
  sub_data <- accidents[accidents$date == dates[i],]
  for (r in 1:5){
    if(r == 1){ results_list <- list()}
    residuals <- data.frame(accidents[accidents$date < dates[i] &
                             accidents$date >= (dates[i] - r),])
    residuals$color <- adjustcolor('darkred', alpha.f = (6-r) * 0.2)
    residuals$size <- (6-r) * 0.2
    results_list[[r]] <- residuals
  }
  residuals <- do.call('rbind', results_list)
  
  png(file_name)
  plot(mapa, col = 'darkgrey', border = 'white')
  points(residuals$x, residuals$y, pch = 16, col = residuals$color,
         cex = residuals$size)
  points(sub_data, pch = 16, 
         col = adjustcolor('darkred', alpha.f = 0.99),
         cex = 1)
  title(main = format(dates[i], '%B %d, %Y'))
  dev.off()
  print(i)
}
