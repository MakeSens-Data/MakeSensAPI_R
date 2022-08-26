#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param data
#' @keywords 
#' @export 
#' @examples
#' heatmap_pm10()

heatmap_pm10 <- function(data)
    {
    library("scales")
    library(ggplot2)
    library("gplots")
    library(lubridate)
    library(latex2exp)

    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')
    dias = seq(as.Date(data[['ts']][1]), as.Date(data[['ts']][length(data$ts]), by = "day")
    dias <- dias[1:length(dias)-1]
    heat_map <- data.frame(row.names =  0:23)
    
    for (i in 1:length(dias))
    {
    Muestra <- subset(data, as.Date(ts)== dias[i])
    Muestra[['ts']] = hour(Muestra[['ts']])
    vec <- c()
    for (j in 0:23)
        {
        if (j %in% Muestra[['ts']])
            {
            vec <- c(vec,subset(Muestra, ts == j)[['pm10_1']])
            }
         else
             {
             vec <- c(vec,NA)
             }
         }
    heat_map[[as.character(dias[i])]] <- vec
    }
    heat_map_matrix<- as.matrix(heat_map)
    
    colors = c(seq(0,54,length=100),seq(55,154,length=100),seq(155,254,length=100),seq(255,354,length=100),seq(355,424,length=100),seq(425,604,length=100))
    my_palette <- colorRampPalette(c('green', 'yellow', 'Orange', 'red', 'Purple','Brown'))(n = 599)
    heatmap.2(heat_map_matrix, margins=c(7,4), dendrogram='none',Colv = FALSE, Rowv = FALSE, col = my_palette, breaks = colors, xlab ='Estampa temporal',ylab ='Horas',
             key.title = '', key.xlab = 'PM10' )
}
