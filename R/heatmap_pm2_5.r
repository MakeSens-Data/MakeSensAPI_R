#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param data
#' @keywords 
#' @export 
#' @examples
#' gradient_pm2_5()

heatmap_pm2_5 <- function(data)
    {
    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')
    dias = seq(as.Date(data[['ts']][1]), as.Date(data[['ts']][length(data[['ts']])]), by = "day")
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
             vec <- c(vec,0)
             }
         }
    heat_map[[as.character(dias[i])]] <- vec
    }
    heat_map_matrix<- as.matrix(heat_map)
    
    colors = c(seq(0,12,length=100),seq(13,37,length=100),seq(38,55,length=100),seq(65,150,length=100),seq(151,250,length=100),seq(251,500,length=100))
    my_palette <- colorRampPalette(c('green', 'yellow', 'Orange', 'red', 'Purple','Brown'))(n = 599)
    heatmap.2(heat_map_matrix, margins=c(7,4), dendrogram='none',Colv = FALSE, Rowv = FALSE, col = my_palette, breaks = colors, xlab ='Estampa temporal',ylab ='Horas',
             key.title = '', key.xlab = 'PM2.5' )
}