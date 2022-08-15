#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param data
#' @keywords 
#' @export 
#' @examples
#' gradient_pm2_5()

gradient_pm2_5 <- function(data)
    {

    library("scales")
    library(ggplot2)
    library("gplots")
    library(lubridate)
    library(latex2exp)
    
    colors <- c('green', 'yellow','Orange', 'red', 'Purple','Brown')
    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')
    
    
    output <- ggplot(data, aes(x = ts, y = pm25_1, color = pm25_1 )) +
      geom_line(size = 1.5)  +
      scale_color_gradientn(colours =colors, limits=c(0,250),  values = rescale(c(0,12,37,55,150,250)), oob = scales::squish ) +
      ylab(TeX("$PM2.5 \\mu / m^3$")) + xlab("Estampa temporal") + labs( color = 'PM2.5')

    output
    }