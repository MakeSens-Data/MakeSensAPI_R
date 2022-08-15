#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param data
#' @keywords 
#' @export 
#' @examples
#' gradient_pm10()

gradient_pm10 <- function(data)
    {
    library("scales")
    library(ggplot2)
    library("gplots")
    library(lubridate)
    library(latex2exp)
    
    colors <- c('green', 'yellow','Orange', 'red', 'Purple','Brown')
    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')
    
    
    output <- ggplot(data, aes(x = ts, y = pm10_1, color = pm10_1 )) +
      geom_line(size = 1.5)  +
      scale_color_gradientn(colours =colors, limits=c(0,425),  values = rescale(c(0,54,154,254,354,425)), oob = scales::squish ) + 
      ylab(TeX("$PM10 \\mu / m^3$")) + xlab("Estampa temporal") + labs( color = 'PM10')
    output
    }