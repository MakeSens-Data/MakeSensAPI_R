#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param data
#' @keywords 
#' @export 
#' @examples
#' gradient_pm2_5()

gradient_pm2_5 <- function(data,sample_rate)
    {

    library("scales")
    library(ggplot2)
    library("gplots")
    library(lubridate)
    library(latex2exp)
    
    colors <- c('green', 'yellow','Orange', 'red', 'Purple','Brown')
    data <- data %>% distinct(data$ts, .keep_all = TRUE)
    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')

    range <- seq(data$ts[1],data$ts[length(data$ts)],sample_rate)

    vec <- c()
    for (i in range){
        if (i %in% data$ts){
            vec <- c(vec,data$pm25_1[which(data$ts == i)])
        }
        else{
            vec <- c(vec,NA)
        }
    }
    dat <- data.frame('ts' = range, 'PM25' = vec)    
    output <- ggplot(dat, aes(x = ts, y = PM25, color = PM25 )) +
          geom_line(size = 1)  +
          scale_color_gradientn(colours =colors, limits=c(0,250),  values = rescale(c(0,12,37,55,150,250)), oob = scales::squish ) +
          ylab(TeX("$PM2.5 \\mu / m^3$")) + xlab("Estampa temporal") + labs( color = 'PM2.5')

output
    }