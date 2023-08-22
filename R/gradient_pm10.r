#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param data
#' @keywords 
#' @export 
#' @examples
#' gradient_pm10()

gradient_pm10 <- function(data,sample_rate)
    {
    library("scales")
    library(ggplot2)
    library("gplots")
    library(lubridate)
    library(latex2exp)
    
    colors <- c('green', 'yellow','Orange', 'red', 'Purple','Brown')
    data <- data %>% distinct(data$ts, .keep_all = TRUE)
    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')

    if (sample_rate == 'm'){
    sample_rate = 'mins'
    }
    range <- seq(from = min(data$ts), to = max(data$ts), by = 'hour')

    vec <- sapply(range, function(i) {
    matching_row <- data[data$ts == i,]
    if(nrow(matching_row) > 0) {
        return(matching_row$pm10_1)
    } else {
        return(NA)
    }
    })
    dat <- data.frame('ts' = range, 'PM10' = vec)

    output <- ggplot(dat, aes(x = ts, y = PM10, color = PM10)) +
    geom_line(size = 1)  +
    scale_color_gradientn(colours =colors, limits=c(0,425),  values = rescale(c(0,54,154,254,354,425)), oob = scales::squish ) + 
        ylab(TeX("$PM10 \\mu / m^3$")) + xlab("Estampa temporal") + labs( color = 'PM10')
        output
    }