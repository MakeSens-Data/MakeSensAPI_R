heatmap_ <- function(data)
    {
    library("scales")
    library(ggplot2)
    library("gplots")
    library(lubridate)
    library(latex2exp)
    library(tidyr)  

    names(data)[2] <- 'variable'
    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')
    data$hora <- hour(data$ts)
    data$fecha <- as.Date(data$ts)

    heat_map <- data %>%
    group_by(fecha, hora) %>%
    summarize(variable = mean(variable, na.rm = TRUE), .groups = 'drop')

    heat_map <- heat_map %>%
    tidyr::complete(fecha, hora, fill = list(variable = NA))

    heat_map_wide <- heat_map %>%  pivot_wider(names_from = fecha, values_from = variable)
    heat_map_matrix <- as.matrix(heat_map_wide)
    
    colors = c(seq(0,54,length=100),seq(55,154,length=100),seq(155,254,length=100),seq(255,354,length=100),seq(355,424,length=100),seq(425,604,length=100))
    my_palette <- colorRampPalette(c('green', 'yellow', 'Orange', 'red', 'Purple','Brown'))(n = 599)
    heatmap.2(heat_map_matrix, margins=c(7,4), dendrogram='none',Colv = FALSE, Rowv = FALSE, col = my_palette, breaks = colors, xlab ='Estampa temporal',ylab ='Horas',
             key.title = '', key.xlab = 'PM10' )
}
