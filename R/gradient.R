gradient <- function(data,variable)
    {

    suppressPackageStartupMessages(library(lubridate))
    suppressPackageStartupMessages(library(gplots))
    library("scales")
    library(ggplot2)
    library("gplots")
    library(lubridate)
    library(latex2exp)
    
    colors <- c('green', 'yellow','Orange', 'red', 'Purple','Brown')
    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')
    names(data)[2] <- 'variable_'

   if (variable == 'PM10'){
      breaks <- c(0, 54, 154, 254, 354, 425)
    }
    else if (variable == 'PM2.5'){
      breaks <- c(0,12,37,55,150,250)
    }
    options(repr.plot.width=18, repr.plot.height=6)
    ggp <- ggplot(data, aes(x = ts, y = variable_, color = variable_ )) + 
       geom_line(size = 0.6)  +
          scale_color_gradientn(colours =colors, limits=c(0,250),  values = rescale(breaks), oob = scales::squish ) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),legend.title = element_blank(),legend.key.height = unit(1, "in"),    panel.border = element_rect(colour = "black", fill = NA, size = 0.5, linetype = "solid"), 
          plot.background = element_rect(fill = "white", colour = "white"), panel.background = element_rect(fill = "white", colour = "white"),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) +
          ylab(TeX(paste('$', variable ,"\\,\\mu / m^3$")))   + xlab("Estampa temporal") 

    print(ggp)
    }