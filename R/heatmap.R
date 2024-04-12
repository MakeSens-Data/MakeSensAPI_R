heatmap <- function(data, variable)
    {
    library(reshape2)
    library('devtools')
    library(lubridate)
    library("gplots")
    library(ggplot2)
    library(latex2exp)
    library(scales)

    
    names(data)[2] <- 'variable'
    data[['ts']] <- as.POSIXct(data[['ts']], format='%Y-%m-%d  %H:%M:%S')
    dias = seq(as.Date(data[['ts']][1]), as.Date(data[['ts']][length(data[['ts']])]), by = "day")
    dias <- dias[1:length(dias)-1]
    heat_map <- data.frame(row.names =  0:23)
    for (i in 1:length(dias))
    {
    Muestra <- subset(data, as.Date(ts)== dias[i])
    Muestra[['ts']] = lubridate::hour(Muestra[['ts']])
    vec <- c()
    for (j in 0:23)
        {
        if (j %in% Muestra[['ts']])
            {
            vec <- c(vec,subset(Muestra, ts == j)[['variable']])
            }
         else
             {
             vec <- c(vec,NA)
             }
         }
    heat_map[[as.character(dias[i])]] <- vec
    }
    heat_map_matrix<- as.matrix(heat_map)
    
    
    data_melted <- melt(heat_map_matrix)
    # Nombrar las columnas apropiadamente para ggplot2
    names(data_melted) <- c('Hours', 'Days', 'value')
    # Definimos la paleta de colores
    if (variable == 'PM10'){
      colors <- c('green', 'yellow', 'orange', 'red', 'purple', 'brown')
      breaks <- c(0, 54, 154, 254, 354, 425)
    }
    else if (variable == 'PM2.5'){
      colors <- c('green', 'yellow', 'orange', 'red', 'purple', 'brown')
      breaks <- c(0,12,37,55,150,250)
    }

    # Rescale the breaks to a range of 0 to 1
    rescaled_breaks <- rescale(breaks, to = c(0, 1))

    # Crear el mapa de calor con ggplot2
    ggp <- ggplot(data_melted, aes(x = Days, y = Hours, fill = value)) +
      geom_tile() + 
      scale_fill_gradientn(colours = colors, limits = c(0, 425), values = rescaled_breaks, oob = squish, na.value = "white") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.background = element_rect(fill = "white", colour = "white"),  # Fondo del panel en blanco
      plot.background = element_rect(fill = "white", colour = "white"),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14)) + 
      ylab( TeX(paste('$', variable, "\\,\\mu / m^3$", sep = ""))) + xlab("Estampa temporal") +
      guides(fill = guide_colorbar(barwidth = 1.0, barheight = 25, title.position = "top", title.hjust = -0.5)) +
      labs(fill = "") 



    # Imprimir el mapa de calor
    print(ggp)
    }