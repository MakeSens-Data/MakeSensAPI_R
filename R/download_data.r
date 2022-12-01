#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param id_device
#' @param start_date
#' @param end_date
#' @param sample_rate
#' @param token
#' @keywords 
#' @export 
#' @examples
#' download_data()

download_data <- function(id_device,start_date,end_date,sample_rate) 
{
    download <- function(id_device,start_date,end_date,sample_rate) 
    {   
        library(httr)
        library(dplyr)
        start_date <- as.numeric(as.POSIXct(start_date))
        end_date <- as.numeric(as.POSIXct(end_date)) 
        datt <- data.frame()
        while (start_date < end_date)
        {
            if (is.null(fields))
            {
            url = paste('https://api.makesens.co/ambiental/metricas/',id_device,'/data?agg=1',sample_rate,'&agg_type=mean&items=1000&max_ts=',end_date,'000&min_ts=',start_date,'000',sep='')
            
            }
            else
            {
            url = paste('https://api.makesens.co/ambiental/metricas/',id_device,'/data?agg=1',sample_rate,'&agg_type=mean&fields=',fields,'&items=1000&max_ts=',end_date,'000&min_ts=',start_date,'000',sep='')                  
            }
            
            r <- GET(url) # nolint
            datos <- content(r, type = 'application/json', simplifyDataFrame = TRUE)
            #Manipular los datos
            dat <- cbind(datos[1]/1000,datos[[2]]) #El primer elemento es el tiempo y el segundo las demas variables
            #dat <- rename(dat, c(value="ts"))
            dat$ts <- as.POSIXlt(dat$ts,origin="1970-01-01")
            t <-  datos[2][[1]][length(datos[2][[1]])] / 1000
            datt <- bind_rows(datt,dat)
            

            if (toString(t) == start_date)
            {
                start_date <- end_date
            }

            else
            {
                start_date <- toString(t)
            }
        }
        return(datt)
    }
    
    tryCatch(download(id_device,start_date,end_date,sample_rate),error = function(e) message('No hay datos en este intervalo'))
}
