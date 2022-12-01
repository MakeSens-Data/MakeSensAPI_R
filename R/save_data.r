#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param id_device 
#' @param start_date
#' @param end_date
#' @param sample_rate
#' @param toke
#' @param format
#' @keywords
#' @export 
#' @examples
#' save_data()

save_data <- function(id_device,start_date,end_date,sample_rate,format,fields=NULL) 
{
    library(httr)
    library(dplyr)
    library(xlsx)
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
        #Descargar los datos
        r <- GET(url)
        datos <- content(r, type = 'application/json', simplifyDataFrame = TRUE)
        #Manipular los datos
        dat <- cbind(datos[1]/1000,datos[[2]]) #El primer elemento es el tiempo y el segundo las demas variables
        dat$ts <- as.POSIXlt(dat$ts,origin="1970-01-01")
        t <-  datos[1][[1]][length(datos[1][[1]])] / 1000
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
    
    if (format == 'csv')
    {
        name <- paste(id_device,'_',datt$ts[1],'_',as.Date(as.POSIXlt(end_date,origin = '1970-01-01')),'.csv',sep='')
        write.csv(datt,name)
    }
    else if (format == 'xlsx')
    {
        name <- paste(id_device,'_',datt$ts[1],'_',as.Date(as.POSIXlt(end_date,origin = '1970-01-01')),'.xlsx',sep='')
        write.xlsx(datt,name)
    }
}