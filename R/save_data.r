#' An example for writing a function in R using Roxygen (e.g. a variant of Doxygen for
#' R) 
#' Well, this function is for writing stuff, I suppose that I need to write here the
#' pourpose of the function.
#' @param IdDevice 
#' @param tmin
#' @param tmax
#' @param frecuency
#' @param toke
#' @param format
#' @keywords
#' @export 
#' @examples
#' save_data()

save_data <- function(IdDevice,tmin,tmax,frecuency,token,format) 
{
    tmin <- as.numeric(as.POSIXct(tmin))
    tmax <- as.numeric(as.POSIXct(tmax))
    datt <- data.frame()
    while (tmin < tmax)
    {
        url =  paste('https://makesens.aws.thinger.io/v1/users/MakeSens/buckets/B',IdDevice,'/data?agg=1',frecuency,'&agg_type=mean&items=1000&max_ts=',tmax, '000&min_ts=',tmin,'000&sort=asc&authorization=',token,sep='')
        #Descargar los datos
        r <- GET(url)
        datos <- content(r, type = 'application/json', simplifyDataFrame = TRUE)
        #Manipular los datos
        dat <- cbind(datos[2]/1000,datos[[1]]) #El primer elemento es el tiempo y el segundo las demas variables
        dat <- rename(dat, c(value="ts"))
        dat$ts <- as.POSIXlt(dat$ts,origin="1970-01-01")
        t <-  datos[2][[1]][length(datos[2][[1]])] / 1000
        datt <- rbind(datt,dat)

        if (toString(t) == tmin)
        {
            tmin <- tmax
        }
        
        else
        {
            tmin <- toString(t)
        }
    }
    if (format == 'csv')
    {
        name <- paste(IdDevice,'_',as.Date(as.POSIXlt(tmin,origin = '1970-01-01')),'_',as.Date(as.POSIXlt(tmax,origin = '1970-01-01')),'.csv',sep='')
        write.csv(datt,name)
    }
    else if (format == 'xlsx')
    {
        name <- paste(IdDevice,'_',as.Date(as.POSIXlt(tmin,origin = '1970-01-01')),'_',as.Date(as.POSIXlt(tmax,origin = '1970-01-01')),'.xlsx',sep='')
        write.xlsx(datt,name)
    }
}