download_data <- function(id_device, start_date, end_date, sample_rate, logs = FALSE, data_type = 'RAW', file_format= NULL, fields = NULL) 
{
  # Función para manejar las variables
    convert_measurements <- function(measurements, mode="lower") {
        # Diccionario de correcciones específicas
        corrections <- list(
            temperatura2 = "temperatura_2",
            temperatura_2 = "temperatura2",
            humedad2 = "humedad_2",
            humedad_2 = "humedad2",
            TEMPERATURA2 = "TEMPERATURA_2",
            TEMPERATURA_2 = "TEMPERATURA2",
            HUMEDAD2 = "HUMEDAD_2",
            HUMEDAD_2 = "HUMEDAD2"
        )
        
        new_measurements <- c()
  
        for (measurement in measurements) {
            # Aplicar correcciones específicas si es necesario
            if (measurement %in% names(corrections)) {
                measurement <- corrections[[measurement]]
            }

            # Convertir a mayúsculas o minúsculas según el modo
            new_measurement <- ifelse(mode == 'upper', toupper(measurement), tolower(measurement))
            new_measurements <- c(new_measurements, new_measurement)
        }
  
        return(new_measurements)
    }

    download <-  function(id_device,start_date,end_date,sample_rate,logs,data_type,fields){
        library(httr)
        library(dplyr)
        library(jsonlite)

        start_date <- as.numeric(as.POSIXct(start_date))
        end_date <- as.numeric(as.POSIXct(end_date)) 
        datt <- data.frame()
        while (start_date < end_date)
        {
            # Validar si se piden logs
            if (logs){
                if (is.null(fields))
            {
                url = paste('https://api.makesens.co/device/',id_device,'/logs?min_ts=',start_date,'000&max_ts=',end_date,'000&agg=',sample_rate,'&data_type=',data_type,sep='') 
            }
             else
            {
                fields_list <- unlist(strsplit(fields, ","))
                fields_list <- convert_measurements(fields_list, mode="upper")
                fields <- paste(fields_list, collapse=",")
                url = paste('https://api.makesens.co/device/',id_device,'/logs?min_ts=',start_date,'000&max_ts=',end_date,'000&agg=',sample_rate,'&data_type=',data_type,'&fields=',fields,sep='') 
            }
            }
            else{
            # Validar si se piden variables especificas
            if (is.null(fields))
            {
                url = paste('https://api.makesens.co/device/',id_device,'/data?min_ts=',start_date,'000&max_ts=',end_date,'000&agg=',sample_rate,sep='','&data_type=',data_type) 
            }
            else
            {
                fields_list <- unlist(strsplit(fields, ","))
                fields_list <- convert_measurements(fields_list, mode="upper")
                fields <- paste(fields_list, collapse=",")
                url = paste('https://api.makesens.co/device/',id_device,'/data?min_ts=',start_date,'000&max_ts=',end_date,'000&agg=',sample_rate,'&data_type=',data_type,'&fields=',fields,sep='') 
            }}
            # Hacer la petición
            r <- GET(url) # nolint
            response_text <- content(r, "text" ,encoding = "UTF-8")
            cleaned_response <- gsub("NaN", "null", response_text)
            data_list <- fromJSON(cleaned_response)
            df <- data_list$data %>% as.data.frame()
            df$ts <- df$ts / 1000 # Convertir milisegundos a segundos
            df$ts <- as.POSIXlt(df$ts,origin="1970-01-01")
            # Concatenar las datas
            if (ncol(datt) == 0) 
            {
                datt <- df
                
            }
            else
            {
                
                missing_cols <- setdiff(names(datt), names(df))
                for(col in missing_cols) {
                    df[[col]] <- NA # Fill new columns with NAs
                }
                datt <- rbind(datt, df)
            }
            
            t <-  as.numeric(data_list$date_range$end) / 1000

            if (toString(t) == start_date)
            {
                start_date <- end_date
            }

            else
            {
                start_date <- toString(t)
            }
            
        }
    
        # Eliminar los repetidos
        datt <- datt %>% distinct(ts, .keep_all = TRUE)
        colnames(datt) <- convert_measurements(colnames(datt), mode="lower")
        # Cambio específico de nombres de columnas
        colnames(datt) <- gsub("pm10_1_ae", "pm10_1_AE", colnames(datt))
        colnames(datt) <- gsub("pm10_2_ae", "pm10_2_AE", colnames(datt))
        colnames(datt) <- gsub("pm25_1_ae", "pm25_1_AE", colnames(datt))
        colnames(datt) <- gsub("pm25_2_ae", "pm25_2_AE", colnames(datt))
        colnames(datt) <- gsub("pm1_1_ae", "pm1_1_AE", colnames(datt))
        colnames(datt) <- gsub("pm1_2_ae", "pm1_2_AE", colnames(datt))
        return(datt)
    }
    
    datt <- download(id_device,start_date,end_date,sample_rate,logs,data_type,fields)

    if (is.null(fields)){
        return(datt)
    }
    else if (file_format == 'csv')
    {
        name <- paste(id_device, 
              format(as.POSIXct(datt$ts[1]), "%Y-%m-%d_%H-%M-%S"), 
              format(as.Date(as.POSIXlt(end_date, origin = '1970-01-01')), "%Y-%m-%d"), 
              '.csv', sep='_')
        write.csv(datt,name)
    }
    else if (file_format == 'xlsx')
    {
        library(xlsx)
        name <- paste(id_device, 
              format(as.POSIXct(datt$ts[1]), "%Y-%m-%d_%H-%M-%S"), 
              format(as.Date(as.POSIXlt(end_date, origin = '1970-01-01')), "%Y-%m-%d"), 
              '.xlsx', sep='_')
        write.xlsx(datt,name)
    }
}


