

#graph_station plots graph per variable
#- Arguments.   Station_table 

graph_station <- function (Station_table, variable)
{
    
 #   #Parameters
    real_data <- Station_table$Real_Data
    estimated_data <- Station_table$Estimated_Data
    dates <- Station_table$Date
    name <- unique(Station_table$Station_Names)
    
    #Units
    if(variable == 'Temperatura_Máxima' || variable == 'Temperatura_Mínima')
    {
        y = "Grados_Centigrados"
    }
    
    if(variable == 'Precipitación')
    {
        
        y = "Mililitros"
    }
    
    if(variable == 'Radiación_Solar')
    {
        y = "Calorias_cm2_diarios"
    }
    
    if(variable == 'Humedad_Relativa')
    {
        y = "Valor"
    }
    
    #Total Data
    dates <- as.Date(dates, format = "%Y-%m-%d")
    
    #Count NA
    NAs <-  which((is.na(real_data=="NA"))==TRUE)
    real_data[NAs] <- estimated_data[NAs]
    
    
    real_dat <- cbind.data.frame(dates, real_data)
    estimated_dat <- cbind.data.frame(dates, estimated_data)
    
    colnames(real_dat) <- c("Dates", "Value")
    colnames(estimated_dat) <- c("Dates", "Value")
    
    #Variable for plot
    grafica <- real_dat
    grafica$Datos <- c(rep("Datos_Reales", nrow(real_dat)))
    grafica$Datos[NAs] <- c("Datos_Estimados")
    
    #Graph
    graph <- ggplot(data=grafica, aes(x=Dates, y=Value, col=Datos)) + geom_point() +ggtitle(paste0(name,"\n",variable)) + theme(plot.title = element_text(hjust = 0.5)) + ylab(y) + xlab("Dias")
    name_grap <- paste0(paste(name,variable, sep="_"),".pdf")
    nameFile <-  paste0(".", "/", "Graphics", "/", name_grap)
    ggsave(nameFile, plot=graph)
    
    
    if(variable == 'Temperatura_Máxima' )
    {
        
        namefile = "TX"
    }
    
    if(variable == 'Temperatura_Mínima' )
    {
        
        namefile = "TM"
    }
    
    
    if(variable == 'Precipitación')
    {
        
        namefile = "P"
    }
    
    if(variable == 'Radiación_Solar')
    {
        namefile = "SR"
    }
    
    if(variable == 'Humedad_Relativa')
    {
        namefile = "RH"
    }
    
    #Plots for Random Forest
    
    if(namefile == "SR" )
    {
        name_file <- paste0(paste(name,namefile, sep="_"),".txt")
        #weather_data <- paste0(".", "/", "SR", "/", name_file )   
        weather_data <- paste0(".", "/Rmawgen/", "Files_By_Station", "/", name_file )
    }
    else if (namefile == "RH")
    {
        name_file <- paste0(paste(name,namefile, sep="_"),".txt")
        #weather_data <- paste0(".", "/", "RH", "/", name_file )   
        weather_data <- paste0(".", "/Rmawgen/", "Files_By_Station", "/", name_file )
    } 
    else
    {
        name_file <- paste0(paste(name,namefile, sep="_"),".txt")
        weather_data <- paste0(".", "/Rmawgen/", "Files_By_Station", "/", name_file )   
        
    }
    
    
    
    write.table(real_dat, file = weather_data, row.names = FALSE, quote = FALSE, sep = "\t", col.names = TRUE)
    
}

#generate_missing_values <- function (listFiles, resumefile, variable)
#graph_ graphs all stations estimated and real data
#Arguments    -listFiles. List of files with format rmawgen 
#             -variable_rmw. variable for using in rmwagen
#             -variable_plot. variable for plot
#Return graphs


#graph_all (list.files(pattern = "\\.csv$"), "./Results/Results_DailyControl.csv", "TEMPERATURE_MAX", 'Temperatura_M?xima', manual = 2, choose_station = c(24,7))

graph_all <- function(listFiles, resumefile, variable_rmw, variable_plot, manual, choose_station)
{
    
    #Data
    data_all <- generate_missing_values (listFiles, resumefile, variable_rmw, manual, choose_station)
    
    #Table per station
    table_station <- lapply(data_all, table_graph, resumefile = resumefile)
    lapply(table_station, function(x) lapply(x, graph_station, variable = variable_plot))
}



#table_graph makes tables for plotting

table_graph <- function(list, resumefile)
{
    
    #List with data
    data_real  <- list$real_data
    data_estimated <- list$estimated_data
    date <- list$date
    
    #Number of stations
    num_stations <- length(colnames(data_real)) 
    
    #Extract start and end data
    info_station <- read.csv(resumefile, header= TRUE)
    info_station$Station_Name <- as.character(info_station$Station_Name)
    info_station$Star_Data <- as.Date(info_station$Star_Data, format = "%Y-%m-%d")
    info_station$End_Data <- as.Date(info_station$End_Data, format = "%Y-%m-%d")
    
    info_station <- unique(info_station[,c("Station_Name", "Star_Data",  "End_Data" ) ])
    info_station <- subset(info_station, Station_Name %in% colnames(data_real))   
    
    station <- list()
    name_station <- list()
    
    #Station 
    for ( i in 1 :num_stations)
    {
        name_station[[i]]  <- rep(colnames(data_real)[i], length(data_real[,i]))
        station[[i]] <- data.frame(date, data_real[,i], data_estimated[,i],name_station[[i]])
        names(station[[i]]) <- c("Date", "Real_Data", "Estimated_Data", "Station_Names")
        star_date <- subset(info_station, Station_Name %in% colnames(data_real)[i])
        star <- star_date$Star_Data
        end <- star_date$End_Data
        station[[i]]  <- subset(station[[i]], Date >= star & Date <= end )
        
    }    
    
    names(station) <- colnames(data_real)
    
    return (station)
}

#paste_columns paste three columns  three columns
paste_columns <- function(column_date, colum_real, colum_estima)
{
    table <- data.frame(column_date, colum_real, colum_estima)
    names(table) <- c("Date","Real_Value","Estimated_Value")
    
    return(table)
    
}
