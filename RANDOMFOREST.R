#RANDOM FOREST

#
library(randomForest)

#subDir <- "Final_Files_HR"
#subDir <- "With_SR"
#subDir <- "Total_Final"


#dir.create(file.path(getwd(), subDir), showWarnings = FALSE)
#joinfiles join all files.

join_file <- function (name)
{
    
    name <- as.character(name)
    files <-  list.files(pattern= name)
    
    if(length(files) != 2)
    {
        stop("There are more file with same name",name)
    }
    else
    {
        variable <-  split_name(files[2])[2]
        
        if(variable == "PTXTM")
        {
            file_first <- read.table(files[2], header = TRUE)
            j <- 1
        }
        else
        {
            file_first <- read.table(files[1], header = TRUE)
            j <-2
        }
        
        file_second <- read.table(files[j], header = TRUE)
        file_second <- file_second[,-which(colnames(file_second)%in% c("TM", "TX", "P"))]
    }
    
    
    result <- merge(file_first, file_second, by= "Date", all= TRUE)
    
    namec <- paste0(name,".txt" )
    
    
    
}




#All files join all files. 

#lapply (names_estaciones, join_file)

#radom_forest fill missing values for missing values
random_forest_SR <- function(station)
{
    #name
    name <- as.character(station)
    
    #Read table
    station <- read.table(station, header = TRUE)
    
    #Real Data
    
    real_data <- station 
    
    #Diference between temperatura
    station$DifferenceTemperature <- station$TX - station$TM
    
    #Location NA's
    NAs= which((is.na(station$SR=="NA"))==TRUE)
    DataNas = station[NAs,]
    
    #Delete Columns "Date", "P", "TM"
    station_rf <- na.omit(station[,-which(colnames(station)%in% c("Date", "TM", "RH"))])
    #DataNas <- na.omit(DataNas[,-which(colnames(station)%in% c("Date", "TM", "RH", "SR"))])
    
    
    #Random Forest
    randomforest <- randomForest(SR ~ ., data=station_rf)
    valPredic <- predict(randomforest, DataNas)
    
    #Put missing values into matrix
    DataNas$SR <- as.numeric(valPredic) 
    station[NAs,] <- DataNas
    
    #Predict
    predic <- station[,-which(colnames(station)=='DifferenceTemperature')]
    
    #Delete .txt
    name <- gsub('.txt', "", name)
    station_names <- c(rep(name, nrow(real_data)))
    result <- data.frame(real_data$Date, real_data$SR, predic$SR, station_names)
    colnames(result) <- c("Date", "Real_Data", "Estimated_Data", "Station_Names")
    
    name <- paste0(name, ".txt")
    #write.table(predic, file = paste(getwd(),"Final_Files_SR", name, sep =  "/"), row.names = FALSE, quote = FALSE, col.names = TRUE)
    
    
    return(result)
    
}

#lapply(list.files(), random_forest_SR)
#setwd("C:/Users/JCRIVERA/Documents/Codigo_Nuevo_Clima/Datos/Datos_Prueba/Hourly_Data/Daily_Data/Random_Forest/Final_Files/With_SR")

#radom_forest fill missing values for missing values
random_forest_RH <- function(station)
{
    #name
    name <- as.character(station)
    
    
    #Read table
    station <- read.table(station, header = TRUE)
    
    #Real Data
    real_data <- station 
    
    #Location NA's
    NAs= which((is.na(station$RH=="NA"))==TRUE)
    DataNas = station[NAs,]
    
    #Delete Columns "Date", "P", "TM"  
    station_rf <- na.omit(station[,-which(colnames(station)%in% c("Date"))])
    #DataNas <- na.omit(DataNas[,-which(colnames(station)%in% c("Date", "TM", "RH", "SR"))])
    
    
    #Random Forest
    randomforest <- randomForest(RH ~ TX+TM+TX*TM, data=station_rf)
    valPredic <- predict(randomforest, DataNas)
    
    #Put missing values into matrix
    DataNas$RH <- as.numeric(valPredic) 
    station[NAs,] <- DataNas
    
    predic <- station
    
    name <- paste0(name, ".txt")
    #write.table(predic, file = paste(getwd(),"Final_Files_HR", name, sep =  "/"), row.names = FALSE, quote = FALSE, col.names = TRUE)
    
    
    #Delete .txt
    name <- gsub('.txt', "", name)
    station_names <- c(rep(name, nrow(real_data)))
    result <- data.frame(real_data$Date, real_data$RH, predic$RH, station_names)
    colnames(result) <- c("Date", "Real_Data", "Estimated_Data", "Station_Names")
    
    
    
    return(result)
    
}

#lapply(list.files(pattern ="\\.txt$"), random_forest_RH)


#graph_all_SR_HR graphs all stations.

graph_all_SR_RH <- function (listFiles, variable)
{
    if(variable == "Radiaci?n_Solar")
    {
        Data_Complete <- lapply(listFiles, random_forest_SR)
        
    }
    
    if(variable == "Humedad_Relativa")
    {
        Data_Complete <- lapply(listFiles, random_forest_RH)
    }
    
    lapply(Data_Complete, graph_station, variable = variable)
    
}  

