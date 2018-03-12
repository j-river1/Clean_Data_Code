#read_files chooses and reads files according to variable desired.
#Arguments     -files. List of files
#              -variable. Variable desired
#Return        -List of files 
#

read_files_form <- function (files, variable)
{
    #Check name file
    vari <- variable
    
    #Start and End Data
    file <- read.csv(paste0(here(),"/Results/","Results_DailyControl.csv"), header = T)
    file$Star_Data <- as.Date(as.character(file$Star_Data), format = "%Y-%m-%d")
    Start_date<- min(as.double(format(file$Star_Data, "%Y")))
    Start_date <- paste0(Start_date, "-1-1")
    
    
    file$End_Data <- as.Date(as.character(file$End_Data), format = "%Y-%m-%d")
    End_date<- max(as.double(format(file$End_Data, "%Y")))
    End_date <- paste0(End_date, "-12-31")
    
    if(split_name(files)[2] == vari)
    {
        #Read file
        namefile <- paste0(getwd(), "/AfterDailyControl_Data/", files)
        read_file <- read.table(namefile, header = T)
        
        #read_file <- read.table(files, header = T, sep = sepa )
        
        
        #Change colnames
        colnames(read_file) <- c("Date", as.character(split_name(files)[1]))
        read_file$Date <- as.Date(read_file$Date, format = "%Y-%m-%d")
        
        #Values NA
        seq_days <- seq(as.Date(Start_date), as.Date(End_date), by="days")
        tableALL <- data.frame(seq_days) 
        colnames(tableALL) <- c("Date")
        text_file <- merge (tableALL, read_file, all.x = TRUE)
        
        
    }
    else
    {
        text_file <- NULL
    }
    
    return(text_file)
}

#put_rmawgenformat puts to file with rmawgen format
#Arguments:       -files= List of files 
#                 -vari = variable match


#put_rmawgenformat(list.files("./AfterDailyControl_Data"), 'TX', Start_date, End_date)

put_rmawgenformat <- function(files, vari)
{
    
    #Read files
    files_reading <- lapply(files, read_files_form, variable = vari)
    
    #Remove Null
    files_reading = files_reading[-which(sapply(files_reading, is.null))]
    
    #Merge Columns
    merge_all <- do.call("cbind", files_reading)
    position_dates <- which(colnames(merge_all)== 'Date')
    merge_all <- merge_all[-position_dates[-1]]
    
    
    merge_all$year <- sapply(strsplit(as.character(merge_all$Date),'-'), "[", 1)
    merge_all$month <- sapply(strsplit(as.character(merge_all$Date),'-'), "[", 2)
    merge_all$day <- sapply(strsplit(as.character(merge_all$Date),'-'), "[", 3)
    merge_all$Date <- NULL
    
    #Ordercolumns
    merge_all<-merge_all[,c(length(merge_all), length(merge_all)-1, length(merge_all)-2, rep(1:(length(merge_all)-3)))]  
    
    name <- paste(vari, ".csv", sep="")
    weather_data <- paste0(".", "/", "Rmawgen", "/", name )            
    #write.csv(merge_all, file = paste(vari, ".csv", sep=""), row.names=FALSE)
    write.csv(merge_all, file = weather_data, row.names=FALSE)
}



#choose_stations chooses stations for applying rmwagen

choose_stations <- function()
{

    #Clustering for stations
    #Read file with longitude and latitude
    file_long_lat <- read.csv(paste0(here(),"/Results/","Results_DailyControl.csv"), header = T)
    
    #Read value of distance clustering
    dist_est <- read.csv(paste0(here(),"/SpatialInformation_InputVariables/","Input_Variables.csv"), header = T)
    
    
    file_station <- file_long_lat[, -c(4:7)]
    file_station <- unique(file_station) 
    
    long <- file_station$Longitude
    lati <- file_station$Latitude
    name <- file_station$Latitude
    
    
    long_lati <- SpatialPointsDataFrame(
      matrix(c(long,lati), ncol=2), data.frame(Station_Name=file_station$Station_Name),
      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    
    mdist <- distm(long_lati)
    hc <- hclust(as.dist(mdist), method="complete")

    
    # define the distance threshold, in this case 10000 m or 10Km
    d=dist_est$dist_Station
    
    # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
    long_lati$clust <- cutree(hc, h=d)
    
    
    # expand the extent of plotting frame
    long_lati@bbox[] <- as.matrix(extend(extent(long_lati),0.001))
    
    # get the centroid coords for each cluster
    cent <- matrix(ncol=2, nrow=max(long_lati$clust))
    for (i in 1:max(long_lati$clust))
      # gCentroid from the rgeos package
      cent[i,] <- gCentroid(subset(long_lati, clust == i))@coords
    
    # compute circles around the centroid coords using a 40m radius
    # from the dismo package
    ci <- circles(cent, d=d, lonlat=T)
    

    jpeg(paste0(here(), "/Graphics/Clustering_Stations/Clustering_Stations.jpeg"))
    plot(ci@polygons, axes=T, main = paste("Clustering Stations to", "\n", d, " meters") )
    plot(long_lati, col=rainbow(4)[factor(long_lati$clust)], add=T)
    dev.off()
    
    
    #Start and end data 
    data_star_end <- file_long_lat[,c("Station_Name", "Star_Data", "End_Data","Variable_Name")]
    data_star_end  <- unique(data_star_end )
    total <- merge(long_lati@data, data_star_end, by= "Station_Name", all.x= TRUE)
    write.csv(total, paste0(here(), "/Results/Clustering_Stations.csv"))
    

}  


#extract_names_data extracts names, star data and end data. 
#Arguments  -Station per year
extract_names_data <- function (stations)
{
    #Date minimun and maximun for station groups
    date_min <- stations$Star_Data[order(stations$Star_Data)][1]
    date_max <- stations$End_Data[order(stations$End_Data)][length(stations$End_Data)]
    
    #Station names
    station_names <- stations$Station_Name
    
    #Data frame
    group <- data.frame(date_min, date_max, station_names)
    colnames(group) <- c("date_min","date_max","station_names") 
    #FSFSAS
    
    return(group)
}



