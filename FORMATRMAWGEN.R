#read_files chooses and reads files according to variable desired.
#Arguments     -files. List of files
#              -variable. Variable desired
#Return        -List of files 

read_files <- function (files, variable, Start_date, End_date, sepa)
{
    #Check name file
    
    if(split_name(files)[2] == variable)
    {
        #Read file
        namefile <- paste0(getwd(), "/AfterDailyControl_Data/", files)
        read_file <- read.table(namefile, header = T, sep = sepa )
        
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

put_rmawgenformat <- function(files, vari, Start_date, End_date, sepa)
{
    
    #Read files
    files_reading <- lapply(files, read_files, variable = vari ,Start_date = Start_date, End_date= End_date, sepa=sepa)
    
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

#missingvalues_rmawgen finds missing values using RMAWGEN package
#Arguments     -List. List of dairy files 
#              -varia. variable for finding missing values
#Return        -List with complete values

#Arguments for Rmawgen
#Argumentos <- c("Valores")
#PREC_CLIMATE <- NULL
#year_max <- 1990
#SR <- c(1600,0)
#RH <- c(100,0)
#LONG <- c(-110.119, NA)
#LAT <- c(27.51458, NA)
#TZ <- c("Etc/GMT+7", NA)
#Read from csv file
#Start_date <- c("2013-01-11")
#End_date <- c("2017-10-11")


#missingvalues_rmawgen <- function (List, varia, Start_date, End_date)
#{
#Read files
# files_rmwageformat <- put_rmawgenformat(List, varia)

#Format
#files_rmwageformat$year <- sapply(strsplit(as.character(files_rmwageformat$Date),'-'), "[", 1)
#files_rmwageformat$month <- sapply(strsplit(as.character(files_rmwageformat$Date),'-'), "[", 2)
#files_rmwageformat$day <- sapply(strsplit(as.character(files_rmwageformat$Date),'-'), "[", 3)

#Character to numeric
#files_rmwageformat$year <- as.numeric(files_rmwageformat$year)
#files_rmwageformat$month <- as.numeric(files_rmwageformat$month)
#files_rmwageformat$day <- as.numeric(files_rmwageformat$day)
#files_rmwageformat$Date <- NULL

#Total Days between start and end day

#seq_days <- seq(as.Date(Start_date), as.Date(End_date), by="days")
#tableALL <- data.frame(seq_days)
#tableALL <-matrix (NA, ncol= ) 
#colnames(tableALL) <- c("Date")

#Put Date into columns
#tableALL$year <- sapply(strsplit(as.character(tableALL$Date),'-'), "[", 1)
#tableALL$month <- sapply(strsplit(as.character(tableALL$Date),'-'), "[", 2)
#tableALL$day <- sapply(strsplit(as.character(tableALL$Date),'-'), "[", 3)
#tableALL$Date <- NULL

#format_rmwage <- merge(tableALL,files_rmwageformat, by = c("year", "month", "day"))
#format_rmwage <- rbind.fill(tableALL,files_rmwageformat)
#example <- merge(format_rmwage, files_rmwageformat, all.x=T)

#write.csv(format_rmwage, file = paste0(varia, ".csv"))
#return(example)


#}


#choose_stations chooses stations for applying rmwagen

choose_stations <- function(file)
{
    #Read file
    file <- read.csv(file, header = T)
    #file$Star_Data <- as.Date(file$Star_Data, "%m/%d/%Y")
    #file$End_Data <- as.Date(file$End_Data, "%m/%d/%Y")
    
    file$Star_Data <- as.Date(as.character(file$Star_Data), "%Y-%m-%d")
    file$End_Data <- as.Date(file$End_Data, "%Y-%m-%d")
    
    file$Station_Name <- as.character(file$Station_Name)
    
    #Start and End Data
    Start_End <- data.frame( (file$Station_Name), (file$Star_Data),  (file$End_Data))
    colnames(Start_End) <- c("Station_Name", "Star_Data", "End_Data")
    Start_End <- unique(Start_End)
    
    #Split by year
    split_year <- split(Start_End,  as.numeric(format(Start_End$Star_Data, "%Y")))
    
    #Grouping by station.
    
    group_station <- lapply(split_year, extract_names_data)
    
    return (group_station )
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



