#Check_TotalData_Day checks if data per day has total data enough for processing as day. Each day must have at least 80% possible data.
# Arguments:  -weatherdata after hourlycontrol. It has to be in days.
#             -Percentage for an admissible day. If data per day has less data than that percentage then
#              day is delete.
#             -weatherfile : Weather file
#                      
# Return:     1 or 0.  If the day meets with condition, return 1.
#                      If the day dont meet with condition, return 0.   

Check_TotalData_Day <- function(weatherdataperday, percentageday, weatherfile, long=LONG[1], lat=LAT[1], tz=TZ[1])
{
    #Delay time
    delaytime <- time_step(weatherdataperday)
    
    #Convert delaytime in number 
    delaytime_number <- delaytime/60
    
    #Days with only a data
    if(delaytime_number == 0)
    {
        check_day = 0
        warning('There is only a data in the station : ', weatherfile ,' in the day: ', unique(weatherdataperday$Date))
    }
    
    else
    {
        
        #Format
        weatherdataperday <- as.data.frame(weatherdataperday)
        colnames(weatherdataperday) = c("Date", "Hour", "Value", "HourDecimal")    
        
        #Different scale
        #Extract part decimal
        extract_partDeci <- unlist(weatherdataperday$HourDecimal)%%1
        is_naturalnumber <-  round(extract_partDeci/delaytime_number, digits = 2)%%1
        change_hour <- unlist(weatherdataperday$HourDecimal[which(is_naturalnumber!=0)])
        
        if(is.null(change_hour)== TRUE)
        {
            #Day divide by delaytime
            HourDecimal_Allday <- seq(0,23.999, by = delaytime_number)
            All_data <- data.frame(HourDecimal_Allday)
            All_data$Value <- NA
        }
        else  
        {
            #Day divide by delaytime
            HourDecimal_Allday <- Table_NA(change_hour, delaytime_number)
            All_data <- data.frame(HourDecimal_Allday)
            All_data$Value <- NA
            
        }
        colnames(All_data) <- c("HourDecimal","Value")
        
        #Exception when station has more delay time
        count_true <- All_data$HourDecimal %in% weatherdataperday$HourDecimal
        if(sum(count_true, na.rm=TRUE) != length(weatherdataperday$Value)){stop('There is a problem with delay time of station : ', weatherfile ,' in the day: ', unique(weatherdataperday$Date))}
        
        #Match values
        All_data[All_data$HourDecimal %in% weatherdataperday$HourDecimal,]$Value <- weatherdataperday$Value
        
        #Control para SR
        if(split_name(weatherfile)[2]=='SR')
        {
            sunrise_hour <- hour_solarnoon_sunrise(unique(weatherdataperday$Date), lat=lat, long=long, timezo=tz, typeofhour= "sunrise") 
            noon_hour <- hour_solarnoon_sunrise(unique(weatherdataperday$Date), lat=lat, long=long, timezo=tz, typeofhour= "solarnoon") 
            
            sunrise_hour <- hour_to_number(sunrise_hour)
            noon_hour <- hour_to_number(noon_hour)
            
            time_solar <- 2*(noon_hour - sunrise_hour) 
            time_solar <- c(sunrise_hour, sunrise_hour + time_solar)
            
            All_data <- subset (All_data, HourDecimal > time_solar[1] & HourDecimal < time_solar[2] ) 
        }
        
        
        #Count NA
        count_NA <- sum(is.na(All_data$Value))
        
        
        check_day <- ifelse(length(All_data$Value)*(1-percentageday) > count_NA, 1, 0 )
        
    }
    return (check_day)
    
}

#Check_Day_Station returns days that meet with condition refers to number of data. 
#Arguments: - data after hourly control
#         : - percentage_data.  Percentage for an admissible day
#Return:    - fill data           

Check_Day_Station <- function(weatherdata, percentage_data, LONG=LONG, LAT=LAT, TZ=TZ)
{
    #Read
    weather_data <- put_format(weatherdata, date_format="%Y-%m-%d" )
    
    #Divide per day
    divi_day <-  divide_by_day(weather_data)
    names_day <- unique(names(divi_day))
    
    
    #Check day 
    check_day <- lapply(divi_day, function (x) Check_TotalData_Day (x, percentageday =percentage_data, weatherfile = weatherdata))
    
    admissible_day <- data.frame(names_day, unlist(check_day))
    colnames(admissible_day) <- c("Date", "CheckDay")
    
    
    days_per_station <- admissible_day[which(admissible_day$CheckDay==1),]$Date
    
    return(days_per_station)
    
}  

#Hour_to_Day converts hours to days.
#Arguments:   - Weather data with hourly control
#             - percentage_data.  Percentage for an admissible day

Hour_to_Day <- function(weather_data, percentage)
{
    
    #Dates
    dates <- Check_Day_Station(weather_data, percentage)
    
    #Divide per day
    divi_day <-  divide_by_day(put_format(weather_data, date_format = "%Y-%m-%d"))
    days_aux <- divi_day[which(names(divi_day) %in% dates)]
    days <- do.call(rbind.data.frame, days_aux)
    
    
    if(split_name (weather_data)[2]== 'P')
    {
        if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
        hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=sum)
        hours_day <- subset(hours_day, hours_day$x <= 1000)
    }
    
    if(split_name (weather_data)[2]== 'SR')
    {
        if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
        #hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=sum)
        SR_day <- lapply(days_aux, SR_hourlytodaily)
        SR_names <- names(SR_day)
        hours_day <- data.frame(SR_names, as.double(unlist(SR_day)))
    }
    
    if(split_name (weather_data)[2]== 'RH')
    {
        if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
        hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=mean)
    }
    
    if(split_name (weather_data)[2]== 'TX')
    {
        if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
        hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=max)
    }
    
    if(split_name (weather_data)[2]== 'TM')
    {
        if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
        hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=min)
    }
    
    colnames(hours_day) <- c("Date","Value")
    
    
    name <- as.character(weather_data)
    weather_data <- paste0("..", "/", "AfterDailyControl_Data", "/", name )            
    write.table (hours_day, file = weather_data, row.names= FALSE, sep = "\t", col.names = TRUE)
    #setwd('..')
    #return(hours_day)
    
}

#Table_NA makes a table witn NA
#Arguments  - HourBreaks. Hours with breaks
#           - timedelay.  Time of reception signal
#return table with NA

Table_NA <- function(HourDecimal,timedelay)
{
    SeqNumbers <- list()
    for (i in 1:length(HourDecimal))
    { 
        
        if(i < length(HourDecimal))
        {
            SeqNumbers[[i]] <-  seq(HourDecimal[i],HourDecimal[i+1], by = timedelay)
        }
        
        
    }
    
    sequ_ini <- seq(0,HourDecimal[1], by = timedelay)
    sequ_final <- seq(HourDecimal[length(HourDecimal)],23.999, by = timedelay)
    listNumbers <- unlist(SeqNumbers)
    
    tabla_na <- c(sequ_ini, listNumbers, sequ_final)
    tabla_na <- unique(tabla_na)
    return(tabla_na)
}

#SR_hourlytodaily converts hourly data into daily data.
#Arguments  - Data_SR : Data SR per day
#           - Hour_Decimal: Hour in decimal
#           - k : elements for mobile average 
#Return     - Data of SR per day with units energy (Wh/m^2) 

#info http://www.trisolar.ca/Content/Insolation.php
#     http://solarelectricityhandbook.com/solar-irradiance.html


SR_hourlytodaily <- function (data_perday, k=3)
{
    #order
    data_perday <- data_perday[order(data_perday$Hour),]
    
    #Values
    data_SR <- data_perday$Value
    
    Hour_Decimal <- data_perday$HourDecimal
    
    #Fill values with moving average with 3 values
    data_SR <- na.ma(data_SR, k = k, weighting = "simple")
    
    
    #Area under curve
    values_SR <- data_SR
    hour_SR <- unlist(Hour_Decimal)
    AUC <- sum(diff(hour_SR)*rollmean(values_SR,2))
    
    #kWh/m^2/day 
    AUC <- AUC/1000
    
    #kWh/m^2/day to calories/cm^2/day
    #kWh/m^2/day = 859824 calories/m^2/day = 85.98 * calories/cm^2/day
    
    AUC <- AUC*85.98
    
    
    return(AUC)
}

#info_station compute the overall results after daily data 
#Arguments      file. Hourly data 
#               percentage. percentage for acceptable day
#               time. If data is hourly so time = 1. If data is daily so time = 0

#results <- lapply(list.files(path= "./AfterDailyControl_Data"), info_station, percentage=Percentage, typefile = 1, sepa= separt, time =2)

info_station<- function(file, percentage, time, sepa )
{
    station_name <- split_name(file)[1]
    variable <- split_name(file)[2]
    
    #read_file <- convert_units(file, date_format="%Y-%m-%d", sepa )
    read_file <- read.table(paste0(here(), "/AfterDailyControl_Data/",file), header = T)
    read_file$Value <- as.double(read_file$Value)
    read_file$Date  <- as.Date(as.character(read_file$Date ), format = "%Y-%m-%d" )
    
    
    days <- sort(read_file$Date)
    
    star_day<- days[1]
    end_day <- days[length(days)]
    
    numbe_days <- as.Date(as.character(end_day), format="%Y-%m-%d")-
        as.Date(as.character(star_day), format="%Y-%m-%d")
    
    numbe_days <- as.double(numbe_days)
    
    
    if(time == 1)
    {
        acceptable_days <- Check_Day_Station(file, percentage)
        acceptable_days <- length(acceptable_days)
    
         result <- data.frame(station_name, variable, star_day, end_day, numbe_days, acceptable_days, percentage)
    }
    if (time == 2)
    {
    
    result <- data.frame(station_name, variable, star_day, end_day)
    }


    return(result)
}



#results <- lapply(list.files(path= "./AfterDailyControl_Data"), info_station, percentage=Percentage, typefile = 1, sepa= " ", time =2)
#lapply(list.files(path= "./AfterDailyControl_Data"), daily_control, daily_restric = Daily_restric, typefile = 1, sepa = separt, date_format = date_format )

#lapply(list.files(), daily_control, daily_restric = Daily_restric, typefile = 1, sepa = separt, date_format = date_format )


daily_control <- function (daily_restric, file, sepa, date_format )
{
  
  #Daily Restrictions
  daily_res <- daily_restric
  
  #Variable
  splitname <- split_name(file)
  variable <- splitname[2]  
  
  # #ReadFile
  #convert_units <- function(weatherdata, date_format="%Y%m%d", typefile, sepa)
  read_file <- convert_units(weatherdata=file , date_format=date_format, sepa= sepa )

  if( anyNA(read_file$Date)== TRUE)
  {
    stop('There is a problem with orginal or input of date format: ', file)
  }

  if(variable == "RH")
  {
    values_out <- which(read_file$Value < daily_res$RH[2] || read_file$Value > daily_res$RH[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "NE", ".txt")
    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }

  }

  if(variable == "TX")
  {
    values_out <- which(read_file$Value < daily_res$TX[2] || read_file$Value > daily_res$TX[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "CD", ".txt")
    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }

  }


  if(variable == "TM")
  {
    values_out <- which(read_file$Value < daily_res$TM[2] || read_file$Value > daily_res$TM[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "CD", ".txt")

    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }

  }

  if(variable == "SR")
  {
    values_out <- which(read_file$Value < daily_res$SR[2] || read_file$Value > daily_res$SR[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "CALCM2", ".txt")

    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }

  }

  if(variable == "P")
  {
    values_out <- which(read_file$Value < daily_res$P[2] || read_file$Value > daily_res$P[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "MM", ".txt" )

    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }

  }
  

  #write.table(read_file, paste0("./AfterDailyControl_Data/", file), row.names = FALSE)
  write.table(read_file, paste0(here(), "/AfterDailyControl_Data/", new_file), row.names = FALSE)
  #write.table(read_file, file, row.names = FALSE)
  #write.table(read_file, file, row.names = FALSE)
  file.copy(from=new_file, to ="../AfterDailyControl_Data")
  
  
  
  
  return(read_file)
  
}

#check_amount_NA works counting number of NA per day if data is daily.
#Arguments    - File with data daily


check_amount_NA <- function(file, porcentage)
{
  #porcentage NA
  porcentage <- 1 - porcentage
  
  #Read table 
  table <- read.table(paste0("./Original_Data/", file), header = TRUE)
  
  #Count days
  number_length  <- as.numeric(as.Date(as.character(table$Date[length(table$Date)]), format = date_format)- as.Date(as.character(table$Date[1]), format = date_format))
  
  
  
  #Count the NA
  number_NA <- sum(is.na(table$Value))
  #number_length <- length(table$Value)
  
  if(number_NA/number_length > porcentage)
  {
    result <- data.frame(split_name(file)[1], split_name(file)[2])
    
  }
  
  else
  {
    result <- NULL
  }  
  
  return (result)
  
}


#Check_All_Station_NA works for choosing stations with meet with porcentage
#Arguments     - List of all stations 
#Porcentage    - porcentage amount of NA

Check_All_Station_NA  <- function (listfiles, porcentage)
{
  
  
  result <- lapply(listfiles, check_amount_NA, porcentage = porcentage)
  result <- result[!sapply(result, is.null)]
  final_results <- do.call("rbind", result)  
  colnames(final_results) <- c("Station_Name", "Variable_Name")
  
  write.table(final_results, "./Results/Stations_Delete.txt", row.names = FALSE)
  return (final_results)
}


#Choose_stations_Daily chooses station with meets the condition NA
#Arguments    - file
#             - names_station
Choose_station_Daily <- function(file, names_station)
{
  if(!any(split_name(file)[1] %in% names_station ))
  {
    file.copy(from=file, to ="../AfterDailyControl_Data")
  }
  
}

#few_NA choose stations with few NA and it could use moving average
#Arguments          -file
#Percentaje         -percentage minimun
few_NA <- function (file, percentage)
{
  #Read table 
  table <- read.table(paste0("./Original_Data/", file), header = TRUE)
  
  
  #Count the NA
  number_NA <- sum(is.na(table$Value))
  number_length <- length(table$Value)
  
  if(number_NA/number_length < percentage)
  {
    result <- data.frame(split_name(file)[1], split_name(file)[2])
    
  }
  
  else
  {
    result <- NULL
  }  
  
  return (result)
  
}

#Check_All_Station_Few_NA works for knowing which stations have few NA.  
#listfiles      - list of files 
#               - percentage of missing values.
Check_All_Station_Few_NA  <- function (listfiles, percentage)
{
  
  
  result <- lapply(listfiles, few_NA, percentage = percentage)
  result <- result[!sapply(result, is.null)]
  final_results <- do.call("rbind", result)  
  colnames(final_results) <- c("Station_Name", "Variable_Name")
  write.table(final_results, "./Results/Stations_Few_NA.txt")
  return (final_results)
}


#clusterstations_longlat is for doing the cluster of stations according to latitude and longitude
#parameters       -file. It has information of latitude and longitute of each station.
#
#return           -clusters of nearest stations  

#Results_DailyControl.csv

clusterstations_long_lat <- function(file)
{
  
  #Read the file Daily Information
  info_station <- read.csv(paste0(getwd(), "/Results/", file), header = T)
  
  #latitude and longitude  
  table <- subset(info_station, Variable_Name == 'P')
  table <- table[,c("Station_Name", "Latitude", "Longitude", "Altitude")]
  table <- as.data.frame(table)
  
  #Convert data to a SpatialPointsDataFrame object
  xy <- SpatialPointsDataFrame(matrix(c(table$Latitude ,table$Longitude), ncol=2), data.frame(Station_Name = table$Station_Name), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  #use the distm for geodesic distance matrix
  mdist <- distm(xy)
  
  hc <- hclust(as.dist(mdist), method="complete")
  
  pdf("/Graphics/Cluster_Stations/Cluster.pdf")
  dev.off()
  
  return (xy)
   
}


#time_step calculates arrival time of signal to station.
#-Arguments: weatherdata = data per day with columns Date, Hour, Value and HourDecimal. This is output of put_format
#          : unit_hour = return hour format.  minutes = "mins", seconds= "secs". Default units = "mins" 
#-Return: delay time in minutes



time_step <- function(weatherdata, unit_hour = "mins")
{
  
  #The arrival time of signal is time more frequently of arrivals time.
  #Each time arrival is equal to diffence between t_i - t_i-1.
  
  weather_data <- as.data.frame(weatherdata)
  colnames(weather_data) <- c("Date","Hour","Value", "HourDecimal")
  
  
  #Convert Hour to Decimal
  decimal_hour <- as.difftime(weather_data$Hour, format="%H:%M:%S", units= unit_hour)
  
  
  #Time arrival of each signal 
  time_arrival <- diff(decimal_hour)
  
  #If there is only one data per day.
  if(length(time_arrival)==0)
  {
    time_more_frequ <- 0
  }
  else
  {
    #Time more frequently
    #frecuen <- sort(table(time_arrival), drecreasing = TRUE)
    time_more_frequ <- as.numeric(names(which.max((table(time_arrival)))))
    
    #time_more_frequ <- as.numeric(names(frecuen)[1])
  }
  return(time_more_frequ)
}  




#hour_solarnoon_sunrise function calculates solar noon and sunrise hour according to position (latitude and longitude) and day
#-Arguments: day, lat: latitude, long: longitude, timezo: time zone (ID) see OlsonNames(), typeofhour = "sunrise" or typeofhour ="solarnoo". 
#-Return:  A list with hour solarnoo and sunrise.


hour_solarnoon_sunrise <- function (day, lat, long, timezo, typeofhour) 
{
  #format
  long <- long[1]
  lat <- lat[1]
  timezo <- as.character(timezo)[1]
  day <- as.character(day)
  
  #Calculates sunrise and soloarnoon hour
  portsmouth <- matrix(c(long, lat), nrow=1)
  for_date <- as.POSIXct(day)
  time_sunrise <- sunriset(portsmouth, for_date, direction="sunrise", POSIXct.out=TRUE)
  hour_suns <- as.POSIXlt(time_sunrise$time, timezo)
  
  
  time_solarnoon <- solarnoon(portsmouth, for_date, POSIXct.out=TRUE)
  hour_noon <- as.POSIXlt(time_solarnoon$time, timezo)
  
  
  #Return hour
  hour_suns <-strsplit(as.character(hour_suns), split=" ", fixed = TRUE)[[1]][2]
  hour_noon <-strsplit(as.character(hour_noon), split=" ", fixed = TRUE)[[1]][2]
  
  if(typeofhour == "sunrise")
  {
    result_hour <- hour_suns
  }
  
  if(typeofhour == "solarnoon")
  {
    result_hour <- hour_noon
  }
  
  
  return(result_hour)
  
}

#The put_formatfunction  has two objectives. The first is to check that the file has a correct 
#format name nombredelaestacion_variable e.g. 12313_P.txt. 
#The second is to put format the variables, as follows: DATE tipo day, Hour tipo hora, Value tipo double.

#-Argument: is weather file.
#           typefile = Specific Folder
#           sepa = separator of file 
#-Return: weather file with format

#convert_units <- function(weatherdata, date_format="%Y%m%d", typefile, sepa)
put_format<- function(originalfile, date_format="%Y%m%d", sepa)
{
  
  #Check format name file. The name is composed by two parts. The first is 
  #name station that has only numbers, and the second name variable.
  split_name <- split_name(originalfile)
  
  
  #Those are variables for weather data.
  variablesnames <- c("TX","TM","RH","SR","P")
  
  #Those are units for variables.
  #CD = Celsius Degree
  #FD = Falherein Degree
  #MM = Milliliters 
  #NE = A number between 0 and 100
  #WAM2 = Watts per meter square
  #MJM2 = Megajoules per meter suare
  #CALCM2 = Calories per meter square
  
  variablesunits = c("CD", "FD","MM", "NE", "WAM2","MJM2", "CALCM2", "KWHM2", "WHM2") 
  
  if(all(str_detect(variablesnames, fixed(split_name[2]))== FALSE)== TRUE){stop('Not valid variable name : ', originalfile)}
  if(all(str_detect(variablesunits, fixed(split_name[3]))== FALSE)== TRUE){stop('Not valid unit : ', originalfile)}
  
  
  #Read file
  fileoriginal <- read.table(paste0(here(),"/Original_Data/",originalfile), header= TRUE, sep=sepa)
  
  
  #Check if file is daily or hourly
  #if it has two columns it is daily 
  #if it has three columns it is hourly
  
  #Define the variables names for hourly and daily
  variablesnames_hourly <- c("Date", "Hour", "Value")
  variablesnames_daily <- c("Date", "Value")
  
  # Check if the file has the correct number of columns and their names
  if(ncol(fileoriginal)==2)
  {
    
    if(all(colnames(fileoriginal) == variablesnames_daily)== FALSE){stop('There is a problem with name of columns :', originalfile)}
    
  }
  
  if(ncol(fileoriginal)==3)
  {
    
    if(all(colnames(fileoriginal) == variablesnames_hourly)== FALSE){stop('There is a problem with name of columns :', originalfile)}
    
    fileoriginal$HourDecimal  <- lapply(fileoriginal$Hour, function (x) hour_to_number (x))
    
    #Control of the hour format: if am or pm are detected, 
    #the code converts the hour to 24h format. The target format includes seconds.
    
    if(any(grepl("m",fileoriginal$Hour)))
    {
      fileoriginal$Hour <- format(strptime(fileoriginal$Hour,"%I:%M %p"),'%H:%M:%S')
    }
    
    else
    {
      #To convert 24 hour format  
      fileoriginal$Hour <- format(strptime(fileoriginal$Hour,"%H:%M"),'%H:%M:%S')
      
    }
  }
  
  
  fileoriginal$Date <- as.Date(as.character(fileoriginal$Date), format= date_format)
  fileoriginal$Value <- as.double(as.character(fileoriginal$Value))
  
  
  fileoriginal <- as.data.frame(fileoriginal)
  return(fileoriginal)
}



#The convert_units function converts units of the original data set to standard units. 
#-Arguments: weather data
#-Return: weather data with standerized units
#lapply(list.files(here("Original_Data")), daily_control, daily_restric = Daily_restric, typefile = 1, sepa = separt, date_format = date_format )
convert_units <- function(weatherdata, date_format="%Y%m%d", sepa)
{
  #Read file
  data <- put_format(weatherdata,date_format, sepa)
  
  #Extract variable names and units
  split_name <- split_name(weatherdata) 
  
  #SR 
  if(split_name[3]=='MJM2')
  {
    data$Value <- data$Value/23.88
  }
  
  #Temperatures: target units is celcius degrees
  if(split_name[3]=='FD')
  {
    data$Value <- (data$Value-32)/1.8
  }
  
  #If the units are kWHM2 (kilowatts per meter square) 
  if(split_name[3]=='KWHM2')
  {
    data$Value <- (1000*data$Value)*0.0858
  }
  
  #If the units are WHM2 (watts per meter square) 
  if(split_name[3]=="WHM2")
  {
    data$Value <- (data$Value)*0.0858
  }
  return(data)
  
}



