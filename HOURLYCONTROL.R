

# Information about time zone https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

#The split_name function works split name file
#-Argument: name file
#-Return: split of name file 

split_name <- function(filename)
{
    split_name <- unlist(strsplit(filename, split='_', fixed=TRUE))
    split_name <- gsub("txt","",gsub("[[:punct:]]","",split_name) )
    
    return(split_name)
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
convert_units <- function(weatherdata, date_format="%Y%m%d", typefile, sepa)
{
    #Read file
    data <- put_format(weatherdata,date_format, typefile, sepa)
    
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

#The limits_TXTM_RH function works for creating limits from input file. 
#-Arguments: restricfile = restriction file 
#          : weatherdata = original data.
#-Return: weather data with limits.

limits_TXTM_RH  <- function(restricfile, weatherdata)
{
    #Read restriction file
    #data_restri <- read.csv(restricfile)
    data_restri <- restricfile
    
    #Read weather data
    data_weather <- convert_units(weatherdata) 
    
    
    #Delete NA
    data_weather <- delete_NA(data_weather, "Value")
    
    #Extract variable names 
    split_name <- split_name(weatherdata) 
    
    
    #TX
    if(split_name[2]=='TX')
    {
        data_weather <- data_weather[data_weather$Value >= data_restri$TX[2] & data_weather$Value <= data_restri$TX[1], ]
    }
    
    #TM
    if(split_name[2]=='TM')
    {
        data_weather <- data_weather[data_weather$Value >= data_restri$TM[2] & data_weather$Value <= data_restri$TM[1], ]
    }
    
    #RH
    if(split_name[2]=='RH')
    {
        data_weather <- data_weather[data_weather$Value>= data_restri$RH[2] & data_weather$Value<=data_restri$RH[1], ]
    }
    
    return(data_weather)
    
}

#limits_SR works puts the limits for solar irridance (SR) per hour.
#-Arguments: SR data,  restriction file.
#-Return: SR with limits.


limits_SR <- function (weatherdata, restricfile)
{
    #Extract data with units
    data_SR  <- convert_units (weatherdata)
    
    
    #Delete row with NA in Value  
    data_SR <- delete_NA(data_SR, "Value")
    
    #Extract Longititude, Latitude and Time Zone
    #data_Lon_Lati <- read.csv(restricfile)
    data_Lon_Lati  <- restricfile
    
    #Divide file per days
    divi_day <- divide_by_day(data_SR)
    
    #A list with date day 
    divi_day <- unique(names(divi_day))
    
    #Sunrise hours
    sunrise_hours <- lapply (divi_day, hour_solarnoon_sunrise, lat = data_Lon_Lati$LAT, long = data_Lon_Lati$LONG, timezo = data_Lon_Lati$TZ, "sunrise")  
    
    #Solarnoon hours
    solarno_hours <- lapply (divi_day, hour_solarnoon_sunrise, lat = data_Lon_Lati$LAT, long = data_Lon_Lati$LONG, timezo = data_Lon_Lati$TZ, "solarnoon") 
    
    
    #Match  days with hour sunrise 
    hourshine_day <- data.frame(divi_day,unlist(sunrise_hours))
    colnames(hourshine_day) <- c("Date", "HourShine")
    hourshine_day$Date <- as.Date(as.character(hourshine_day$Date), format="%Y-%m-%d")
    data_with_shinehour <- merge(data_SR,hourshine_day, by = "Date", all= TRUE ) 
    
    
    #Match  days with hour hour_solarnoon and sunshine 
    hoursolar_noo <- data.frame(divi_day,unlist(solarno_hours))
    colnames(hoursolar_noo) <- c("Date", "HourNoon")
    hoursolar_noo$Date <- as.Date(as.character(hoursolar_noo$Date), format="%Y-%m-%d")
    data_hourshine_noonhour <- merge(data_with_shinehour,hoursolar_noo, by = "Date", all= TRUE ) 
    
    #Convert from hour to decimal
    data_hourshine_noonhour$HourShine <- lapply(data_hourshine_noonhour$HourShine, function (x) hour_to_number (x))
    data_hourshine_noonhour$HourNoon  <- lapply(data_hourshine_noonhour$HourNoon, function (x) hour_to_number (x))
    
    #Find maximun radiation solar per hour
    data_hourshine_noonhour$MaxRadiac <- mapply(function (h, k , x, y, z) parabola_per_day(h, k , x, y, z), h = data_hourshine_noonhour$HourNoon, k=data_Lon_Lati$SR[1], x=data_hourshine_noonhour$HourShine, y=data_Lon_Lati$SR[2], z=data_hourshine_noonhour$HourDecimal)
    
    #Find values meet condition that Value less than MaxRadiac
    data_hourshine_noonhour <- data_hourshine_noonhour[data_hourshine_noonhour$Value <= data_hourshine_noonhour$MaxRadiac, ]
    
    
    #Return data with limits
    
    data_hourshine_noonhour <- data_hourshine_noonhour[, c("Date", "Hour", "Value", "HourDecimal")]
    data_hourshine_noonhour$Hour <- unlist(data_hourshine_noonhour$Hour)
    
    data_hourshine_noonhour <- as.data.frame(data_hourshine_noonhour)
    
    return(data_hourshine_noonhour)
    
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

#divide_by_day  function divides file y
#-Arguments: weather file with hourly limits .
#-Retun: list with date and data daily

divide_by_day <- function (weather_data)
{
    split_data <- split(weather_data, weather_data$Date)
    return(split_data)
}

#parabola_per_day function calculates point in the parabola.   
#-Arguments: vertex (h,k) = (vertex_h, vertex_k ) 
#            point  (x,y) = (pointsunrise_x, pointsunshine_y). pointsunshine_y should be zero. Irradiacion solar is zero at sunshine  
#            pointval
#-Retun: Maximum irradiation solar according to hour.


parabola_per_day <- function (vertex_h, vertex_k, pointsunshine_x, pointsunshine_y, pointvalu)
{
    # The parabola equation is y = a(x - h)^2 + k
    # a = (y - k)/(x - h)^2
    
    a <- (pointsunshine_y - vertex_k)/(pointsunshine_x - vertex_h)^2
    y <- a*(pointvalu- vertex_h)^2 + vertex_k
    
    #Values before sunrise and after sunset will be close to zero
    y[y<=0] <- 2
    
    return(y)  
}  


#limits_P  puts the limits for precipation (P) per hour.
#-Arguments: P data,  restriction file.
#-Return: P with limits.

limits_P <- function (weatherdata)
{
    #Extract data with units
    data_P  <- convert_units (weatherdata)
    
    
    data_P <- delete_NA(data_P, "Value")
    
    #Divided by day
    divi_day <- divide_by_day(data_P)
    names_day <- unique(names(divi_day))
    
    #Compute  time step
    delaytime <- lapply(divi_day, function (x) time_step (x))
    
    #Date with date per day 
    date_delay <- data.frame(names_day,unlist(delaytime))
    colnames(date_delay) <- c("Date", "Time_Step")
    date_delay$Date <- as.Date(as.character(date_delay$Date), format="%Y-%m-%d")
    data_delaytime <- merge(data_P,date_delay, by = "Date", all= TRUE ) 
    
    #Compute max precipitation 
    data_delaytime$Max_prec <- lapply(data_delaytime$Time_Step, function (x) 40*x^(log(25)/log(1440)))
    
    
    data_delaytime <- data_delaytime[data_delaytime$Value < data_delaytime$Max_prec, ]
    data_delaytime <- data_delaytime[, c("Date", "Hour", "Value", "HourDecimal", "Time_Step")]
    data_delaytime <- as.data.frame(data_delaytime)
    
    return(data_delaytime)
}

#delete_NA delete rows that contains NA in an specif column (Variable)
#Arguments: -weatherdata, original weather data
#           -Variable, varible name
#Return   : Data without NA in Variable name.

delete_NA <- function (weatherdata, Variable)
{
    
    index <- complete.cases(weatherdata[, Variable])
    weatherdata <- weatherdata[index, ]
    return(weatherdata)
}

plot_original_aftercleaning <- function (originaldata, aftercleaningdata, title, units_variable, location_legend)
{
    
    plot(originaldata$HourDecimal, originaldata$Value, ylim=range(c(originaldata$Value,aftercleaningdata$Value)), col= "red", main = title, xlab= "Horas", ylab = units_variable)
    par(new = TRUE)
    plot(aftercleaningdata$HourDecimal, aftercleaningdata$Value, ylim=range(c(originaldata$Value,aftercleaningdata$Value)), axes = FALSE, xlab = "", ylab = "", col= "blue")
    legend(location_legend, c("Datos_Limpios","Datos_Originales"),lty=1, col=c("blue","red"), bty='n', cex=.75)
    
}



#hour_to_number converts hours into number between 0 and 23
#-Arguments: hourformat: Hour in format character 
#-Return: A number between 0 and 23.
hour_to_number <- function(hourformat)
{
    hourformat <- as.character(hourformat)
    split_hour <- strsplit(hourformat,":")
    hora_number <- as.numeric(split_hour[[1]])
    hora_number <- as.numeric(split_hour[[1]][1])+as.numeric(split_hour[[1]][2])/60
    return(hora_number)
    
} 

#results function shows results after and before of hourly control
#-Arguments: originalfil.    Original Data. 
#          : aftercleaning.  Data after hourly control 
#-Return: result.  A vector with station name, variable name, size original data,
#                  and size data after hourly control    

results <- function(originaldata, restricfile, typefile, sepa)
{
    station_name <- split_name(originaldata)[1]
    variable_names <- split_name(originaldata)[2]
    
    #Size original data
    original_size <- length(put_format(originaldata,typefile, sepa))$Value
    
    #Extract Variable Name
    if(variable_names =='P')
    {
        aftercleaning <- limits_P(originaldata)
        aftercleaning <- aftercleaning[,c("Date", "Hour", "Value")]
        aftercleaning_length <- length(limits_P(originaldata)$Value)
        diference <- original_size -  aftercleaning_length
        
    }
    if(variable_names =='SR')
    {
        aftercleaning <- limits_SR(originaldata, restricfile)
        aftercleaning <- aftercleaning[,c("Date", "Hour", "Value")]
        aftercleaning_length <- length(limits_SR(originaldata, restricfile)$Value)
        diference <- original_size -  aftercleaning_length
        
    }
    
    if(variable_names =='TX' | variable_names =='TM' | variable_names =='RH')
    {
        aftercleaning <- limits_TXTM_RH(restricfile, originaldata)
        aftercleaning <- aftercleaning[,c("Date", "Hour", "Value")]
        aftercleaning_length <- length(limits_TXTM_RH(restricfile, originaldata)$Value)
        diference <- original_size -  aftercleaning_length
    }
    
    result <- data.frame(station_name, variable_names, original_size, aftercleaning_length, diference)
    #colnames(results) <- c("Station_Name", "Variable_Name", "OriginalData_Size", "CleanData_Size", "ErrorData_Size")
    name <- as.character(originaldata)
    originaldata <- paste0("..", "/", "AfterHourlyControl_Data", "/", name )
    
    write.table (aftercleaning, file = originaldata, row.names = FALSE, quote = FALSE, sep = "\t", col.names = TRUE)
    
    return(result)
    
}

#move_files_txt moves txt files according to path 
#Arguments:   from = origin path 
#               to = destination path  
move_files_txt <- function(from, to, format ="\\.txt$")
{
    
    files_txt <- list.files(path = from,pattern = format)
    file.copy(from = files_txt, to= to) 
    file.remove(files_txt) 
    
}

