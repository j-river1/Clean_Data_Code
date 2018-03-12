
#generate_missing_values generates values of all stations using rmawgen.
#Arguments    -ListFiles. Lisf of files with format for rmawgen 
#             -resumefile. Lsit with resumen all stations 

#generate_missing_values(list.files(), list.files()[1],  "PRECIPITATION")
generate_missing_values <- function (variable, choose_station)
{
    
    #station_info <- choose_stations(resumefile)
    #names
    name_TX <- paste0(getwd(), "/Rmawgen/", "TX.csv")
    TEMPERATURE_MAX <- read.csv(name_TX, header=T, check.names = FALSE)
    
    name_TM <- paste0(getwd(), "/Rmawgen/", "TM.csv")
    TEMPERATURE_MIN <- read.csv(name_TM, header =T, check.names = FALSE)
    
    name_P <- paste0(getwd(), "/Rmawgen/", "P.csv")
    PRECIPITATION <- read.csv(name_P, header =T, check.names = FALSE)

    
    if(variable=='TEMPERATURE_MAX')
    {
        #Name files
        #generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=1, manual=manual, choose_station = choose_station, year_min, year_max)
        generator_values <-  applying_rmwagen_2(TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=1, choose_station = choose_station)
    }
    
    if(variable=='TEMPERATURE_MIN')
    {
        #generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=2, manual=manual, choose_station = choose_station)
        generator_values <-  applying_rmwagen_2(TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=2, choose_station = choose_station)
    }    
    
    if(variable=='PRECIPITATION')
    {

        #generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=3, manual=manual, choose_station = choose_station)
        generator_values <-  applying_rmwagen_2(TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=3, choose_station = choose_station) 
         
    }
       
    
    return(generator_values)
}

#Applying rmawagen works rmwagen function 
#Arguments - info_station: dataframe with station name, start and  end date
#          - manual. This is way for matching station then using rmwagen.
#                    If manual is 1. It is automatic
#                    If manual is 2. User must enter stations for matching
#          - choose stations. Choose the stations for matching. It must be a vector

#graph_all (list.files(pattern = "\\.csv$"), "./Results/Results_DailyControl.csv", "TEMPERATURE_MAX", 'Temperatura_M?xima', manual = 2, choose_station = c(24,7))
applying_rmwagen <- function (info_station, TEMPERATURE_MAX, TEMPERATURE_MIN, PRECIPITATION, n_GPCA_iter = 10, n_GPCA_iteration_residuals =10, lag=2, p_prec = 3, p_test =2,menu, manual, choose_station, year_min, year_max)
{
    if (manual == 1)
    {
      #Arguments for rmwagen
       station <- as.vector(info_station$station_names)
       year_min <- as.Date(info_station$date_min, "%Y-%m-%d")
       year_max <- as.Date(info_station$date_max, "%Y-%m-%d")
       
    }  
  
    if(manual == 2)
    {
      #Read Station 
      #stations <- read.csv("./Results/Results_DailyControl.csv", header=TRUE)
      #stations <- read.csv(paste0(here(), "/Results/Clustering_Stations.csv"), header=TRUE)
      
      #stations <- subset(stations, Station_Name %in% c("11045010"))
      stations <- choose_station
    }

  #year_min <- as.numeric(format(year_min[1], "%Y"))
  #year_max <- as.numeric(format(year_max[1], "%Y"))
  
  year_min <- year_min
  year_max <- year_max
  #Start and End Data
  # Start_Data_Sta <- unique(year_min)
  # End_Data_Sta <- unique(year_max)
  
  
  All_data <- seq(as.Date(paste0(year_min,"-1-1")), as.Date(paste0(year_max, "-12-31")), by="days")
  
  

    
    
    generationTemperature <- ComprehensiveTemperatureGenerator(
        station=station,
        Tx_all=TEMPERATURE_MAX,
        Tn_all=TEMPERATURE_MIN,
        year_min=year_min,
        year_max=year_max,
        p=5,
        n_GPCA_iteration=n_GPCA_iter,
        n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
        sample="monthly"
    )
    
    if(menu == 1 )
    { 
        
        real_data <- generationTemperature$input$Tx_mes
        fill_data <-generationTemperature$out$Tx_gen
    } 
    
    if(menu == 2)
    {
        real_data <- generationTemperature$input$Tn_mes
        fill_data <-generationTemperature$out$Tn_gen
    }
    
    if(menu == 3)
    {
        #Error distribution. Check if precipitation distribuition is biased to zero.  
        median <- lapply(PRECIPITATION[station], median, na.rm =T)
        test <- lapply(median, function(x) { if (x < 1) {result <- TRUE} else {result <- FALSE}})
        
        if(any (test == TRUE))
        {
            warning("There is a problem with distribution of Precipitation. This is very biased to zero")
            PRECIPITATION[station] <- PRECIPITATION[station] + 4
            generation_prec <- ComprehensivePrecipitationGenerator(
                station=station,
                prec_all=PRECIPITATION,
                year_min=year_min,
                year_max=year_max,
                p= 3,
                n_GPCA_iteration= 10,
                n_GPCA_iteration_residuals= 0,
                sample = "monthly",
                no_spline = FALSE,
                nscenario = 20)
            
            real_data <- generation_prec$prec_mes - 4
            fill_data <- generation_prec$prec_gen - 4
            
        }
        
        else 
        {
            generation_prec <- ComprehensivePrecipitationGenerator(
                station=station,
                prec_all=PRECIPITATION,
                year_min=year_min,
                year_max=year_max,
                p= 3,
                n_GPCA_iteration= 10,
                n_GPCA_iteration_residuals= 0,
                sample = "monthly",
                no_spline = FALSE,
                nscenario = 20)
            
            real_data <- generation_prec$prec_mes
            fill_data <- generation_prec$prec_gen 
            
        }
        


    }
    
    result <- list(real_data= real_data, estimated_data = fill_data, date=All_data)
    
    return(result)
    
    
}




#Applying rmawagen works rmwagen function 
#Arguments - info_station: dataframe with station name, start and  end date
#          - manual. This is way for matching station then using rmwagen.
#                    If manual is 1. It is automatic
#                    If manual is 2. User must enter stations for matching
#          - choose stations. Choose the stations for matching. It must be a vector

#graph_all (list.files(pattern = "\\.csv$"), "./Results/Results_DailyControl.csv", "TEMPERATURE_MAX", 'Temperatura_M?xima', manual = 2, choose_station = c(24,7))
applying_rmwagen_2 <- function (TEMPERATURE_MAX, TEMPERATURE_MIN, PRECIPITATION, n_GPCA_iter = 10, n_GPCA_iteration_residuals =10, lag=2, p_prec = 3, p_test =2, choose_station, menu)
{

  station <- choose_station
  
  #Read year max and min of input
  year_min <- as.Date(as.character(variables$Start_date), format="%Y-%m-%d")
  year_min <- as.numeric(format(year_min, format="%Y"))
  
  
  year_max <- as.Date(as.character(variables$End_date), format="%Y-%m-%d")
  year_max <- as.numeric(format(year_max, format="%Y"))
  

  All_data <- seq(as.Date(paste0(year_min,"-1-1")), as.Date(paste0(year_max, "-12-31")), by="days")
  
  generationTemperature <- ComprehensiveTemperatureGenerator(
    station=station,
    Tx_all=TEMPERATURE_MAX,
    Tn_all=TEMPERATURE_MIN,
    year_min=year_min,
    year_max=year_max,
    p=5,
    n_GPCA_iteration=n_GPCA_iter,
    n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
    sample="monthly"
  )
  
   if(menu == 1 )
   {

     real_data <- generationTemperature$input$Tx_mes
     fill_data <-generationTemperature$out$Tx_gen
   }

   if(menu == 2)
  {
    real_data <- generationTemperature$input$Tn_mes
    fill_data <-generationTemperature$out$Tn_gen
  }

  if(menu == 3)
  {
    #Error distribution. Check if precipitation distribuition is biased to zero.
    median <- lapply(PRECIPITATION[station], median, na.rm =T)
    test <- lapply(median, function(x) { if (x < 1) {result <- TRUE} else {result <- FALSE}})

    if(any (test == TRUE))
    {
      warning("There is a problem with distribution of Precipitation. This is very biased to zero")
      PRECIPITATION[station] <- PRECIPITATION[station] + 4
      generation_prec <- ComprehensivePrecipitationGenerator(
        station=station,
        prec_all=PRECIPITATION,
        year_min=year_min,
        year_max=year_max,
        p= 3,
        n_GPCA_iteration= 10,
        n_GPCA_iteration_residuals= 0,
        sample = "monthly",
        no_spline = FALSE,
        nscenario = 20)

      real_data <- generation_prec$prec_mes - 4
      fill_data <- generation_prec$prec_gen - 4

    }

    else
    {
      generation_prec <- ComprehensivePrecipitationGenerator(
        station=station,
        prec_all=PRECIPITATION,
        year_min=year_min,
        year_max=year_max,
        p= 3,
        n_GPCA_iteration= 10,
        n_GPCA_iteration_residuals= 0,
        sample = "monthly",
        no_spline = FALSE,
        nscenario = 20)

      real_data <- generation_prec$prec_mes
      fill_data <- generation_prec$prec_gen

    }



  }

  result <- list(real_data= real_data, estimated_data = fill_data, date=All_data)

  return( result )
  
  
}




















