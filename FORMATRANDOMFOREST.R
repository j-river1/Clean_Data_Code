
#match_files merge the files according their names
#Arguments      -listFiles list of files


match_files  <- function(type)
{
  
  slitingnames <- split_name(list.files(here("Rmawgen", "Files_By_Station")))
  length <- seq(1, length(slitingnames ), by = 3)
  names_stations <- unique(split_name(list.files(here("Rmawgen", "Files_By_Station")))[length])

  files  <- lapply(names_stations, merge_files, listfiles = list.files(here("Rmawgen", "Files_By_Station")), type = type)
  return (files)

}

#merge_files merges files according to names
merge_files <- function (name, listfiles, type)
{
  name <- as.character(name)
  
  files <-  list.files(here("Rmawgen", "Files_By_Station"), pattern= name)
  read_files <- lapply(files,label_for_files)
  tablaPerday <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Date", all = TRUE),read_files)
  
  namec <- paste0(name,".txt" )

  if(type == "RandomForest")
  {
    write.table(tablaPerday, file = paste0(here("RandomForest"), "/", namec), row.names = FALSE, quote = FALSE, col.names = TRUE)
  }
  if(type == "Final_Data")
  {
    write.table(tablaPerday, file = paste0(here("Final_Data", namec)), row.names = FALSE, quote = FALSE, col.names = TRUE)
  }
  
  return(tablaPerday)
}

#label_for_files puts label a files.

label_for_files <- function (name)
{
  file <- read.table(paste0(here("Rmawgen", "Files_By_Station"), "/", name), header=T)
  variable <-  split_name(name)[2]
  names(file) <- c("Date", variable)
  
  return(file) 
} 

#move_files_SR_HR works for copying data from AfterDailyControl_Data to  Files_By_Station

move_files_SR_HR <- function ()
{

  list_files_SR <- list.files(here("AfterDailyControl_Data"), pattern ="_SR_")
  list_files_RH <- list.files(here("AfterDailyControl_Data"), pattern ="_RH_")
  
  
  for  (i in 1: length(list_files_SR))
  {
    path_from <- paste0(here("AfterDailyControl_Data"), "/", list_files_SR[i])
    path_to <- paste0(here("Rmawgen", "Files_By_Station"), "/",list_files_SR[i])
    file.copy(path_from, path_to)
    
  }
  
  for  (i in 1: length(list_files_RH))
  {
    name_comp <- list_files_RH[i]
    path_from <- paste0(here("AfterDailyControl_Data"), "/", list_files_RH[i])
    path_to <- paste0(here("Rmawgen", "Files_By_Station"), "/",list_files_RH[i])
    file.copy(path_from, path_to)
    
  }
  
}




#The split_name function works split name file
#-Argument: name file
#-Return: split of name file 

split_name <- function(filename)
{
  split_name <- unlist(strsplit(filename, split='_', fixed=TRUE))
  split_name <- gsub("txt","",gsub("[[:punct:]]","",split_name) )
  
  return(split_name)
}





