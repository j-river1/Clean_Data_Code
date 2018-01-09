#format for random forest 

#match_files merge the files according their names
#Arguments      -listFiles list of files


match_files  <- function(listFiles, resumefile, type)
{
 
  filenames <- read.csv(resumefile, header=T)
  names_stations <- unique(filenames$Station_Name)
    
  #Grouping according names station
  files  <- lapply(names_stations, merge_files, listfiles = list.files(), type = type)

  return (files)
}

#merge_files merges files according to names
merge_files <- function (name, listfiles, type)
{
  name <- as.character(name)
  
  files <-  list.files(pattern= name)
  read_files <- lapply(files,label_for_files)
  tablaPerday <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Date", all = TRUE),read_files)
  
  namec <- paste0(name,".txt" )
  #write.table(tablaPerday, file = paste(getwd(),"Random_Forest", namec, sep =  "/"), row.names = FALSE, quote = FALSE, col.names = TRUE)
  if(type == 1)
  {
    write.table(tablaPerday, file = paste("../../RandomForest", namec, sep =  "/"), row.names = FALSE, quote = FALSE, col.names = TRUE)
  }
  if(type == 2)
  {
    write.table(tablaPerday, file = paste("../../Final_Data", namec, sep =  "/"), row.names = FALSE, quote = FALSE, col.names = TRUE)
  }
  
  return(tablaPerday)
}

#label_for_files puts label a files.

label_for_files <- function (name)
{
  file <- read.table(name, header=T)
  variable <-  split_name(name)[2]
  names(file) <- c("Date", variable)
    
  return(file) 
} 


move_files_SR_HR <- function ()
{
    list_files_SR <- list.files("../../AfterDailyControl_Data", pattern ="_SR_")
    list_files_RH <- list.files("../../AfterDailyControl_Data", pattern ="_RH_")
    
    lapply(list_files_SR, copy_paste_files)
    lapply(list_files_RH, copy_paste_files)
    
}

#paste_name works for delete the last part of name. e.g. Chihauaita_SR_WAM2 converts to Chihauaita_SR
paste_name <- function(namefile)
{
    split_na <-  split_name(namefile) [3] 
    name_comple <- gsub(paste0("_",split_na),"",namefile)
    return (name_comple)
}

#copy_extension works for making path of files
copy_paste_files <- function(namefile)
{
    name_comp <- paste_name(namefile)
    path_from <- paste0("../../AfterDailyControl_Data/", namefile)
    path_to <- paste0("../../Rmawgen/Files_By_Station/", name_comp)
    file.copy(path_from, path_to)

}





# w <- lapply(files, function(x){read.table(x, header=TRUE)})
# 
# lapply(name_estaciones, merge_files, listfiles =list.files() )


