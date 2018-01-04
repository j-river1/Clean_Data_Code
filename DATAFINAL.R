#Read files of HR and SR
#Arguments listfiles is a list of files with pattern txt.
read_files <- function (listfiles)    
{
       
    files_merge <- lapply(listfiles,merge_tables)
    names_filemerge <- lapply(listfiles, function(x) {names = split_name(x) 
                                                         return (names)})
    names(files_merge) <- names_filemerge
    variable_names <- names(files_merge)
        
    save_files <- lapply(seq_along(files_merge), function(y,n,i){
                                                                name <- n[[i]]
                                                                write.table(y[[i]], file = paste0("../Final_Data/", name, ".txt" ))
                                                                }, y=files_merge, n=names(files_merge))
 
}



merge_tables <- function(namefile)
{
    #incomplete name
    file <- read.table(namefile, header= TRUE) 
    file$SR <- NULL
    file$RH <- NULL
    
    
    
    #File SR
    filename <- split_name(namefile)
    files_SR <- read.table(paste0("./SR/",filename, "_SR.txt"), header= TRUE)
    names(files_SR) = c("Date", "SR" ) 
    files_SR$Date <- NULL
    
    #File RH
    filename <- split_name(namefile)
    files_RH <- read.table(paste0("./RH/",filename, "_RH.txt"), header= TRUE)
    names(files_RH) = c("Date", "RH" ) 
    files_RH$Date <- NULL
   
    
    
    file_total <- cbind(file, files_SR, files_RH)                                                         
    
    return(file_total)
    
    
}