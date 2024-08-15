## libraryload
library(stringr)
library(dplyr)

## File load
setwd("C:/Users/64223/Documents/GitHub/HeresyGames")
DirPlayerLog <- file.path(getwd(), "records/player matches")
DirGamelog <- file.path(getwd(), "records/games")

## Read item in list of files from  from Dir
readfileToDF <- function(index, mydir, fileList){
  return(as.data.frame(read.table(file = file.path(mydir, fileList[index]), sep = ',', header = TRUE, quote = "")))
}

## clean up a DF
removeQuotes <- function(x) gsub("\"", "", x)

## Purpose
##  1. read a directory of files
##  2. import those files
##  3. unpack all the files into a single DF
##  4. add the file name as a column
##  5. clean up that DF
manyToOne <- function(mypath){
  # setup
  myfiles <- list.files(mypath)
  filenames <- sapply(strsplit(myfiles,"\\."), `[`, 1)
  nfiles <- 1:length(myfiles)
  
  
  ## make it a DF 
  myDF <- lapply(nfiles, readfileToDF, mypath, myfiles)
  myDF <- do.call(rbind, myDF)
  myDF <- cbind(filenames, myDF)
  
  colnames(myDF) <- str_remove(colnames(myDF), "X")
  colnames(myDF) <- str_remove_all(colnames(myDF), "\\.")
  myDF <- myDF %>%
    mutate_if(is.character, removeQuotes)
  
  return(myDF)
}


playerDF <- manyToOne(DirPlayerLog)
gameDF   <- manyToOne(DirGamelog)


## write out to CSV
write.csv(playerDF
          , paste(DirPlayerLog, "/manyGames", ".csv", sep = "")
          , row.names=FALSE
)

write.csv(gameDF
          , paste(DirGamelog, "/manyGames", ".csv", sep = "")
          , row.names=FALSE
)

