  pollutantmean <- function (directory, pollutant,id =1:332)
    {
    sourcepath<- paste0(getwd(),"/",directory,collapse = " ")
      all_files <- list.files(sourcepath, full.names = TRUE)
      mydata <- data.frame()
      for (i in id) {
        mydata <- rbind(mydata, read.csv(all_files[i]))
      }
    
    mean(mydata[, pollutant], na.rm = TRUE )
    
  }