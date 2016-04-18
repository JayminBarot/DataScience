corr <- function(directory, threshold=0){
  sourcepath<- paste0(getwd(),"/",directory,collapse = " ")
  all_files <- list.files(sourcepath, full.names = TRUE)
  
  result <- vector(mode = "numeric",length = 0)
  res <- data.frame()
  for (i in 1:length(all_files)){
    
    files<- read.csv(all_files[i])
    
    tot<-sum((!is.na(files$sulfate)) & (!is.na(files$nitrate)))
    
    if (tot > threshold) {
      t1<- files[which(!is.na(files$sulfate)), ]
      t2<- t1[which(!is.na(t1$nitrate)), ]
      result<- c(result,cor(t2$sulfate, t2$nitrate))
      
    }
    
  }
  
  result
}