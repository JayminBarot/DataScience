complete <- function (directory,id = 1:332){
  sourcepath<- paste0(getwd(),"/",directory,collapse = " ")
  all_files <- list.files(sourcepath, full.names = TRUE)
  
  mydata <- data.frame()
  res <- data.frame()
  
  for(i in id){
    mydata <-  read.csv(all_files[i])
    cmplt<- data.frame(i, sum(complete.cases(mydata)))
    res <- rbind(res, cmplt)
  }
  colnames(res)<-c("id","nobs")
  res
}