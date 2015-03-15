pollutantmean<-function(directory, pollutant, ID=1:332) {   
  files_list <- list.files("specdata/specdata", full.names = TRUE)        
  dat <- data.frame()
  for (i in ID){
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  if(pollutant=='sulfate'){
    mymean <- mean(dat[ ,2], na.rm = TRUE)
    mymean
  }
  else {
    mymean2 <- mean(dat[ ,3], na.rm = TRUE)
    mymean2
  }
}
