corr<- function(directory, threshold=0){
  files_list <- list.files("specdata/specdata", full.names = TRUE)
  corr<-numeric()
  for (i in 1:length(files_list)){
    data <- read.csv(files_list[i], header=TRUE, colClasses=c("NULL", NA, NA, "NULL"))
    if (sum(complete.cases(data)) > threshold) {
      sulphateComplete <- data[complete.cases(data),]
      nitrateComplete <- data[complete.cases(data), ]
      corr <- c(corr , cor(data$sulfate,data$nitrate,use="complete.obs"))
  }
  }
  corr
}