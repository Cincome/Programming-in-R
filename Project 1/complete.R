complete<- function(directory, id=1:332){
  nobs<-c()
  files_list <- list.files("specdata/specdata", full.names = TRUE)
  for (i in id){
  data<- read.csv(files_list[i])
  sum_complete<- sum(complete.cases(data))
  nobs<-c(nobs, sum_complete)
}
dat<- data.frame(id,nobs)
dat}