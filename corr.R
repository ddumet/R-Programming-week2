corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  correlation<-vector(mode="numeric")
  
  for (int in 1:332) {
    fileToRead<-paste(directory,"/",formatC(int, width=3, flag="0"),".csv", sep="")
    monitorReadings<-read.csv(fileToRead)
    if ( sum(complete.cases(monitorReadings$nitrate, monitorReadings$sulfate)) > threshold ) {
      correlation<-c(correlation, cor(monitorReadings[,"sulfate"], monitorReadings[,"nitrate"],use = "pairwise.complete.obs"))
    }
  }
  return(correlation)
}