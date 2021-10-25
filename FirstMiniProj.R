## CMSC 197 -2 
## First Mini Project
## by Jon Robien E. Jinon



## POLLUTANT MEAN
## example on how to run:
## >pollutantmean("specdata", "sulfate", 10:100)
## >[1] 2.715065
pollutantmean <- function(directory, pollutant, id = 1:332){
  pollutants = c()
  
  dataFiles = list.files(directory)
  
  for (i in id){
    fPath = paste(directory, "/", dataFiles[i], sep="")
    
    data = read.csv(fPath, header = TRUE)
    
    pollutants = c(pollutants, data[,pollutant])
    
  }
  
  pollutants_mean = mean(pollutants, na.rm = TRUE)
  
  pollutants_mean
  
}


## cOMPLETE
## example:
## >complete("specdata", 30:33)
## >    id  nobs
## >1   30  932
## >2   31  483
## >3   32  616
## >4   33  466

complete <- function(directory, id = 1:332){
  idList = c()
  
  nobsList = c()
  
  files = list.files(directory)
  
  for (i in id){
    fPath = paste(directory, "/", files[i], sep = "")
    
    data = read.csv(fPath, header = TRUE)
    
    completeCases = data[complete.cases(data),]
    
    idList = c(idList, i)
    
    nobsList = c(nobsList, nrow(completeCases))
  }
  
  data.frame(id = idList, nobs = nobsList)
}

## CORRELATIONS
##  example:
##  >corr("specdata", 30)

corr <- function(directory, threshold = 0){
  completeList = complete(directory, 1:322)
  
  compAboveThresh = subset(completeList, nobs > threshold)
  
  correlations <- vector()
  
  fileNames = list.files(directory)
  
  for (i in compAboveThresh$id){
    fPath = paste(directory, "/", fileNames[i], sep="")
    
    data = read.csv(fPath, header = TRUE)
    
    completeCases = data[complete.cases(data),]
    
    count = nrow(completeCases)
    
    if (count >= threshold){
      correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate))
    }
  }
  
  correlations
}

## Calculate 30-day Mortality Rate
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
head(outcome)

outcome [, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], 
     main = paste("Hospital 30-Day Death (Mortality) Rates from heart attack"),
     xlab = "Deaths",
     col = "lightblue"
    )



