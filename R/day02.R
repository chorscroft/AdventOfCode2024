# Advent of Code Day 2
# Part 1

## Create a function to check if a report fails the safety checks
checkSafety<-function(report){
  ## check if report is ascending or descending
  if(report[1]>report[2]){
    dir<-"desc"
  } else {
    dir<-"asc"
  }
  ## assume report won't fail the checks
  fail<-F
  ## loop over each level in the report
  for(i in 1:(length(report)-1)){
    ## if the gap is too big or the numbers are the same, fail the safety check
    if(fail==F && abs(report[i]-report[i+1])>3 || report[i]==report[i+1]){
      fail<-T
    }
    ## if all levels aren't going in the same direction, fail the safety check
    if (fail==F && dir == "desc"){
      if (report[i]<report[i+1]){
        fail<-T
      }
    }
    if (fail==F && dir == "asc"){
      if (report[i]>report[i+1]){
        fail<-T
      }
    }
  }
  ## return the result
  return(fail)
}


## Create connection to file
mydata<-file("Data/day02.txt","r")

## Initialise count of safe reports
safeCount<-0

## Loop over each line in the file
while (TRUE){
  ## get next line
  line = readLines(mydata, n = 1)
  ## if blank, stop reading lines
  if (length(line) == 0){
    break
  } else {
    ## split line into a numeric vector
    report<-as.numeric(unlist(strsplit(line," ")))
    ## run safety checks on the report
    fail<-checkSafety(report)
    ## if it doesn't fail, then iterate the safe report count
    if (fail==F){
      safeCount<-safeCount+1
    }
  }
}
## Close connect to the file
close(mydata)

## Read out the result
safeCount



# Part 2

## Create connection to file
mydata<-file("Data/day02.txt","r")

## Initialise count of safe reports
safeCount<-0

## Loop over each line in the file
while (TRUE){
  ## get next line
  line = readLines(mydata, n = 1)
  ## if blank, stop reading lines
  if (length(line) == 0){
    break
  } else {
    ## split line into a numeric vector
    report<-as.numeric(unlist(strsplit(line," ")))
    ## run safety checks on the report
    fail<-checkSafety(report)
    ## if it doesn't fail, then iterate the safe report count
    if (fail==F){
      safeCount<-safeCount+1
    }
    
    ## If it does fail, try removing each level in turn to see if that passes
    if(fail==T){
      for(i in 1:length(report)){
        ## remove one level
        tempRep <- report[-i]
        ## run safety checks
        tempFail<-checkSafety(tempRep)
        ## if it doesn't fail, then iterate the safe report count
        if (tempFail==F){
          safeCount<-safeCount+1
          ## skip to the next report
          break
        }
      }
    }
  }
}
## Close connect to the file
close(mydata)

## Read out the result
safeCount


