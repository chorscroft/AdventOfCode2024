# Advent of Code Day 4
# Part 1

## Set the line length
areaSize <- 140
## Read in the file
mydata<-read.fwf("Data/day04.txt",rep(1,areaSize),comment.char="")



## Initialise the count
countXmas<-0

## Loop over each datapoint
for(i in 1:nrow(mydata)){
  for (j in 1:nrow(mydata)){
    ## if there is an X, check for XMAS in each direction
    if(mydata[i,j]=="X"){
      #N
      if(i>=4 && mydata[i-1,j]=="M" && mydata[i-2,j]=="A" && mydata[i-3,j]=="S"){
        countXmas<-countXmas+1
      }
      #NE
      if(i>=4 && j<=areaSize-3 && mydata[i-1,j+1]=="M" && mydata[i-2,j+2]=="A" && mydata[i-3,j+3]=="S"){
        countXmas<-countXmas+1
      }
      #E
      if(j<=areaSize-3 && mydata[i,j+1]=="M" && mydata[i,j+2]=="A" && mydata[i,j+3]=="S"){
        countXmas<-countXmas+1
      }     
      #SE
      if(i<=areaSize-3 && j<=areaSize-3 && mydata[i+1,j+1]=="M" && mydata[i+2,j+2]=="A" && mydata[i+3,j+3]=="S"){
        countXmas<-countXmas+1
      }     
      #S
      if(i<=areaSize-3 && mydata[i+1,j]=="M" && mydata[i+2,j]=="A" && mydata[i+3,j]=="S"){
        countXmas<-countXmas+1
      } 
      #SW
      if(i<=areaSize-3 && j>=4 && mydata[i+1,j-1]=="M" && mydata[i+2,j-2]=="A" && mydata[i+3,j-3]=="S"){
        countXmas<-countXmas+1
      } 
      #W
      if(j>=4 && mydata[i,j-1]=="M" && mydata[i,j-2]=="A" && mydata[i,j-3]=="S"){
        countXmas<-countXmas+1
      } 
      #NW
      if(i>=4 && j>=4 && mydata[i-1,j-1]=="M" && mydata[i-2,j-2]=="A" && mydata[i-3,j-3]=="S"){
        countXmas<-countXmas+1
      }
    }
  }
}

## Read out the result
countXmas


# Part 2

## Initialise the count

countXmas<-0
## Loop each internal datapoint
for(i in 2:(nrow(mydata)-1)){
  for (j in 2:(nrow(mydata)-1)){
    ## if there is an A, check for MAS in both directions
    if(mydata[i,j]=="A"){
      if(((mydata[i-1,j-1]=="M" && mydata[i+1,j+1]=="S") || (mydata[i+1,j+1]=="M" && mydata[i-1,j-1]=="S")) &&
         ((mydata[i-1,j+1]=="M" && mydata[i+1,j-1]=="S") || (mydata[i+1,j-1]=="M" && mydata[i-1,j+1]=="S"))){
        countXmas<-countXmas+1
      }
    }
  }
}

## Read out the result
countXmas

