# Advent of Code Day 6
# Part 1

## Read in file
fileName<-"Data/day06.txt"
areaSize <- 130
mydata<-read.fwf(fileName,rep(1,areaSize),comment.char="")

## Find the start location
i<-1
j<-1
while(T){
  if(mydata[i,j]=="^"){
    break
  } else {
    i<-i+1
    if(i>areaSize){
      i<-1
      j<-j+1
    }
  }
}
startLoc<-c(i,j)
## Initialise the current location
currLoc<-startLoc
## Set the ^ to a .
mydata[i,j]<-"."

## Initialise the map of visitied locations
map<-matrix(F,nrow=areaSize,ncol=areaSize)
map[currLoc[1],currLoc[2]]<-T

## Initialise the original direction
dir<-"N"

## Step until leave the area
while(T){
  if(dir=="N"){
    if(currLoc[1]-1==0){
      break
    } else if(mydata[currLoc[1]-1,currLoc[2]]=="."){
      map[currLoc[1]-1,currLoc[2]]<-T
      currLoc[1]<-currLoc[1]-1
    } else if(mydata[currLoc[1]-1,currLoc[2]]=="#"){
      dir<-"E"
    }
  } else if(dir=="S"){
    if(currLoc[1]+1>areaSize){
      break
    } else if(mydata[currLoc[1]+1,currLoc[2]]=="."){
      map[currLoc[1]+1,currLoc[2]]<-T
      currLoc[1]<-currLoc[1]+1
    } else if(mydata[currLoc[1]+1,currLoc[2]]=="#"){
      dir<-"W"
    }
  } else if(dir=="W"){
    if(currLoc[2]-1==0){
      break
    } else if(mydata[currLoc[1],currLoc[2]-1]=="."){
      map[currLoc[1],currLoc[2]-1]<-T
      currLoc[2]<-currLoc[2]-1
    } else if(mydata[currLoc[1],currLoc[2]-1]=="#"){
      dir<-"N"
    }
  } else if(dir=="E"){
    if(currLoc[2]+1>areaSize){
      break
    } else if(mydata[currLoc[1],currLoc[2]+1]=="."){
      map[currLoc[1],currLoc[2]+1]<-T
      currLoc[2]<-currLoc[2]+1
    } else if(mydata[currLoc[1],currLoc[2]+1]=="#"){
      dir<-"S"
    }
  }
}

## Return the number of visited locations
sum(map)

# Part 2

## Store the original map of visited locations and original dataset
old_map<-map
orig_mydata<-mydata

## Initialise a count of times they get stuck in a loop
loopCount<-0

## Loop through each location the obstacle can be put, using only the
## locations that were visited
obsi<-1
obsj<-1
while(T){
  old_map[obsi,obsj]<-F
  while(old_map[obsi,obsj]==F){
    obsi<-obsi+1
    if(obsi>areaSize){
      obsj<-obsj+1
      obsi<-1
    }
    if(obsj>areaSize){
      break
    }
    if(obsi==startLoc[1] && obsj==startLoc[2]){
      obsi<-obsi+1
    }
  }
  if(obsj>areaSize){
    break
  }
  
  ## put an obstacle in the data
  mydata<-orig_mydata
  mydata[i,j]<-"."
  mydata[obsi,obsj]<-"#"
  
  ## initiliase current location
  currLoc<-startLoc
  
  ## keep a map of each time a location is visited from each direction
  mapN<-matrix(F,nrow=areaSize,ncol=areaSize)
  mapE<-matrix(F,nrow=areaSize,ncol=areaSize)
  mapS<-matrix(F,nrow=areaSize,ncol=areaSize)
  mapW<-matrix(F,nrow=areaSize,ncol=areaSize)
  
  ## Start by going north
  mapN[currLoc[1],currLoc[2]]<-T
  dir<-"N"
  ## Step until leave the area or get stuck in a loop
  while(T){
    if(dir=="N"){
      if(currLoc[1]-1==0){
        break
      } else if(mydata[currLoc[1]-1,currLoc[2]]=="."){
        if(mapN[currLoc[1]-1,currLoc[2]]==T){
          loopCount<-loopCount+1
          break
        } else {
          mapN[currLoc[1]-1,currLoc[2]]<-T
        }
        currLoc[1]<-currLoc[1]-1
      } else if(mydata[currLoc[1]-1,currLoc[2]]=="#"){
        dir<-"E"
      }
    } else if(dir=="S"){
      if(currLoc[1]+1>areaSize){
        break
      } else if(mydata[currLoc[1]+1,currLoc[2]]=="."){
        if(mapS[currLoc[1]+1,currLoc[2]]==T){
          loopCount <- loopCount+1
          break
        } else {
          mapS[currLoc[1]+1,currLoc[2]]<-T
        }
        currLoc[1]<-currLoc[1]+1
      } else if(mydata[currLoc[1]+1,currLoc[2]]=="#"){
        dir<-"W"
      }
    } else if(dir=="W"){
      if(currLoc[2]-1==0){
        break
      } else if(mydata[currLoc[1],currLoc[2]-1]=="."){
        if(mapW[currLoc[1],currLoc[2]-1]==T){
          loopCount<-loopCount+1
          break
        } else {
          mapW[currLoc[1],currLoc[2]-1]<-T
        }
        currLoc[2]<-currLoc[2]-1
      } else if(mydata[currLoc[1],currLoc[2]-1]=="#"){
        dir<-"N"
      }
    } else if(dir=="E"){
      if(currLoc[2]+1>areaSize){
        break
      } else if(mydata[currLoc[1],currLoc[2]+1]=="."){
        if(mapE[currLoc[1],currLoc[2]+1]==T){
          loopCount<-loopCount+1
          break
        } else {
          mapE[currLoc[1],currLoc[2]+1]<-T
        }
        currLoc[2]<-currLoc[2]+1
      } else if(mydata[currLoc[1],currLoc[2]+1]=="#"){
        dir<-"S"
      }
    }
  }
}

## Read out the number of times they get stuck in a loop
loopCount


