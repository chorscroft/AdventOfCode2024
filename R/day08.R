# Advent of Code Day 8
# Part 1

## Read in file
fileName<-"Data/day08.txt"
areaSize <- 50
mydata<-read.fwf(fileName,rep(1,areaSize),comment.char="")

## Initialise antenna data frame
antenna<-data.frame(freq=character(),x=integer(),y=integer())

## Find each antenna in the grid and record its frequency and location
for(i in 1:ncol(mydata)){
  for(j in 1:nrow(mydata)){
    if(mydata[i,j]!="."){
      antenna<-rbind(antenna,data.frame(freq=mydata[i,j],x=i,y=j))
    }
  }
}

## Initialise map of antinodes
map <-matrix(F,nrow = nrow(mydata),ncol=ncol(mydata)) 
## Loop over each pair of antenna
for(i in 1:(nrow(antenna)-1)){
  for(j in (i+1):nrow(antenna)){
    ## If the antenna have the same frequency then get the location of the antinodes
    if(antenna$freq[i]==antenna$freq[j]){
      antinodex<-(antenna$x[i]-antenna$x[j])+antenna$x[i]
      antinodey<-(antenna$y[i]-antenna$y[j])+antenna$y[i]
      ## If they are in the grid, record in the map
      if(antinodex>0 && antinodex<=nrow(mydata) && antinodey>0 && antinodey<=ncol(mydata)){
        map[antinodex,antinodey]<-T
      }
      antinodex<-(antenna$x[j]-antenna$x[i])+antenna$x[j]
      antinodey<-(antenna$y[j]-antenna$y[i])+antenna$y[j]
      if(antinodex>0 && antinodex<=nrow(mydata) && antinodey>0 && antinodey<=ncol(mydata)){
        map[antinodex,antinodey]<-T
      }
    }
  }
}

## Return the number of antinodes
sum(map)


# Part 2

## Initialise map of antinodes
map <-matrix(F,nrow = nrow(mydata),ncol=ncol(mydata)) 
## Loop over each pair of antenna
for(i in 1:(nrow(antenna)-1)){
  for(j in (i+1):nrow(antenna)){
    ## If the antenna have the same frequency then get the location of the antinodes
    if(antenna$freq[i]==antenna$freq[j]){
      ## record location of antenna that have at least two of the same frequency
      map[antenna$x[i],antenna$y[i]]<-T
      map[antenna$x[j],antenna$y[j]]<-T
      
      
      antinodex<-(antenna$x[i]-antenna$x[j])+antenna$x[i]
      antinodey<-(antenna$y[i]-antenna$y[j])+antenna$y[i]
      ## Keep stepping the same distance until reach the edge of the grid
      while(antinodex>0 && antinodex<=nrow(mydata) && antinodey>0 && antinodey<=ncol(mydata)){
        # If they are in the grid, record in the map
        map[antinodex,antinodey]<-T
        antinodex<-(antenna$x[i]-antenna$x[j])+antinodex
        antinodey<-(antenna$y[i]-antenna$y[j])+antinodey
      }
      
      antinodex<-(antenna$x[j]-antenna$x[i])+antenna$x[j]
      antinodey<-(antenna$y[j]-antenna$y[i])+antenna$y[j]
      while(antinodex>0 && antinodex<=nrow(mydata) && antinodey>0 && antinodey<=ncol(mydata)){
        map[antinodex,antinodey]<-T
        antinodex<-(antenna$x[j]-antenna$x[i])+antinodex
        antinodey<-(antenna$y[j]-antenna$y[i])+antinodey
      }
    }
  }
}

## Return the number of antinodes
sum(map)