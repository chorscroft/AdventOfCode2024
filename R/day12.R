# Advent of Code Day 12
# Part 1

fileName<-"Data/day12.txt"
areasize<-140
mydata<-read.fwf(fileName,rep(1,areasize),comment.char="")

areas<-matrix(0,areasize,areasize)
checkNext<-matrix(F,areasize,areasize)
perimeter<-matrix(0,areasize,areasize)

curr_area<-0
while(sum(areas==0)>0){
  for(i in 1:areasize){
    for (j in 1:areasize){
      if((areas[i,j]==0 && sum(checkNext)==0) || checkNext[i,j] == T){
        if(areas[i,j]==0 && sum(checkNext)==0){
          curr_area<-curr_area+1
        }
        areas[i,j]<-curr_area
        checkNext[i,j] <-F
        ## N
        if(i>1){
          if(areas[i-1,j]==0 && mydata[i,j]==mydata[i-1,j]){
            checkNext[i-1,j]<-T
          }
        }
        ## W
        if(j>1){
          if(areas[i,j-1]==0 && mydata[i,j]==mydata[i,j-1]){
            checkNext[i,j-1]<-T
          }
        }
        ## S
        if(i<areasize){
          if(areas[i+1,j]==0 && mydata[i,j]==mydata[i+1,j]){
            checkNext[i+1,j]<-T
          }
        }
        ## E
        if(j<areasize){
          if(areas[i,j+1]==0 && mydata[i,j]==mydata[i,j+1]){
            checkNext[i,j+1]<-T
          }
        }
        
      }
    }
  }
}

perimeter<-matrix(0,areasize,areasize)

for(a in 1:max(areas)){
  for(i in 1:areasize){
    for(j in 1:areasize){
      if(areas[i,j]==a){
        ##N
        if(i==1){
          perimeter[i,j]<-perimeter[i,j]+1
        } else if (areas[i-1,j]!=a){
          perimeter[i,j]<-perimeter[i,j]+1
        }
        ##W
        if(j==1){
          perimeter[i,j]<-perimeter[i,j]+1
        } else if (areas[i,j-1]!=a){
          perimeter[i,j]<-perimeter[i,j]+1
        }
        ##S
        if(i==areasize){
          perimeter[i,j]<-perimeter[i,j]+1
        } else if (areas[i+1,j]!=a){
          perimeter[i,j]<-perimeter[i,j]+1
        }
        ##E
        if(j==areasize){
          perimeter[i,j]<-perimeter[i,j]+1
        } else if (areas[i,j+1]!=a){
          perimeter[i,j]<-perimeter[i,j]+1
        }
      }
    }
  }
}

price<-0
for(a in 1:max(areas)){
  price<- price + sum(perimeter[areas==a])*sum(areas==a)
}
price

# Part 2

edges<-rep(0,max(areas))

## first row
currArea<-0
i<-1
for(j in 1:areasize){
  if(currArea != areas[i,j]){
    currArea<-areas[i,j]
    edges[currArea]<-edges[currArea]+1
  }
}
## last row
currArea<-0
i<-areasize
for(j in 1:areasize){
  if(currArea != areas[i,j]){
    currArea<-areas[i,j]
    edges[currArea]<-edges[currArea]+1
  }
}
## first col
currArea<-0
j<-1
for(i in 1:areasize){
  if(currArea != areas[i,j]){
    currArea<-areas[i,j]
    edges[currArea]<-edges[currArea]+1
  }
}
## last col
currArea<-0
j<-areasize
for(i in 1:areasize){
  if(currArea != areas[i,j]){
    currArea<-areas[i,j]
    edges[currArea]<-edges[currArea]+1
  }
}




currArea<-0
for(i in 1:(areasize-1)){
  for(j in 1:(areasize)){
    ## below
    if(currArea!=areas[i,j] && areas[i+1,j] != areas[i,j]){
      currArea<-areas[i,j]
      edges[currArea]<-edges[currArea]+1
    } else if (currArea!=areas[i,j] || (currArea==areas[i,j] && areas[i+1,j] == areas[i,j])){
      currArea<-0
    }
  }
}
currArea<-0
for(i in 2:(areasize)){
  for(j in 1:(areasize)){
    ## above
    if(currArea!=areas[i,j] && areas[i-1,j] != areas[i,j]){
      currArea<-areas[i,j]
      edges[currArea]<-edges[currArea]+1
    } else if (currArea!=areas[i,j] || (currArea==areas[i,j] && areas[i-1,j] == areas[i,j])){
      currArea<-0
    }
  }
}
currArea<-0
for(j in 1:(areasize-1)){
  for(i in 1:(areasize)){
    ## right
    if(currArea!=areas[i,j] && areas[i,j+1] != areas[i,j]){
      currArea<-areas[i,j]
      edges[currArea]<-edges[currArea]+1
    } else if (currArea!=areas[i,j] || (currArea==areas[i,j] && areas[i,j+1] == areas[i,j])){
      currArea<-0
    }
  }
}
currArea<-0
for(j in 2:(areasize)){
  for(i in 1:(areasize)){
    ## left
    if(currArea!=areas[i,j] && areas[i,j-1] != areas[i,j]){
      currArea<-areas[i,j]
      edges[currArea]<-edges[currArea]+1
    } else if (currArea!=areas[i,j] || (currArea==areas[i,j] && areas[i,j-1] == areas[i,j])){
      currArea<-0
    }
  }
}
                
                
price<-0
for(a in 1:max(areas)){
  price<- price + edges[a]*sum(areas==a)
}
price
