# Advent of Code Day 10
# Part 1

## Read in the file
fileName<-"Data/day10.txt"
no_characters_in_line<-45
mydata<-read.fwf(fileName,rep(1,no_characters_in_line))

trails<-function(i,j,currNum){
  if(currNum==9){
    if(length(visitedi)==0){
      count<<-count+1
      visitedi<<-i
      visitedj<<-j
    }
    fail<-F
    for(x in 1:length(visitedi)){
      if(i==visitedi[x] && j==visitedj[x]){
        fail<-T
        break
      }
    }
    if(fail==F){
      count<<-count+1
      visitedi<<-c(visitedi,i)
      visitedj<<-c(visitedj,j)
    }
    return(T)
  }
  ##go up
  if(i>1 && mydata[i-1,j]==currNum+1){
    temp<-trails(i-1,j,currNum+1)
  }
  ##go right
  if(j<nrow(mydata) && mydata[i,j+1]==currNum+1){
    temp<-trails(i,j+1,currNum+1)
  }
  ##go down
  if(i<nrow(mydata) && mydata[i+1,j]==currNum+1){
    temp<-trails(i+1,j,currNum+1)
  }
  ##go left
  if(j>1 && mydata[i,j-1]==currNum+1){
    temp<-trails(i,j-1,currNum+1)
  }
  return(F)
}

result<-0
for(i in 1:nrow(mydata)){
  for(j in 1:ncol(mydata)){
    if(mydata[i,j]==0){
      count<-0
      visitedi<-NULL
      visitedj<-NULL
      temp<-trails(i,j,0) 
      result<-result+count
    }
  }
}
result


## Part 2

trails<-function(i,j,currNum){
  if(currNum==9){
    # if(length(visitedi)==0){
    #   count<<-count+1
    #   #visitedi<<-i
    #   #visitedj<<-j
    # }
    # fail<-F
    # for(x in 1:length(visitedi)){
    #   if(i==visitedi[x] && j==visitedj[x]){
    #     fail<-T
    #     break
    #   }
    # }
    # if(fail==F){
      count<<-count+1
    #   visitedi<<-c(visitedi,i)
    #   visitedj<<-c(visitedj,j)
    # }
    return(T)
  }
  ##go up
  if(i>1 && mydata[i-1,j]==currNum+1){
    temp<-trails(i-1,j,currNum+1)
  }
  ##go right
  if(j<nrow(mydata) && mydata[i,j+1]==currNum+1){
    temp<-trails(i,j+1,currNum+1)
  }
  ##go down
  if(i<nrow(mydata) && mydata[i+1,j]==currNum+1){
    temp<-trails(i+1,j,currNum+1)
  }
  ##go left
  if(j>1 && mydata[i,j-1]==currNum+1){
    temp<-trails(i,j-1,currNum+1)
  }
  return(F)
}

result<-0
for(i in 1:nrow(mydata)){
  for(j in 1:ncol(mydata)){
    if(mydata[i,j]==0){
      count<-0
      # visitedi<-NULL
      # visitedj<-NULL
      temp<-trails(i,j,0) 
      result<-result+count
    }
  }
}
result
