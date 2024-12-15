# Advent of Code Day 11
# Part 1

## Stop R using scientific notation
options(scipen = 999)


fileName<-"Data/day11.txt"


mydata<-as.vector(unlist(read.table(fileName)))


blinks<-25
for(b in 1:blinks){
  i<-1
  while(i<=length(mydata)){
    if(mydata[i]==0){
      mydata[i]<-1
    } else if (nchar(mydata[i]) %% 2 ==0){
       new1<-as.numeric(substr(mydata[i],1,nchar(mydata[i])/2))
       new2<-as.numeric(substr(mydata[i],nchar(mydata[i])/2+1,nchar(mydata[i])))
       mydata[i]<-new1
       if(i<length(mydata)){
         mydata<-c(mydata[1:i],new2,mydata[(i+1):length(mydata)])
       } else {
        mydata<-c(mydata,new2)
       }
      i<-i+1
    } else {
      mydata[i]<-mydata[i]*2024
    }
    
    i<-i+1
  }
}
length(mydata)


# Part 2

mydata<-as.vector(unlist(read.table(fileName)))
value<-rep(1,length(mydata))

blinks<-75
for(b in 1:blinks){
  # reduce
  i<-1
  j<-2
  while(i<=length(mydata) && j<=length(mydata)){
    if(mydata[i]==mydata[j]){
      value[i]<-value[i]+value[j]
      value<-value[-j]
      mydata<-mydata[-j]
      j<-j-1
    }
    j<-j+1
    if(j>length(mydata)){
      i<-i+1
      j<-i+1
    }
  }
  
  # blink
  i<-1
  while(i<=length(mydata)){
    if(mydata[i]==0){
      mydata[i]<-1
    } else if (nchar(mydata[i]) %% 2 ==0){
      new1<-as.numeric(substr(mydata[i],1,nchar(mydata[i])/2))
      new2<-as.numeric(substr(mydata[i],nchar(mydata[i])/2+1,nchar(mydata[i])))
      mydata[i]<-new1
      if(i<length(mydata)){
        mydata<-c(mydata[1:i],new2,mydata[(i+1):length(mydata)])
        value<-c(value[1:i],value[i],value[(i+1):length(value)])
      } else {
        mydata<-c(mydata[1:i],new2)
        value<-c(value,value[i])
      }
      i<-i+1
    } else {
      mydata[i]<-mydata[i]*2024
    }
    
    i<-i+1
  }
}
sum(value)