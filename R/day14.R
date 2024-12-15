# Advent of Code Day 14
# Part 1

## Read in the file
data<-read.table("Data/day14.txt",col.names=c("p","v"))
size<-c(101,103) #c(11,7)


p<-matrix(NA,nrow(data),2)
v<-matrix(NA,nrow(data),2)

for(i in 1:nrow(data)){
  p[i,]<-as.numeric(unlist(strsplit(substr(data[i,1],3,nchar(data[i,1])),",")))+1
  v[i,]<-as.numeric(unlist(strsplit(substr(data[i,2],3,nchar(data[i,2])),",")))
}


newp<-p
for(i in 1:100){
  newp<-newp+v
  for(r in 1:nrow(newp)){
    if(newp[r,1]<1){
      newp[r,1]<-newp[r,1]+size[1]

    }
    if(newp[r,1]>size[1]){
      newp[r,1]<-newp[r,1]-size[1]
    }
    if(newp[r,2]<1){
      newp[r,2]<-newp[r,2]+size[2]
      
    }
    if(newp[r,2]>size[2]){
      newp[r,2]<-newp[r,2]-size[2]
    }

  }
}


quad1<-sum(newp[,1]<(size[1]+1)/2 & newp[,2]<(size[2]+1)/2)
quad2<-sum(newp[,1]<(size[1]+1)/2 & newp[,2]>(size[2]+1)/2)
quad3<-sum(newp[,1]>(size[1]+1)/2 & newp[,2]<(size[2]+1)/2)
quad4<-sum(newp[,1]>(size[1]+1)/2 & newp[,2]>(size[2]+1)/2)

quad1*quad2*quad3*quad4


## part 2

data<-read.table("Data/day14.txt",col.names=c("p","v"))
size<-c(101,103) #c(11,7)


p<-matrix(NA,nrow(data),2)
v<-matrix(NA,nrow(data),2)

for(i in 1:nrow(data)){
  p[i,]<-as.numeric(unlist(strsplit(substr(data[i,1],3,nchar(data[i,1])),",")))+1
  v[i,]<-as.numeric(unlist(strsplit(substr(data[i,2],3,nchar(data[i,2])),",")))
}


newp<-p
seconds<-0
while(seconds<=10000){
  seconds<-seconds+1
  newp<-newp+v
  for(r in 1:nrow(newp)){
    if(newp[r,1]<1){
      newp[r,1]<-newp[r,1]+size[1]
      
    }
    if(newp[r,1]>size[1]){
      newp[r,1]<-newp[r,1]-size[1]
    }
    if(newp[r,2]<1){
      newp[r,2]<-newp[r,2]+size[2]
      
    }
    if(newp[r,2]>size[2]){
      newp[r,2]<-newp[r,2]-size[2]
    }
  }
  quad1<-sum(newp[,2]<(size[2]+1)/4)
  quad2<-sum(newp[,2]>=(size[2]+1)/4 & newp[,2]<(size[2]+1)/2)
  quad3<-sum(newp[,2]>=(size[2]+1)/2 & newp[,2]<3*(size[2]+1)/4)
  quad4<-sum(newp[,2]>=3*(size[2]+1)/4 & newp[,2]<(size[2]+1))
  
  if(seconds >= 0 && quad1<quad2 && quad2<quad3 && quad1*1.5<quad3){
    plot(newp[,1],newp[,2],main = seconds)
  }
  
}
