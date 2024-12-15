# Advent of Code Day 13
# Part 1

mydata<-file("Data/day13.txt","r")

tokens<-0

while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
    ## solve simultaneous equation
    multi1<-bb[2]
    multi2<-bb[1]
    ba[1]<-ba[1]*multi1
    bb[1]<-bb[1]*multi1
    prize[1]<-prize[1]*multi1
    ba[2]<-ba[2]*multi2
    bb[2]<-bb[2]*multi2
    prize[2]<-prize[2]*multi2
    
    pressa<-(prize[2]-prize[1])/(ba[2]-ba[1])
    pressb<-(prize[1]-pressa*ba[1])/bb[1]
    
    ##if(all.equal(pressa, as.integer(pressa))==T && all.equal(pressb, as.integer(pressb))==T){
    if(pressa%%1==0 && pressb%%1==0){ 
         tokens<-tokens+3*pressa+pressb
    }
    
    
  } else {
    if(substr(line,1,8)=='Button A'){
      ba <- unlist(strsplit(line," "))[3:4]
      ba[1]<-substr(ba[1],3,nchar(ba[1])-1)
      ba[2]<-substr(ba[2],3,nchar(ba[2]))
      ba<-as.numeric(ba)
    } else if(substr(line,1,8)=='Button B'){
      bb <- unlist(strsplit(line," "))[3:4]
      bb[1]<-substr(bb[1],3,nchar(bb[1])-1)
      bb[2]<-substr(bb[2],3,nchar(bb[2]))
      bb<-as.numeric(bb)
    } else if(substr(line,1,5)=='Prize'){
      prize <- unlist(strsplit(line," "))[2:3]
      prize[1]<-substr(prize[1],3,nchar(prize[1])-1)
      prize[2]<-substr(prize[2],3,nchar(prize[2]))
      prize<-as.numeric(prize)
    }
  }
}
close(mydata)

tokens




mydata<-file("//gstt.local/Users/15/CHORSCROFT/Documents/AdventOfCode/AdventOfCode2024-main/Data/day13.txt","r")

tokens<-0

while (TRUE){
  line = readLines(mydata, n = 1)
  if (line=="" || length(line) == 0){
    ## solve simultaneous equation
    multi1<-bb[2]
    multi2<-bb[1]
    ba[1]<-ba[1]*multi1
    bb[1]<-bb[1]*multi1
    prize[1]<-prize[1]*multi1
    ba[2]<-ba[2]*multi2
    bb[2]<-bb[2]*multi2
    prize[2]<-prize[2]*multi2
    
    pressa<-(prize[2]-prize[1])/(ba[2]-ba[1])
    pressb<-(prize[1]-pressa*ba[1])/bb[1]
    
    ##if(all.equal(pressa, as.integer(pressa))==T && all.equal(pressb, as.integer(pressb))==T){
    if(pressa%%1==0 && pressb%%1==0){ 
      tokens<-tokens+3*pressa+pressb
    }
    print.default(tokens)
    if (length(line) == 0){
      break
    }
    
  } else {
    if(substr(line,1,8)=='Button A'){
      ba <- unlist(strsplit(line," "))[3:4]
      ba[1]<-substr(ba[1],3,nchar(ba[1])-1)
      ba[2]<-substr(ba[2],3,nchar(ba[2]))
      ba<-as.numeric(ba)
    } else if(substr(line,1,8)=='Button B'){
      bb <- unlist(strsplit(line," "))[3:4]
      bb[1]<-substr(bb[1],3,nchar(bb[1])-1)
      bb[2]<-substr(bb[2],3,nchar(bb[2]))
      bb<-as.numeric(bb)
    } else if(substr(line,1,5)=='Prize'){
      prize <- unlist(strsplit(line," "))[2:3]
      prize[1]<-substr(prize[1],3,nchar(prize[1])-1)
      prize[2]<-substr(prize[2],3,nchar(prize[2]))
      prize<-as.numeric(prize)+10000000000000
    }
  }
}
close(mydata)

tokens

## 99048934849633 too low
