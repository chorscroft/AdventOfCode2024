# Advent of Code Day 15
# Part 1

## Read in file
fileName<-"Data/day15.txt"
areaSize <- 50
map<-read.fwf(fileName,rep(1,areaSize),comment.char="",n = areaSize)

startread<-F
instr<-NULL
mydata<-file(fileName,"r")
while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
    startread<-T
  } else {
    if(startread==T){
      instr<-paste0(instr,line)
    }
  }
}
close(mydata)
instr<-unlist(strsplit(instr,""))


for(i in 1:nrow(map)){
  for(j in 1:ncol(map)){
    if(map[i,j]=="@"){
      currLoc<-c(i,j)
      break
    }
  }
}
map[currLoc[1],currLoc[2]]<-"."

for(x in 1:length(instr)){
  ##Up
  if(instr[x]=="^"){
    if(map[currLoc[1]-1,currLoc[2]]=="."){
      currLoc[1]<-currLoc[1]-1
    } else if(map[currLoc[1]-1,currLoc[2]]=="O"){
      i<-1
      while(map[currLoc[1]-i,currLoc[2]]=="O"){
        i<-i+1
      }
      if(map[currLoc[1]-i,currLoc[2]]=="."){
        map[currLoc[1]-i,currLoc[2]]<-"O"
        map[currLoc[1]-1,currLoc[2]]<-"."
        currLoc[1]<-currLoc[1]-1
      }
    }
  }
  ##Down
  if(instr[x]=="v"){
    if(map[currLoc[1]+1,currLoc[2]]=="."){
      currLoc[1]<-currLoc[1]+1
    } else if(map[currLoc[1]+1,currLoc[2]]=="O"){
      i<-1
      while(map[currLoc[1]+i,currLoc[2]]=="O"){
        i<-i+1
      }
      if(map[currLoc[1]+i,currLoc[2]]=="."){
        map[currLoc[1]+i,currLoc[2]]<-"O"
        map[currLoc[1]+1,currLoc[2]]<-"."
        currLoc[1]<-currLoc[1]+1
      }
    }
  }
  ##Left
  if(instr[x]=="<"){
    if(map[currLoc[1],currLoc[2]-1]=="."){
      currLoc[2]<-currLoc[2]-1
    } else if(map[currLoc[1],currLoc[2]-1]=="O"){
      i<-1
      while(map[currLoc[1],currLoc[2]-i]=="O"){
        i<-i+1
      }
      if(map[currLoc[1],currLoc[2]-i]=="."){
        map[currLoc[1],currLoc[2]-i]<-"O"
        map[currLoc[1],currLoc[2]-1]<-"."
        currLoc[2]<-currLoc[2]-1
      }
    }
  }
  ##Right
  if(instr[x]==">"){
    if(map[currLoc[1],currLoc[2]+1]=="."){
      currLoc[2]<-currLoc[2]+1
    } else if(map[currLoc[1],currLoc[2]+1]=="O"){
      i<-1
      while(map[currLoc[1],currLoc[2]+i]=="O"){
        i<-i+1
      }
      if(map[currLoc[1],currLoc[2]+i]=="."){
        map[currLoc[1],currLoc[2]+i]<-"O"
        map[currLoc[1],currLoc[2]+1]<-"."
        currLoc[2]<-currLoc[2]+1
      }
    }
  }
}




result<-0
for(i in 1:nrow(map)){
  for(j in 1:ncol(map)){
    if(map[i,j]=="O"){
      result<-result+100*(i-1)+(j-1)
    }
    
    
  }
}
result


## Part 2

## Read in file
fileName<-"Data/day15.txt"
areaSize <- 50
map<-read.fwf(fileName,rep(1,areaSize),comment.char="",n = areaSize)

startread<-F
instr<-NULL
mydata<-file(fileName,"r")
while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
    startread<-T
  } else {
    if(startread==T){
      instr<-paste0(instr,line)
    }
  }
}
close(mydata)
instr<-unlist(strsplit(instr,""))

oldmap<-map
map<-matrix(".",nrow=nrow(oldmap),ncol=ncol(oldmap)*2)
for(i in 1:nrow(oldmap)){
  for(j in 1:ncol(oldmap)){
    if(oldmap[i,j]=="#"){
      map[i,j*2-1]<-"#"
      map[i,j*2]<-"#"
    } else if(oldmap[i,j]=="."){
      map[i,j*2-1]<-"."
      map[i,j*2]<-"."
    } else if(oldmap[i,j]=="@"){
      map[i,j*2-1]<-"."
      map[i,j*2]<-"."
      currLoc<-c(i,j*2-1)
    } else if(oldmap[i,j]=="O"){
      map[i,j*2-1]<-"["
      map[i,j*2]<-"]"
    }
  }
}

canthisboxmove<-function(coord,dir){
  ## coord is left half of box i.e. the "["
  if(dir=="^"){
    ##up
    if(map[coord[1]-1,coord[2]]=="." && map[coord[1]-1,coord[2]+1]=="."){
      return(T)
    } else if (map[coord[1]-1,coord[2]]=="#" ||  map[coord[1]-1,coord[2]+1]=="#"){
      return(F)
    } else if(map[coord[1]-1,coord[2]]=="["){
      return(canthisboxmove(c(coord[1]-1,coord[2]),"^"))
    } else if(map[coord[1]-1,coord[2]]=="]" && map[coord[1]-1,coord[2]+1]=="["){
      firstbox<-canthisboxmove(c(coord[1]-1,coord[2]-1),"^")
      secondbox<-canthisboxmove(c(coord[1]-1,coord[2]+1),"^")
      if(firstbox==T && secondbox==T){
        return(T)
      } else {
        return(F)
      }
    } else if(map[coord[1]-1,coord[2]]=="]"){
      return(canthisboxmove(c(coord[1]-1,coord[2]-1),"^"))
    } else if(map[coord[1]-1,coord[2]+1]=="["){
      return(canthisboxmove(c(coord[1]-1,coord[2]+1),"^"))
    }
  } else if(dir=="v"){
    ##down
    if(map[coord[1]+1,coord[2]]=="." && map[coord[1]+1,coord[2]+1]=="."){
      return(T)
    } else if (map[coord[1]+1,coord[2]]=="#" ||  map[coord[1]+1,coord[2]+1]=="#"){
      return(F)
    } else if(map[coord[1]+1,coord[2]]=="["){
      return(canthisboxmove(c(coord[1]+1,coord[2]),"v"))
    } else if(map[coord[1]+1,coord[2]]=="]" && map[coord[1]+1,coord[2]+1]=="["){
      firstbox<-canthisboxmove(c(coord[1]+1,coord[2]-1),"v")
      secondbox<-canthisboxmove(c(coord[1]+1,coord[2]+1),"v")
      if(firstbox==T && secondbox==T){
        return(T)
      } else {
        return(F)
      }
    } else if(map[coord[1]+1,coord[2]]=="]"){
      return(canthisboxmove(c(coord[1]+1,coord[2]-1),"v"))
    } else if(map[coord[1]+1,coord[2]+1]=="["){
      return(canthisboxmove(c(coord[1]+1,coord[2]+1),"v"))
    }
  }
}
boxestomove<-function(coord,dir,boxes){
  ## coord is left half of box i.e. the "["
  if(dir=="^"){
    ##up
    if(map[coord[1]-1,coord[2]]=="." && map[coord[1]-1,coord[2]+1]=="."){
      return(boxes)
    } else if (map[coord[1]-1,coord[2]]=="#" ||  map[coord[1]-1,coord[2]+1]=="#"){
      return(boxes)
    } else if(map[coord[1]-1,coord[2]]=="["){
      boxes<-rbind(boxes,c(coord[1]-1,coord[2]))
      return(boxestomove(c(coord[1]-1,coord[2]),"^",boxes))
    } else if(map[coord[1]-1,coord[2]]=="]" && map[coord[1]-1,coord[2]+1]=="["){
      boxes<-rbind(boxes,c(coord[1]-1,coord[2]-1))
      firstbox<-boxestomove(c(coord[1]-1,coord[2]-1),"^",boxes)
      boxes<-rbind(firstbox,c(coord[1]-1,coord[2]+1))
      secondbox<-boxestomove(c(coord[1]-1,coord[2]+1),"^",boxes)
      boxes<-secondbox
    } else if(map[coord[1]-1,coord[2]]=="]"){
      boxes<-rbind(boxes,c(coord[1]-1,coord[2]-1))
      return(boxestomove(c(coord[1]-1,coord[2]-1),"^",boxes))
    } else if(map[coord[1]-1,coord[2]+1]=="["){
      boxes<-rbind(boxes,c(coord[1]-1,coord[2]+1))
      return(boxestomove(c(coord[1]-1,coord[2]+1),"^",boxes))
    }
  } else if(dir=="v"){
    ##down
    if(map[coord[1]+1,coord[2]]=="." && map[coord[1]+1,coord[2]+1]=="."){
      return(boxes)
    } else if (map[coord[1]+1,coord[2]]=="#" ||  map[coord[1]+1,coord[2]+1]=="#"){
      return(boxes)
    } else if(map[coord[1]+1,coord[2]]=="["){
      return(boxestomove(c(coord[1]+1,coord[2]),"v",rbind(boxes,c(coord[1]+1,coord[2]))))
    } else if(map[coord[1]+1,coord[2]]=="]" && map[coord[1]+1,coord[2]+1]=="["){
      boxes<-rbind(boxes,c(coord[1]+1,coord[2]-1))
      firstbox<-boxestomove(c(coord[1]+1,coord[2]-1),"v",boxes)
      boxes<-rbind(firstbox,c(coord[1]+1,coord[2]+1))
      secondbox<-boxestomove(c(coord[1]+1,coord[2]+1),"v",boxes)
      boxes<-secondbox
    } else if(map[coord[1]+1,coord[2]]=="]"){
      boxes<-rbind(boxes,c(coord[1]+1,coord[2]-1))
      return(boxestomove(c(coord[1]+1,coord[2]-1),"v",boxes))
    } else if(map[coord[1]+1,coord[2]+1]=="["){
      boxes<-rbind(boxes,c(coord[1]+1,coord[2]+1))
      return(boxestomove(c(coord[1]+1,coord[2]+1),"v",boxes))
    }
  }
  return(boxes)
}


for(x in 1:length(instr)){
  ##Up
  if(instr[x]=="^"){
    if(map[currLoc[1]-1,currLoc[2]]=="."){
      currLoc[1]<-currLoc[1]-1
    } else if(map[currLoc[1]-1,currLoc[2]]=="[" || map[currLoc[1]-1,currLoc[2]]=="]"){
      if(map[currLoc[1]-1,currLoc[2]]=="["){
        coord<-c(currLoc[1]-1,currLoc[2])
      } else {
        coord<-c(currLoc[1]-1,currLoc[2]-1)
      }
      if(canthisboxmove(coord,"^")==T){
        ##move the boxes
        movingboxes<-matrix(boxestomove(coord,"^",coord),ncol=2)
        for(b in 1:nrow(movingboxes)){
          map[movingboxes[b,1],movingboxes[b,2]]<-"."
          map[movingboxes[b,1],movingboxes[b,2]+1]<-"."
        }
        for(b in 1:nrow(movingboxes)){
          map[movingboxes[b,1]-1,movingboxes[b,2]]<-"["
          map[movingboxes[b,1]-1,movingboxes[b,2]+1]<-"]"
        }
        ##move yourself
        currLoc[1]<-currLoc[1]-1
      }
    }
  }
  ##Down
  if(instr[x]=="v"){
    if(map[currLoc[1]+1,currLoc[2]]=="."){
      currLoc[1]<-currLoc[1]+1
    } else if(map[currLoc[1]+1,currLoc[2]]=="[" || map[currLoc[1]+1,currLoc[2]]=="]"){
      if(map[currLoc[1]+1,currLoc[2]]=="["){
        coord<-c(currLoc[1]+1,currLoc[2])
      } else {
        coord<-c(currLoc[1]+1,currLoc[2]-1)
      }
      if(canthisboxmove(coord,"v")==T){
        ##move the boxes
        movingboxes<-matrix(boxestomove(coord,"v",coord),ncol=2)
        for(b in 1:nrow(movingboxes)){
          map[movingboxes[b,1],movingboxes[b,2]]<-"."
          map[movingboxes[b,1],movingboxes[b,2]+1]<-"."
        }
        for(b in 1:nrow(movingboxes)){
          map[movingboxes[b,1]+1,movingboxes[b,2]]<-"["
          map[movingboxes[b,1]+1,movingboxes[b,2]+1]<-"]"
        }
        ##move yourself
        currLoc[1]<-currLoc[1]+1
      }
    }
  }
  ##Left
  if(instr[x]=="<"){
    if(map[currLoc[1],currLoc[2]-1]=="."){
      currLoc[2]<-currLoc[2]-1
    } else if(map[currLoc[1],currLoc[2]-1]=="]"){
      i<-1
      while(map[currLoc[1],currLoc[2]-i]=="]" || map[currLoc[1],currLoc[2]-i]=="["){
        i<-i+1
      }
      if(map[currLoc[1],currLoc[2]-i]=="."){
        map[currLoc[1],currLoc[2]-i]<-"["
        for(j in (i-1):1){
          if(map[currLoc[1],currLoc[2]-j]=="["){
            map[currLoc[1],currLoc[2]-j]<-"]"
          } else {
            map[currLoc[1],currLoc[2]-j]<-"["
          }
        }
        map[currLoc[1],currLoc[2]-1]<-"."
        currLoc[2]<-currLoc[2]-1
      }
    }
  }
  ##Right
  if(instr[x]==">"){
    if(map[currLoc[1],currLoc[2]+1]=="."){
      currLoc[2]<-currLoc[2]+1
    } else if(map[currLoc[1],currLoc[2]+1]=="["){
      i<-1
      while(map[currLoc[1],currLoc[2]+i]=="[" || map[currLoc[1],currLoc[2]+i]=="]"){
        i<-i+1
      }
      if(map[currLoc[1],currLoc[2]+i]=="."){
        map[currLoc[1],currLoc[2]+i]<-"]"
        for(j in (i-1):1){
          if(map[currLoc[1],currLoc[2]+j]=="["){
            map[currLoc[1],currLoc[2]+j]<-"]"
          } else {
            map[currLoc[1],currLoc[2]+j]<-"["
          }
        }
        map[currLoc[1],currLoc[2]+1]<-"."
        currLoc[2]<-currLoc[2]+1
      }
    }
  }
}


result<-0
for(i in 1:nrow(map)){
  for(j in 1:ncol(map)){
    if(map[i,j]=="["){
      result<-result+100*(i-1)+(j-1)
    }
    
    
  }
}
result
