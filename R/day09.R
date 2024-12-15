# Advent of Code Day 9
# Part 1

## Stop R using scientific notation
options(scipen = 999)

## Read in the file
fileName<-"Data/day09.txt"
no_characters_in_line<-19999
mydata<-read.fwf(fileName,rep(1,no_characters_in_line))

## Initialise data frames for the files (ids) and free space (gaps)
ids<-data.frame(IDs=integer(),length=integer())
gaps<-data.frame(IDs=integer(),length=integer())

## read through string and extract ids and gaps
id<-0
i<-1
while(T){
  ## read in the files
  ids<-rbind(ids,data.frame(IDs=id,length=mydata[1,i]))
  i<-i+1
  if(i>ncol(mydata)){
    break
  }
  ## read in the gaps
  gaps<-rbind(gaps,data.frame(IDs=id,length=mydata[1,i]))
  i<-i+1
  id<-id+1
}

## Initialise the result
result<-0

## start at position 0
pos<-0
while(T){
  ## Read in the next file and update the results
  ## ID
  for(pos in pos:(pos+ids$length[1]-1)){
    result<-result+pos*ids$IDs[1]
  }
  pos<-pos+1
  ## Remove the file once it has been processed.
  ids<-ids[-1,]
  ## stop running if all files have been processed
  if(nrow(ids)==0){
    break
  }
  ## GAP
  if(gaps$length[1]>0){
    ## pull id from the end file into the gap
    for(pos in pos:(pos+gaps$length[1]-1)){
      result<-result+pos*ids$IDs[nrow(ids)]
      ids$length[nrow(ids)]<-ids$length[nrow(ids)]-1
      ## remove file if it has been fully processed
      if(ids$length[nrow(ids)]==0){
        ids<-ids[-nrow(ids),]
      }
    }
    pos<-pos+1
  }
  ## remove gap once it has been processed
  gaps<-gaps[-1,]
}

## Read out result
result


# Part 2

## Stop R using scientific notation
options(scipen = 999)

## Read in the file
fileName<-"//gstt.local/Users/15/CHORSCROFT/Documents/AdventOfCode/AdventOfCode2024-main/Data/day09.txt"
no_characters_in_line<-19999
mydata<-read.fwf(fileName,rep(1,no_characters_in_line))

## Initialise data frames for the files (ids) and free space (gaps)
ids<-data.frame(IDs=integer(),length=integer())
gaps<-data.frame(IDs=integer(),length=integer())

## read through string and extract ids and gaps
id<-0
i<-1
while(T){
  ## read in the files
  ids<-rbind(ids,data.frame(IDs=id,length=mydata[1,i]))
  i<-i+1
  if(i>ncol(mydata)){
    break
  }
  ## read in the gaps
  gaps<-rbind(gaps,data.frame(IDs=id,length=mydata[1,i]))
  i<-i+1
  id<-id+1
}

## Add in the current start location for each file in the ids data frame
loc<-1
for(i in 1:nrow(ids)){
  ids$start[i]<-loc
  loc<-loc+ids$length[i]+gaps$length[i]
}

## Get the maximum id 
max_id<-nrow(ids)-1

## Loop over each ID except ID=0
for(i_id in max_id:1){
  ## Find location in data frame
  i<-which(ids$IDs==i_id)
  for(j in 1:(i-1)){
    ## Find if there is a gap it can fit into
    if(ids$start[j]+ids$length[j]+ids$length[i]-1<ids$start[j+1]){
      ## move into gap
      ids$start[i]<-ids$start[j]+ids$length[j]
      ## rearrange ids data frame
      temp<-ids[i,]
      ids<-ids[-i,]
      ids<-rbind(ids[1:j,],temp,ids[(j+1):nrow(ids),])
      i<-i+1
      break
    }
   
  }
}

## Initialise result
result<-0
## iterate result for each position
for(i in 1:nrow(ids)){
  for(pos in ids$start[i]:(ids$start[i]+ids$length[i]-1)){
    result<-result+(pos-1)*ids$IDs[i]
  }
}

## Read out result
result