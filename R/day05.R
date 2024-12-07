# Advent of Code Day 5
# Part 1

## Initialise a matrix to store the instructions in
instructions<-matrix(NA,nrow=0,ncol=2)
## Start by reading the instructions in
instr<-T
## Initialise the result
result<-0
## Create connection to file
mydata<-file("Data/day05.txt","r")

while (TRUE){
  ## Read line of data file
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
    ## If line is blank we have reached the end of the instructions
    instr<-F
  } else {
    if (instr==T){
      ## Add to the matrix of instructions
      instructions<-rbind(instructions,as.numeric(unlist(strsplit(line,"|",T))))
    } else {
      ## Get the line for printing
      printing<-as.numeric(unlist(strsplit(line,",",T)))
      ## Check all the instructions and if any fail then stop
      fail<-F
      for(p in 2:(length(printing))){
        for(i in 1:nrow(instructions)){
          if(printing[p]==instructions[i,1]){
            for(j in 1:(p-1)){
              if(printing[j]==instructions[i,2]){
                fail<-T
                break
              }
            }
          }
          if(fail==T){
            break
          }
        }
        if(fail==T){
          break
        }
      }
      if(fail==F){
        ## if the line didn't fail then add the middle number
        result<-result+printing[(length(printing)+1)/2]
      }
    }
  }
}
## Close the connection to the data
close(mydata)

## Read out the result
result


# Part 2

## Initialise a matrix to store the instructions in
instructions<-matrix(NA,nrow=0,ncol=2)
## Start by reading the instructions in
instr<-T
## Initialise the result
result<-0
## Create connection to file
mydata<-file("Data/day05.txt","r")

while (TRUE){
  ## Read line of data file
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else if (line==""){
    ## If line is blank we have reached the end of the instructions
    instr<-F
  } else {
    if (instr==T){
      ## Add to the matrix of instructions
      instructions<-rbind(instructions,as.numeric(unlist(strsplit(line,"|",T))))
    } else {
      ## Get the line for printing
      printing<-as.numeric(unlist(strsplit(line,",",T)))
      fail<-F
      ## Check to see if the line fails any instructions
      for(p in 2:(length(printing))){
        for(i in 1:nrow(instructions)){
          if(printing[p]==instructions[i,1]){
            for(j in 1:(p-1)){
              if(printing[j]==instructions[i,2]){
                fail<-T
                break
              }
            }
          }
          if(fail==T){
            break
          }
        }
        if(fail==T){
          break
        }
      }
      if(fail==T){
        ## For each failed instruction, flip the locations of the numbers
        ## Keep repeating until no instructions are failed
        change<-T
        while(change==T){
          change<-F
          for(p in 2:(length(printing))){
            for(i in 1:nrow(instructions)){
              if(printing[p]==instructions[i,1]){
                for(j in 1:(p-1)){
                  if(printing[j]==instructions[i,2]){
                    change<-T
                    temp<-printing[p]
                    printing[p]<-printing[j]
                    printing[j]<-temp
                  }
                }
              }
            }
          }
        }
        ## once the line doesn't fail then add the middle number
        result<-result+printing[(length(printing)+1)/2]
      }
    }
  }
}
## Close the connection to the data
close(mydata)

## Read out the result
result