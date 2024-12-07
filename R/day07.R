# Advent of Code Day 7
# Part 1

## Stop R using scientific notation
options(scipen = 999)

## Create a connection to the file
mydata<-file("Data/day07.txt","r")

## Define a function to check if the arrangement of operators and elements
## will result in the target
assess<-function(target,elements,operator){
  number<-elements[1]
  for(i in 1:length(operator)){
    if(operator[i]=="x"){
      number<-number*elements[i+1]
    } else if (operator[i]=="+"){
      number<-number+elements[i+1]
    }
  }
  if(number==target){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


## Initialise the result
result<-0

## Loop through each line of the file 
while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else {
    ## Split line
    splitline<-strsplit(line, ":")
    ## Extract the targets and elements
    target<-as.numeric(splitline[[1]][1])
    elements<-as.numeric(unlist(strsplit(splitline[[1]][2]," ")))[-1]
    
    ## Get the number of gaps
    gaps<-length(elements)-1
    ## Initialise with all multiplications
    operator<-rep("x",gaps)
    while(T){
      ## assess the current operators
      if(assess(target,elements,operator)==T){
        result<-result+target
        break
      }  
      ## iterate operators
      fail<-F
      i<-1
      while(operator[i]=="+"){
        i<-i+1
        if(i>gaps){
          fail<-T
          break
        }
      }
      if(fail==T){
        break
      } else {
        operator[i]<-"+"
        if(i>1){
          operator[1:(i-1)]<-"x"
        }
      }
      
    }
  }
}
## Close the connection
close(mydata)

## Read out the result
result



# Part 2

options(scipen = 999)

# Create a connection to the file
mydata<-file("Data/day07.txt","r")

assess2<-function(target,elements,operator){
  number<-elements[1]
  for(i in 1:length(operator)){
    if(operator[i]==1){
      number<-number*elements[i+1]
    } else if (operator[i]==2){
      number<-number+elements[i+1]
    } else if (operator[i]==3){
      number<-as.numeric(paste0(number,elements[i+1]))
    }
    if(sum((elements)==0)==0){
      if(number>target){
        break
      }
    }
  }
  if(number==target){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



result<-0
loopcount<-0
## Loop through each line of the file 
while (TRUE){
  line = readLines(mydata, n = 1)
  loopcount<-loopcount+1
  print.default(loopcount)
  if (length(line) == 0){
    break
  } else {
    ## Split line
    splitline<-strsplit(line, ":")
    target<-as.numeric(splitline[[1]][1])
    elements<-as.numeric(unlist(strsplit(splitline[[1]][2]," ")))[-1]
    
    gaps<-length(elements)-1
    operator<-rep(1,gaps)
    while(T){
      if(assess2(target,elements,operator)==T){
        result<-result+target
        break
      }  
      ## iterate operators
      fail<-F
      i<-1
      operator[i]<-operator[i]+1
      while(operator[i]>3){
        operator[i]<-1
        i<-i+1
        if(i>gaps){
          fail<-T
          break
        }
        operator[i]<-operator[i]+1
      }
      if(fail==T){
        break
      }
    }
  }
}

## Close the connection
close(mydata)

## Read out the result
result


