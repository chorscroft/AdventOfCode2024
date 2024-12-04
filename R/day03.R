# Advent of Code Day 3
# Part 1

## Create a connection to the file
mydata<-file("Data/day03.txt","r")

## Initialise the string
myStr<-""

## Loop through each line of the file 
while (TRUE){
  line = readLines(mydata, n = 1)
  if (length(line) == 0){
    break
  } else {
    ## Concatenate each line of the file together
    myStr<-paste0(myStr,line)
  }
}
## Close the connection
close(mydata)

## Initialise the result
result<-0
## Start at character 1
i<-1
## loop until there is no more possible multiplications
while(i<nchar(myStr)-4){
  ## find the start of a potential multiplication
  if(substr(myStr,i,i+3)=="mul("){
    ## initilaise the numbers
    firstnumber<-""
    secondnumber<-""
    
    ## Assume it will pass the checks
    fail <-F
    firstDone <- F #track if the first number is complete
    secondDone <- F #track if the second number is complete
    # skip to start of potential first number
    i<-i+4
    # Get the next character
    testdigit<-substr(myStr,i,i)
    # Record if it is a number
    if(is.na(as.numeric(testdigit))==FALSE){
      firstnumber<-paste0(firstnumber,testdigit)
    } else {
      # fail if it is not a number
      fail <- T
    }
    if(fail==F){
      i<-i+1
      testdigit<-substr(myStr,i,i)
      # if a comma appears then it is the end of the first number
      if(testdigit==","){
        firstDone<-T
      } else if(is.na(as.numeric(testdigit))==FALSE){
        firstnumber<-paste0(firstnumber,testdigit)
      } else {
        fail <- T
      }
    }
    if(fail==F & firstDone==F){
      i<-i+1
      testdigit<-substr(myStr,i,i)
      if(testdigit==","){
        firstDone<-T
      } else if(is.na(as.numeric(testdigit))==FALSE){
        firstnumber<-paste0(firstnumber,testdigit)
      } else {
        fail <- T
      }
    }
    if(fail==F & firstDone==F){
      i<-i+1
      testdigit<-substr(myStr,i,i)
      if(testdigit==","){
        firstDone<-T
      } else {
        fail <-T
      }
    }
    # If the first number is complete then try and find the second number
    if(fail==F & firstDone==T){
      i<-i+1
      testdigit<-substr(myStr,i,i)
      if(is.na(as.numeric(testdigit))==FALSE){
        secondnumber<-paste0(secondnumber,testdigit)
      } else {
        fail <- T
      }
    }
    if(fail==F & firstDone==T){
      i<-i+1
      testdigit<-substr(myStr,i,i)
      # If a bracket appears then it is the end of the second number
      if(testdigit==")"){
        secondDone<-T
      } else if(is.na(as.numeric(testdigit))==FALSE){
        secondnumber<-paste0(secondnumber,testdigit)
      } else {
        fail <- T
      }
    }
    if(fail==F & firstDone==T & secondDone==F){
      i<-i+1
      testdigit<-substr(myStr,i,i)
      if(testdigit==")"){
        secondDone<-T
      } else if(is.na(as.numeric(testdigit))==FALSE){
        secondnumber<-paste0(secondnumber,testdigit)
      } else {
        fail <- T
      }
    }
    if(fail==F & firstDone==T & secondDone==F){
      i<-i+1
      testdigit<-substr(myStr,i,i)
      if(testdigit==")"){
        secondDone<-T
      } else {
        fail <-T
      }
    }
    ## if the function was valid, multiply the numbers and add to final result
    if(fail==F & firstDone==T & secondDone==T){
      result<-result+as.numeric(firstnumber)*as.numeric(secondnumber)
    }
  } else {
    # iterate through the string
    i<-i+1
  }
  
  
}

## Read out the result
result


# Part 2

## Do the same but this time check if enabled or not
result<-0
i<-1
## Start with the functions enabled
enabled<-T
while(i<nchar(myStr)-4){
  ## enable the functions
  if(substr(myStr,i,i+3)=="do()"){
    enabled<-T
    i<-i+4
  ## disable the functions
  } else if(substr(myStr,i,i+6)=="don't()"){
    enabled<-F
    i<-i+7
    
  } else {
    if(substr(myStr,i,i+3)=="mul("){
      firstnumber<-""
      secondnumber<-""
      
      fail <-F
      firstDone <- F
      secondDone <- F
      i<-i+4
      testdigit<-substr(myStr,i,i)
      if(is.na(as.numeric(testdigit))==FALSE){
        firstnumber<-paste0(firstnumber,testdigit)
      } else {
        fail <- T
      }
      if(fail==F){
        i<-i+1
        testdigit<-substr(myStr,i,i)
        if(testdigit==","){
          firstDone<-T
        } else if(is.na(as.numeric(testdigit))==FALSE){
          firstnumber<-paste0(firstnumber,testdigit)
        } else {
          fail <- T
        }
      }
      if(fail==F & firstDone==F){
        i<-i+1
        testdigit<-substr(myStr,i,i)
        if(testdigit==","){
          firstDone<-T
        } else if(is.na(as.numeric(testdigit))==FALSE){
          firstnumber<-paste0(firstnumber,testdigit)
        } else {
          fail <- T
        }
      }
      if(fail==F & firstDone==F){
        i<-i+1
        testdigit<-substr(myStr,i,i)
        if(testdigit==","){
          firstDone<-T
        } else {
          fail <-T
        }
      }
      if(fail==F & firstDone==T){
        i<-i+1
        testdigit<-substr(myStr,i,i)
        if(is.na(as.numeric(testdigit))==FALSE){
          secondnumber<-paste0(secondnumber,testdigit)
        } else {
          fail <- T
        }
      }
      if(fail==F & firstDone==T){
        i<-i+1
        testdigit<-substr(myStr,i,i)
        if(testdigit==")"){
          secondDone<-T
        } else if(is.na(as.numeric(testdigit))==FALSE){
          secondnumber<-paste0(secondnumber,testdigit)
        } else {
          fail <- T
        }
      }
      if(fail==F & firstDone==T & secondDone==F){
        i<-i+1
        testdigit<-substr(myStr,i,i)
        if(testdigit==")"){
          secondDone<-T
        } else if(is.na(as.numeric(testdigit))==FALSE){
          secondnumber<-paste0(secondnumber,testdigit)
        } else {
          fail <- T
        }
      }
      if(fail==F & firstDone==T & secondDone==F){
        i<-i+1
        testdigit<-substr(myStr,i,i)
        if(testdigit==")"){
          secondDone<-T
        } else {
          fail <-T
        }
      }
      ## Only calculate if the function is enabled
      if(fail==F & firstDone==T & secondDone==T & enabled==T){
        result<-result+as.numeric(firstnumber)*as.numeric(secondnumber)
      }
      
    } else {
      i<-i+1
    }
  }
}

## Read out the result
result
