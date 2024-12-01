# Advent of Code Day 1
# Part 1

## Read in the file
data<-read.table("Data/day01.txt",col.names=c("one","two"))

## Sort the two lists
list_one<-sort(data$one)
list_two<-sort(data$two)

## Find the differences between the two lists
diff<-abs(list_one-list_two)

## Read out the result
sum(diff)


# Part 2

## Initialise the similarity score
sim_score<-0
## Loop over each item in list one
for(i in 1:length(list_one)){
  ## find the count of the item in list two
  count<-sum(list_two==list_one[i])
  ## increase similarity score
  sim_score<-sim_score+count*list_one[i]
}

## Read out the result
sim_score

