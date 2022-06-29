#install.packages("Rcpp") # uncomment if not installed
library(Rcpp)
setwd("~/Masters/CProg/Assig2/RanWalk")
sourceCpp("ranwalk.cpp")

#This looks like at the walk function
walk(5,seed = 20)
walk2(10,seed =21)
p1=c()
p2=c()
p3=c()
for(i in 1:10000){
  w=walk2(1000,seed=1)
  p1=c(p1,length(which(w==0)))
  p2=c(p2,length(which(w==1)))
  p3=c(p3,length(which(w==2)))
}
#Question 1
#Has the destination [1,-3] reached after walking a 100 roads? returns a vector of Bool, as destination might have been reached twice.
TRUE%in%Destination(as.integer(c(-1,3)),100,seed = 19) #yes destination has been reached
which(Destination(as.integer(c(-1,3)),100,seed = 1)==TRUE) #It has been reached after walking 61 roads

#Question2
yes20 = rep(0,1000) #stores 1 if reached wihin 20 and 0 if not
for(i in 1:1000){
  walks = Destination(as.integer(c(1,-3)),20,seed = i)
  #
  if(TRUE%in%walks){
    yes20[i] = 1
  
  }
}
sum(yes20)/1000 #probability is 0.11 of reaching destination within walking 20 roads


#Question 3
#compare the two walk functions
library(rbenchmark)
benchmark(walk(100,2),walk2(100,2),replications(100),order = "relative") 
#error using benchmark error so it wont work - "Error in formula.default(object, env = baseenv()) : invalid formula"
#-------
#Using microbenchmark instead
library(microbenchmark)
microbenchmark(walk1 = {walk(100000,2)},walk2 = {walk2(100000,2)}, times = 1000L, unit = "milliseconds")
#interesting to see my second walk function is more efficient
#------


#Question 4
#Manhattan distance
avgD <- c()
for(i in 1:1000){
  avgD<-c(avgD,ManhattanD(50))
}
mean(avgD) # = 11 for walk2 and 15 for walk
#my worry here is tha the walk functions are equivalent
