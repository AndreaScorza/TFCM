check <- TRUE
while(check)
{
  for (i in unique(seq(1,20,1)))
  {
    if(i < 10)
    {
      if (i %% 2 == 0)
      {
        print(c(i, i+1))
      }
    }
    else
    {
      check <- FALSE
    }
  }
}


mysteryFunction <- function(digit,add) {
  result <- digit + add
  result }
slicerFunction <- function(digit,criterion=5) {
  result <- digit[digit<criterion]
  result }
test <- c(4,5,6,7,8,7,6,5,4,3,2,1) 
slicerFunction(test) 
slicerFunction(test,8)
mysteryFunction(test,5)

myCounter <- function(number) {
  num_vec <- c(number)
  result = number + num_vec[number]
  result
}

myCounter(4)
myCounter(number=4)
myCounter(c(4,10))
myCounter(1:10)
myCounter(10:1)

require(ggplot2)
qplot(c(4.3,5.2,6.9),c(10.1,11.2,11.9),xlab="x label", ylab="y label")

plot(seq(0,10,1),seq(0,10,1),pch=20,col=8) 
lines(c(0,10),c(10,0),col=3) 
points(rnorm(15,5,0.5),rnorm(15,4,0.5),col=2,pch=20) 
text(5,10, "test")
quartz(5,6)

par(mfrow=c(2,3))


a <- 0:9
b <- seq(0,0.9,0.1)
ind <- rep(c("A","B"),length(a)/2)
indAlt <- c(rep("C",length(a)/2),rep("D",length(a)/2)) 
myFrame <- data.frame(index1=ind,index2=indAlt,a,b)

names(myFrame) 
myFrame$index1 
myFrame$index2 
dim(myFrame)

myFrame[1:5,] 
myFrame[,1:2]

myFrame[myFrame$index1 == "A",] 
myFrame[myFrame$index1 == "A",]$b

myFrame$c <- "test"
myFrame$d <- myFrame$a %%2 
myFrame$diffa <- c(0, diff(myFrame$a)) 
myFrame$diffb <- c(0, diff(myFrame$b)) 
myFrame$diffd <- c(0, diff(myFrame$d))


mean(myFrame$a) 
summary(myFrame$a) 
summary(myFrame) 
levels(myFrame$index1)

myFrame

with(myFrame,tapply(a,list(index1,index2),median)) 
with(myFrame,aggregate(a,list(l1=index1,l2=index2),median))
with(myFrame[myFrame$index2 == "C",],aggregate(a,list(l1=index1,l2=index2),median))


fast <- rnorm(20,200,10)
slow <- rnorm(20,400,10)
t.test(a,b)
fast
slow


