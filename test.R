require(ggplot2)

check <- TRUE
while(check)
{
  for (i in unique(seq(2,20,2)))
  {
    if(i < 20)
    {
      if (i %% 2 == 0)
      {
        #print(c(i, i+1))
      }
    }
    else
    {
      check <- FALSE
    }
  }
}
tito <-c(1,2,3,3,4,5)
#print (tito)
#print(unique(tito))
#print (seq(0,10))

mysteryFunction	<- function(digit,add)
{
  result	<- digit	+	add
  result
}
tito <- tito[tito<3]
#print(tito)

#print(mysteryFunction(3,2))

print("test")
r <- 10:1
#print(r)

#stampa le x del primo vettore e le y del secondo vettore
#qplot(c(4.3,5.2,6.9),c(10.1,11.2,11.9),xlab="x	label",	ylab="y	label")

#pch è la forma dei pallini, col è il colore
#plot(seq(0,10,1),seq(0,10,1),pch=20,col=2)
#lines(c(0,10),c(10,0),col=3)
#points(rnorm(15,5,0.5),rnorm(15,4,0.5),col=4,pch=20)

#scrive test alle coordinate x=5 e x=9
text(5,9, "test")

#apre una nuova finestra di plot
#quartz()
#plot(5,3)

# stampa 10 volte il valore 5
#print (rep(5,10))

a <- 0:9
b <- seq(0,0.9,0.1)
ind <- rep(c("A","B"),length(a)/2)
indAlt <- c(rep("C",length(a)/2),rep("D",length(a)/2))
myFrame <- data.frame(index1=ind,index2=indAlt,a,b) #passo 4 variabili rinominando ind e indAlt in rispettivamente index1 e index2

names(myFrame)
myFrame$index1
myFrame$index2 
dim(myFrame) #divisi per numero di osservazioni e numero di variabili


#myFrame[1:5,] #stampa le prime cinque observation (righe, da 1 a 5)
#myFrame[,1:2] # stampa le colonne, da 1 a 2

myFrame[myFrame$index1 == "A",] #stampa le righe dove index1 è uguale ad A
myFrame[myFrame$index1 == "A",]$b #stampa la variabile b dove index1 = A

# diff calcola la differenza tra gli elementi di un vettore di posizione i e i+1 , settiamo il primo elemento a zero perchè sennò non potremmo calcoalre la differenza 
diff(myFrame$a)
print(c(0,3))
myFrame$c	<- "test"
myFrame$d	<- myFrame$a	%%2
myFrame$diffa	<- c(0, diff(myFrame$a))
myFrame$diffb <- c(0, diff(myFrame$b))
myFrame$diffd <- c(0, diff(myFrame$d))


mean(myFrame$a)
summary(myFrame$a)
summary(myFrame)
levels(myFrame$index1)

with(myFrame,tapply(a,list(index1,index2),median))
with(myFrame,aggregate(a,list(l1=index1,l2=index2),median))
with(myFrame[myFrame$index2	==	
               "C",],aggregate(a,list(l1=index1,l2=index2),median))

# rnorm(n, mean, sd)
# n = number of random variables to generate
# mean = if not specified defaults to 0
# sd = standard deviation

fast	<- rnorm(20,200,10)
slow	<- rnorm(20,400,10)
t.test(fast,slow)

