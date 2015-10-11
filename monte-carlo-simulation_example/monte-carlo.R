#Set seed to generate Random Numbers
set.seed(1)

plot_hist <- function(X,n){
  hist(X,probability=TRUE,xlab="x", main="Histogram of the draws of X",col="grey")
}

plot_density <- function(X){
  dens <- function(X) dexp(X,0.2)
  curve(dens, add = TRUE, col = "red")
}

simulate <- function(n){
  #Random sample execution time for module X1,X2,X3
  X = rexp(3,rate=0.2)

  #Total execution time of the system
  X = max(X)
  #Simulating for n trials
  X = replicate(n,max(rexp(3,rate=0.2)))
  plot(X)
  #histograph of X
  #plot_hist(X,n)
  #density curve of X
  #plot_density(X)
  return (mean(X))
}

y <- replicate(5,simulate(1000))
mean(y)
y <- replicate(5,simulate(10000))
mean(y)
y <- replicate(5,simulate(100000))
mean(y)