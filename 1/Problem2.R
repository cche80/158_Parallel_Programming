library(pixmap)
library(NMF)
getapprox <- function(a,k){
   # plot(mtr)
    mtr<-a@grey
    aout<-nmf(mtr, k)
    w<-aout@fit@W
    h<-aout@fit@H
    approxa<-w %*% h
    approxa<- pmin(approxa, 1)
    mtrnew<-a
    mtrnew@grey <-approxa
    return(mtrnew) 
}

mtr<- read.pnm('MtRush.pgm')
newmtr = getapprox(mtr, 50)

plotmae <- function(a,kvec){
  vec=rep(0,times=length(kvec))
  for (i in 1:length(kvec))
  {
    cat("Now k=",kvec[i],"\n")
    mtrnew = getapprox(a, kvec[i])
    vec[i]=sum(abs(mtrnew@grey-a@grey)/(length(mtr@grey)))
  }
  plot(kvec, vec, ylab="Error", xlab="order of k")
  return(vec)
}

# plotmae(mtr, 30:50)

# Write code to investigate the relation of degree of approximation to rank, as follows.
# We will define the mean absolute error (MAE) as the average absolute difference between the true pixel brightness and approximate one, with the averaging taking place over all pixels of the image. Write a function with "declaration"

# plotmae <- function(a,kvec)
# where a is as above and kvec is a vector of ranks (assumed to be in ascending order). The function will find the MAE for each rank, and plot the former against the latter. See my quick tutorial.
