library(parallel)
library(pixmap)
library(NMF)


getapprox <- function(a,k){
  # plot(mtr)
  mtr<-a
  aout<-nmf(mtr, k)
  w<-aout@fit@W
  h<-aout@fit@H
  approxa<-w %*% h
  approxa<- pmin(approxa, 1)
  return(approxa) 
}



nmfsnow <-function(cls, a, k){
  # a is mtr
  inmtr<-a@grey
  rowgrps <- splitIndices(nrow(inmtr), length(cls))
  unchunks <- Map(function(grp) inmtr[grp,] , rowgrps)
  mout <- clusterApply(cls, unchunks,nmf,k)
  mapproxa <- lapply(mout, function(temp) (temp@fit@W %*% temp@fit@H))
  mapproxa <- lapply(mapproxa, function(temp) pmin(temp, 1))
  newm<-(matrix(Reduce(c, lapply(mapproxa, function(temp) Reduce(c, t(temp)))), ncol=a@size[2], byrow=TRUE))
  newmtr<-a
  newmtr@grey<-newm
  return(newmtr)
}

# test cluster
mtr<- read.pnm('MtRush.pgm')
#c1<-makePSOCKcluster(rep("localhost",4))
#newmtr<-nmfsnow(c1, mtr, 30)
#plot(newmtr)



plottimes <- function(cls, a, k, clssizeevec){
  if (length(clssizeevec)<=0){
    cat("clssizevec size invalid!")
  }
  timevec = rep(0,times=length(clssizeevec))
  for (i in 1:length(clssizeevec))
  {
    cat("Now is ", i,"/",length(clssizeevec), " test\n")
    temp<-system.time(nmfsnow(cls[1:unlist(clssizeevec[i])], a, k))
    timevec[i]=unname(temp[3])
  }
  plot(clssizeevec, timevec, ylab="Elapse Time in second", xlab="# of Nodes")
  return(timevec)
}
c1<-makePSOCKcluster(rep("localhost",16))
plottimes(c1, mtr, 60, list(1,3,5,7,9,11))
stopCluster(c1)
