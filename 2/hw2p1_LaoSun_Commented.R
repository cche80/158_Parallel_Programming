library(parallel)	# offers SNOW parallel programming capability
library(Rdsm)

# A program that calculates (u'vu) and put it IN PLACE in w. u is a vector and v is a matrix.
parquad <- function(u,v,w) {
  require(parallel)
  myidxsr <- splitIndices(nrow(v), myinfo$nwrkrs)[[myinfo$id]]
  #myidxsc <- splitIndices(ncol(v), myinfo$nwrkrs)[[myinfo$id]]
  #v_col<-ncol(v)
  temp_sum<-0
  for (i in myidxsr){
    for (j in 1:ncol(v)){
      temp_sum <- temp_sum + u[1,i]*v[i,j]*u[1,j]
    }
  }
  rdsmlock("vallock")
  w[1,1]<-w[1,1]+temp_sum
  rdsmunlock("vallock")
}
# Same as HJK's structure

# A program that calculates (u'vu) and put it IN PLACE in w. u is a vector and v is a matrix.
# LaoSun Style
mmulthread <- function(u,v,w) {
  require(parallel)
# change the whole row vector u into a colume vector for last step
  m <- matrix(u[1,],nrow=ncol(v),ncol=1)
# parallelism happens when u'v, then use this result to multiply u in the last CS step, split col
  myidxsr <- splitIndices(ncol(v), myinfo$nwrkrs)[[myinfo$id]] # [[]] reference a R-list gives vector
  u[,myidxsr]<-u[,]%*%v[,myidxsr]# in place update u, myidxsr is a vector
  
  rdsmlock("vallock")	# CS
  w[1,1]<-w[1,1]+u[,myidxsr]%*%m[myidxsr,]
  rdsmunlock("vallock")
  0		# prevent wasteful return
}


test <- function(cls) {
  mgrinit(cls)
  mgrmakevar(cls,"a",1,6)
  mgrmakevar(cls,"b",6,6)
  mgrmakevar(cls,"c",1,1)
  mgrmakelock(cls, "vallock")
  a[,] <- 1:6
  b[,] <- rep(1)
  c[1,1]<-0
  clusterExport(cls,"mmulthread")
  clusterEvalQ(cls,mmulthread(a,b,c))
  print(c[,])
}

