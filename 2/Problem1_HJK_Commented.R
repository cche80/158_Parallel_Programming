library(parallel)
library(Rdsm)

# A program that calculates (u'Au) and put it IN PLACE in val. u is a vector and A is a matrix.
parquad <- function(u, a, val){
  require(parallel)		# include parallel library
  myidxs <- splitIndices(nrow(a), myinfo$nwrkrs)[[myinfo$id]]	# job splitting by rows of matrix a (naive parallelism)
  a_col<-ncol(a)
  temp_sum<-0
  for (i in myidxs){
    for (j in 1:a_col){		# parallelism happens when u'(Au), aka Au is NOT parallelized.
      temp_sum <- temp_sum + u[1,i]*a[i,j]*u[1,j]	# use a local variable to lower memory traffic
    }
  }
  rdsmlock("vallock")	# CS access: update global counter val
  val[1,1]<-val[1,1]+temp_sum
  rdsmunlock("vallock")
}

myprint <- function(val){
  val[1,1]=99
}

#Create cluster
cls <- makeCluster(2)
clusterExport(cls, "parquad")	# export the functions to them
				# (no need to export variables because they are shared!)
#clusterExport(cls, "myprint")	# for testing

# init clusters using Rdsm
mgrinit(cls)

#init 3 shared variables and 1 lock among all clusters
mgrmakevar(cls, "val", 1,1)
mgrmakevar(cls, "u", 1,3)
mgrmakevar(cls, "a", 3,2)
mgrmakelock(cls, "vallock")

#setup test variables
a[1,1]<-10
a[1,2]<-11
a[2,1]<-12
a[2,2]<-13
a[3,1]<-14
a[3,2]<-15

u[1,1]<-1
u[1,2]<-2
u[1,3]<-3

val[1,1]<-0

clusterEvalQ(cls, parquad(u, a, val))	# RUN!

stopCluster(cls)
