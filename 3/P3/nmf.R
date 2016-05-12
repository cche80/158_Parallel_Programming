nmfgpu <- function(a,k) {
    resultVector = .Call("nmfgpuC", a, as.integer(nrow(a)), as.integer(ncol(a)), as.integer(k), as.integer(100))
    list( matrix(resultVector[1:(nrow(a)*k)], nrow=nrow(a), ncol=k, byrow =TRUE), matrix(resultVector[(nrow(a)*k+1):length(resultVector)], nrow=k, ncol=ncol(a), byrow = TRUE) )
}