## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly.
## Thus, the given code generates cached data for finding the inverse of a matrix.


## The makeCacheMatrix function creates a special matrix object that can cache its inverse
## Currently, the input is assumed to be in form of a non-singular, invertible matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  setMatrix<-function(y){x<<-matrix(y,nrow=sqrt(length(y)),ncol=sqrt(length(y)))}
  getMatrix<-function() matrix(x,nrow=sqrt(length(x)),ncol=sqrt(length(x)))
   setInverse<-function(i){inv<<-i}
  getInverse<-function(){inv}
  list(set=setMatrix,get=getMatrix,setInv=setInverse,getInv=getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv))
  {
    message("Getting Cached Data")
    return(inv)
  }
  
  inv<-solve(x$get(),...)
  x$setInv(inv)
  inv
}
  
  
  

