## This function uses a nonsingular matrix as input, and output
## an inverse matrix. The inputs matrix is chached, so it is faster
## when it is used agsin.

## This "makeCacheMatrix" function takes a matrix as input and four
## methods are defined to manipulate the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m=NULL
  set<-function(y){
    x<<-y
    m=NULL
  }
  get<-function() x
  setInverse<-function(inverseMatrix){
    m<-inverseMatrix
  }
  getInverse<-function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The "CacheSolve" function takes a matrix as input and output a 
## inverse matrix, which is also cached so that it can be returned 
## when the function is called again.

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if (!is.null(m)){
    return(m)
  }
  data<-x$get()
  print(data)
  m<-solve(data)
  x$setInverse(m)
  m
}