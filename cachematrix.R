
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ##defining empty numeric matrix to hold inverse
  InvertedMatrix <- matrix(numeric(0),0,0)
    
  get <- function() {
    x
  }
  
  set <- function (y){
    InvertedMatrix <<- matrix(numeric(0),0,0)
    x <<- y
    
  }
   
  getInverse <- function(){
    InvertedMatrix

  }
  
  setInverse <- function(m){
    InvertedMatrix <<- m
  }
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix. If the inverse
## has already been calculated (and the matrix has not changed), then the cachesolve should  
## retrieve the inverse from the cache.  

cacheSolve <- function(x, ...) {
  
  ## I first read the inverse matrix object of x, if it has value(i.e. rows and cols are not zero) then return it from cache
  ## otherwise, retrieve data from x object, check if it is a square matrix, so calculate inverse and set the result to cache.
  
  inv <-  x$getInverse()
  rownum <- nrow(inv)
  colnum <- ncol(inv)
   
  if ( rownum != 0 && colnum !=0 ){  
    message("getting cached data")
    return(inv) 
  }
  
  data <- x$get()
  if ( nrow(data) != ncol(data)) {
    message ("Matrix is not square. Inverse cannot be calculated!")
    return(inv)
  }
    
  inv <- solve(data)
  x$setInverse(inv)
  inv
}


