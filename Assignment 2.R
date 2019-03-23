 ## Write a short comment describing this function.
 ## This function makeCacheMatrix can calculate the inverse of a matrix/
  makeCacheMatrix <- function( x = matrix()) {
    invMat <- NULL
    set <-function(y) {
      x <<- y
      invMat <<- NULL
    }
    
    get <- function() x
    setinverse <- function(result) invMat <<- result
    getinverse <- function() invMat
    list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
  }

  ## set - assigns matrix passed as the argument
  ## get - returns the original matrix
  ## setinverse - set the martix inversion
  ## getinverse - get the result of matrix inversion
  
  ## Write a short comment describing this function.
  ## This function cacheSolve can retrieve the inverse of matrix.
  cacheSolve <- function(x, ...){
    ## Return a matrix that is the inverse of 'x'
    invMat <- x$getinv()
    if(!is.null(invMat)){
      message("Getting cached data: ")
      return(invMat)
    }
    mat <- x$get()
    invMat <- solve(mat, ...)
    x$setinverse(invMat)
    invMat
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  