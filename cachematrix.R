    ## function makeCacheMatrix set from outside into variables matrix
    ## and its inverse and print values of matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    InvMatrix <- NULL
        ## set new values to matrix and InvMatrix to Null
    set <- function(y) {  
      x <<- y
      InvMatrix <<- NULL
    }
        ## get matrix
    get <- function() x  
        ##set values to InvMatrix from outside
    setInverse <- function(solve) InvMatrix <<- solve 
        ##get InvMatrix
    getInverse <- function() InvMatrix 
        ##list of all this functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


    ## cacheSolve return a matrix that is the inverse of 'x
cacheSolve <- function(x, ...) {
      ## first read matrix with makeCacheMatrix
  Iv <- x$getInverse()
      ##check if we have inverse and return it if it is so
  if(!is.null(Iv)) {
    message("getting cached data")
    return(Iv)
  }
      ##if it is not we read matrix
  data <- x$get()
      ## and find its inverse
  Iv <- solve(data, ...)
      ##then put inverse of matrix to cache
  x$setInverse(Iv)
      ## and return this inverse
  Iv
}
