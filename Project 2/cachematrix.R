## makeCacheMatrix and cacheSolve will create a matrix and solve
## for the inverse.  The inverse will be stored, so that it won't
## have to be repeatedly calculated.

## makeCacheMatrix will allow a matrix to be created and will 
## store (cache) its inverse.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve will find the inverse of a matrix. Before it does 
##the actual calculations,it will check to see if the inverse 
##of the matrix has been calculated before. If it has, it will
##print the previously solved answer, instead of doing the 
##calculation again.

cacheSolve <- function(x, ...) {
       
  m <- x$getsolve()
        ## Checks to see if calculation has already been done.
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
      ## Solves for the inverse 
  data <- x$get()
  m<- solve(data,...)
  x$setsolve(m)
  m
}
