## These two functions, makeCacheMatrix and cacheSolve, are very similar to the 
## example functions, makeVector and cacheMean. makeCacheMatrix is a function 
## which creates a special kind of matrix, the inverse of which can be cached. 
## cacheSolve is a function which calculates the inverse of a square matrix, and 
## if the inverse has already been calculated it retrieves the inverse matrix from
## memory rather than computing it again. 

## There are two important variables in this function, x and i. x is the original
## matrix supplied and i is the inverse. x can be supplied when makeCacheMatrix
## is called and it will be retained in the global environment. Four functions are
## also defined within the makeCacheMatrix environment, one each for setting the
## matrix and its inverse, and one each for their retrieval. When setting a new
## matrix, ie. a new value for x using $set, i is set to null so that a new 
## inverse has to be calculated. The inverse can also be set manually using
## $setinverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve uses the solve function in R to compute the inverse of a matrix. If
## the value of i, the variable we previously set up as the inverse matrix, is not
## equal to NULL then cacheSolve will simply return the already stored value of i.
## If it is equal to null then the function will calculate the inverse and return it. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  else{
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }
        ## Return a matrix that is the inverse of 'x'
