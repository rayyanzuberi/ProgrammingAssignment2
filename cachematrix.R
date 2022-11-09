## Overall: These two functions are used to create a matrix (first function).
## and provide a number of helper functions in order to perform subsequent
## inversion and caching (still first function). The second function is used to
## perform the inversion if there is no cached value and store this value in the
## cache. However, if there is a cached value, this second function will not
## perform the inversion but take the value from the cache.

## Function that sets the value of the matrix, gets the value of the matrix
## Function is also able to set the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is able to take the matrix from makeCacheMatrix
## and uses this to generate the inverse using R's solve function; 
## however it only performs this solving in the case that the answer is not
## already cached by the function, in which case it takes the inverse from the 
## cache and doesn't solve the matrix using the function. In  the event that a 
## new calculation does occur, the setinverse function is used to set the value
## of the inverse in the cache.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}