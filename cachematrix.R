## The following functions enable caching the inverse of a matrix so that 
## it is not computed every time it is needed

## makeCacheMatrix function initializes the required variables in global 
## environment and returns a list of the function objects

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve function first checks if the inverse of matrix passed as argument 
## already exists in the cache and returns it if it does exists. If not, the 
## inverse is now computed and stored in the cache and it is returned.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
