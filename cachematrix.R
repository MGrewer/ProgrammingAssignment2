makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## sets the cache to empty by default (it's not been calculated at this point)
  set <- function(y) {
    x <<- y
    m <<- NULL ## makes sure the cache is empty when you set the matrix to something new
  } 
  
  get <- function() x ## returns the matrix
  setInverse <- function(Solve) m <<- Solve ## called during cacheSolve
  getInverse <- function() m ## returns the inverse of the matrix
  
  ## The list command allows these commands to be called outside the function (ex.x$get)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse() ## Pull in the data from the cache, if it exists.  Will return NULL if no data exists
  if(!is.null(m)) { ## if not NULL return the following message and print cahced data
    message("getting cached data")
    return(m)  ## this ends the function without doing any of the below calculations
  }
  data <- x$get()  ## Pull in the matrix, to then calculate the inverse
  m <- solve(data, ...) ## calculate the inverse
  x$setInverse(m) ## cache the calculated inverse
  m  ## print the inverse
}