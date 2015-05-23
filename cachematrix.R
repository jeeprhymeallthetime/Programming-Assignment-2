## The makeCacheMatrix funciton makes a list of functions.  One to set a matrix, one to retrieve a matrix
## set an inverse matrix and retrieve an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This funciton first checks to see if the value set using the makeCacheMatrix function contains anything in the holder
## for inverse functions.  If it is NULL, the inverse matrix is solved for and set into the makeCacheMatrix value

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
}
    