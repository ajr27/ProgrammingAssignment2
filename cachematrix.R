## The following functions store the result of a matrix calculation in the cache and check
## for whether that object already exists in the cache.

## makeCacheMatrix creates an empty variable which will store the matrix on which cacheSolve
## will be called along with other functions to be used in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setMat <- function(s) mat <<- s
  getMat <- function() mat
  list(set = set, get = get,
       setMat = setMat,
       getMat = getMat)
}


## cacheSolve first refers to the result of the makeCacheMatrix function to get the subject
## matrix then checks whether the result of the inverse of that matrix is in the cache, if
## so, it retrieves and returns it, otherwise, it calculates the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getMat()
  if(!is.null(mat)){
    message("Getting cached data...")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setMat(mat)
  mat
}
