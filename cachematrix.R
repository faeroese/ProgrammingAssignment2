## Following pair of functions compute and cache the inverse of a matrix
## so that it does not need to be computed repeatedly.

##Sample usage: x<-matrix(c(2, 4, 3, 1), 2,2)
##x1<-makeCacheMatrix(x)
##cacheSolve(x1)

##makeCacheMatrix: This function creates a special
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##check to ensure the matrix is square -although we are assuming this will never fire 
  ##as the matrix should always be invertable I wanted to try and add some error 
  ##checking in for practice
  if (sqrt(length(x))%%1 > 0) 
                {print("Bad input. The input for makeCacheMatrix function should be 
                 in the format of an already constructed square matrix e.g
                  x<-matrix(c(2, 4, 3, 1), 2,2) where x is fed into makeCacheMatrix 
                  (e.g. makeCacheMatrix(x)) or you can feed matrix(c(2, 4, 3, 1), 2,2) 
                  straight into makeCacheMatrix. 
                  You cannot feed a 3,2 matrix for example as this is not sqaure")}
 
  else {
  cachedata <- NULL
  set <- function(y) {
    x <<- y
    cachedata <<- NULL
  }
  get <- function() x
  setinvmat <- function(solve) cachedata <<- solve
  getinvmat <- function() cachedata
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}}


##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  cachedata <- x$getinvmat()
  if(!is.null(cachedata)) {
    message("getting cached data")
    return(cachedata)
  }
  data <- x$get()
  cachedata <- solve(data, ...)
  x$setinvmat(cachedata)
  cachedata
}