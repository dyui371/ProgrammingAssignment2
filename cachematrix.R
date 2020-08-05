## Watcharee Nontiprasert , 8 Aug 2020
## Put comments here that give an overall description of what your
## functions do
## This function is able to cache potentially time-consuming computations
## To test result please see 
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
 
## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
  }
    get <- function() x
  
    setInverse <- function (inverse) m <<- inverse
    getInverse <- function () m
    list(set = set, get = get , 
         setInverse = setInverse ,
         getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getInverse() 
   if(!is.null(m)) {
       message("getting cached data")
       return(m)
 }
   data <- x$get()
   m <- solve(data,...)
   x$setInverse(m)
   m
}
