## This will Cache the Inverse of a Matrix:
## Matrix inversion is usually a expensive computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly and saves time considerably for large datasets.
## Below written are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL ## setting inverse initially to null
  set <- function(b) {
    a <<- b
    inv <<- NULL 
  }
  get <- function() a
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will compute the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix have not changed), then it will retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'a'
  inv <- a$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- a$get()
  inv <- solve(mat, ...)
  a$setInverse(inv)
  inv
}
