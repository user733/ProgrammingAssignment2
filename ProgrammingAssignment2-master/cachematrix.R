#makeCacheMatrix function: This function creates a special "matrix" object that can cache its inverse.

#cacheSolve function: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## this function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.na(inv)) {
    print("getting cached data")
    return(inv)
  }
  data <- matrix()
  data <- x$get()
  
  if(nrow(data)==ncol(data))
    inv <- solve(data, ...)
  x$setinverse(inv)
  inv
        
}
