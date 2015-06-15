## The functions allow to define a matrix and calculate the inverse of the matrix, 
## only if no inverse has been calculated before (or at least stored before)

## Function creates and gets a matrix, creates and gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function verifies if there is a inverse matrix stored
## If there is a inverse matrix stored, it returns it
## If not it calculates the inverse matrix and stores it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv        
}
