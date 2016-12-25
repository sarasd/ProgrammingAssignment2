## Calculate the inverse of a matrix. 
## Using scoping rules we first check if the inverse has already been calculated and return that value.
## We otherwise calculated and return it. 


## Creat an object of type makeCacheMatrix which includes the matrix, the inverse of the matrix 
## and the functions of the makeCacheMatrix which are getters and setters functions

makeCacheMatrix <- function(x = matrix()) {
  ## return a list of objects to feed the cacheSolve function
  inv <- NULL 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv )
  }

## Gets the inverse of the matrix. It fist checks if the inverse has already been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
