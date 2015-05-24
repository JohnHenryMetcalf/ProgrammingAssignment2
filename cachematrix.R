## The below function creates a special cache "Matrix" that actually makes a list object of functions 
##to set the matrix, get the matrix, set inverse of the function, and get the inverse of the function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


##The finds the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already 
##been found. If so, it gets the inverse from cache instead of running solve() again. 
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
