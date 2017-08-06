##the first function, makeVector creates a special "vector", which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) { ##function set the vaule of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x  ##return the value of the matrix
  setinverse <- function(inverse) i <<- inverse ##set the inverse of the matrix
  getinverse <- function() i  ##get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setminverse,
       getinverse = getinverse)
}

## the second function is to find whether there have stored the inverse of the matrix, if so, use the value that calcuted previously. 
##if not, calculate the inverse value of the matrix.

cacheinverse <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)  ##matrix that is the inverse of i will be returned
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinverse(i)
  i ##return the matrix
}
