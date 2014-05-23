## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a matrix "object" that includes the original matrix data x, 
#the cached inverse i, and 4 functions needed to access and modify the matrix cache
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}


## Write a short comment describing this function
#This function checks to see if the inverse value is already set for the 
#matrix object. If not, it pulls out the matrix data with get(), solves
#for the inverse, and sets the inverse value using setInverse()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getInverse()
  if (!is.null(i)) {
    message("Getting cached data")    
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
