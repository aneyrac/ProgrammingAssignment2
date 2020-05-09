##These functions calculate and cache the inverse of a matrix
##When the user wants to calculate the inverse of a matrix previously computed,
##the saved value is returned instead of doing the calculation again

##This function creates a special "matrix" object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##define the cache m
  set <- function(y){ ##set the matrix and re-initialize m in the parent environment
    x <<- y 
    m <<- NULL
  }
  get <- function() x ##get the matrix
  setinverse <- function(inverse) m <<- inverse ##set the inverse of the matrix
  getinverse <- function() m ##get the inverse
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


##This function calculates the inverse of the object obtained from makeCacheMatrix
##If the inverse has already been calculated, the function returns the saved inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse() 
  if(!is.null(m)){ ##return the inverse if it has already been calculated and cached
    message("getting cached data")
    return(m)
  }
  data <- x$get() ##if it hasn´t, calculate the inverse, set it to the object, an finally return it
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
