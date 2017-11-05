## Two functions were created below.
## The 1st function "makeCacheMatrix" create a matrix object that can cache its inverse
## computed by the second function "cacheSolve". 
## The 2nd function "cacheSolve" computes the inverse of the "matrix" 
## returned by the 1st function "makeCacheMatrix".
## If the inverse has already been computed and the matrix has not changed, 
## the "cacheSolve" will retrieve the inverse from the cache (i.e., the 1st function).


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}


## This function computes the inverse of the "matrix" return by "makeCacheMatrix".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrix(m)
      m
}


# Function testing
my_matrixA <- matrix(c(1:4), 2, 2)
my_matrixB <- matrix(c(50, 60, 70, 80), 2, 2)
aMatrix <- makeCacheMatrix(my_matrixA)
aMatrix$get()
aMatrix$getmatrix()
aMatrix$set(my_matrixB)
cacheSolve(aMatrix)
aMatrix$getmatrix()









