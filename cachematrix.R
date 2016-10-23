## This assignment is made up of two functions that calculates and stores the inverse of a matrix, as per the the assignment instructions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL 
    set <- function(y) { 
      x <<- y 
      m <<- NULL 
    }
    get <- function() x 
    setinverse <- function(solve) m <<- solve 
    getinverse <- function() m 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse() 
    if(!is.null(m)) {
      message("getting cached data")
      return(m) 
    }
    data <- x$get() 
    m <- solve(data, ...) 
    x$setinverse(m) 
    m 
  }
  
## Return a matrix that is the inverse of 'x'

## Example: enter the code below to return a matrix that is the inverse of 'x'.

x<-matrix(c(1,0,5,2,1,6,3,4,0),3,3,byrow=FALSE)
makeCacheMatrix(x)
cacheSolve(makeCacheMatrix(x))