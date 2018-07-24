## The following functions create a special object that stores a matrix and 
## caches its inverse

## The first function 'makeCacheMatrix' creates a special "matrix" object that
## stores the values of matrix and its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The function below calculates the inverse of the special "matrix"
## created with the above function. If the inverse has already been calculated,
## it get`s the inverse from the cache and skips the computation. And if not,
## it calculates the inverse of the matrix and sets the value of the inverse
##in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <-x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
