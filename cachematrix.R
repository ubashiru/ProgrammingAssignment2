## Put comments here that give an overall description of what your
## functions do
## The aim in this experiment is to write a pair of functions, namely "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              #initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}     #function to get matrix x
  setinverse <- function(inverse) {inv <<-inverse}
  getinverse <- function() {inv}          #function to obtain inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
        
}
