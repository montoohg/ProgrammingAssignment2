
## Programming assignment 2 - To compute the square of an inversible matrix using solve().
## Assumption - Matrix supplied is always invertible

## Function takes a matrix and return a list containing functions to set & get matrix and set & get the inverse of matrix.  


makeCacheMatrix <- function(X = matrix()) {  # X is the input invertible matrix.
  inver <- NULL
  set <- function(y) {
    X <<- y
    inver <<- NULL
  }
  get <- function() X
  setinverse <- function(solve) inver <<- solve
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to calculate the inverse of a matrix. Using 'if' it checks if its calcualted the inverse. It will either return the cached
## inverse matrix with the message 'getting cached data' or calcualte the inverse and return the inverse matrix

cacheSolve <- function(X, ...) {
  inver <- X$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data_mat <- X$get()
  inver <- solve(data_mat, ...)
  X$setinverse(inver)
  inver
}  


## Return a matrix that is the inverse of 'x'

