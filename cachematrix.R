## Containts functions to:
##  - Create a special "matrix" object that can cache its inverse.
##  - Compute the inverse of the special "matrix" returned by ... 
##  makeCacheMatrix. If the inverse has already been calculated ...
## and the matrix has not changed, retrieves the inverse from the cache.


## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # set inverse to null
  set <- function(y) # set function
  {
    x <<- y # update value of matrix
    i <<- NULL # reset inverse
  }
  get <- function() x #return matrix
  setinverse <- function(inverse) i <<- inverse # set the inverse of the matrix
  getinverse <- function() i # return the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # default functions
  
}


## Compute the inverse of the special "matrix" returned by ... 
##  makeCacheMatrix. If the inverse has already been calculated ...
## and the matrix has not changed, retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # get the current inverse
  if(!is.null(i)) # check if previously calculated, if yes...
  {
    message("getting cached data") 
    return(i) # return previously calculated inverse
  }
  data <- x$get() # get the current matrix
  i <- solve(data, ...) # calculate the inverse
  x$setinverse(i) # update the inverse in the matrix object
  i # return the inverse
}
