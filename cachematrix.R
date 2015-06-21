#
## The following functions provide a caching capabiliy for the inverting
## of a matrix.
#
# It is assumed that all matricies fed into these functions will be reversable so
# there is no special handling included for exceptions.
#
# Auth: aburow
# Date: 2015.6.21
#

makeCacheMatrix <- function(x = matrix()) {
  # This function is used to manipulate the cacheing of a matrix
  #
  # Auth: aburow
  # Date: 2015.6.21
  #
  
  # Init variables for cacheing
  i.cache <<- NULL
  m.cache <<- x
  
  # set new matrix and clear inverse to indicate a change
  set <- function(x) {
    m.cache <<- x
    i.cache <<- NULL
  }
  
  # return the matrix
  get <- function() m.cache
  
  # set the inverse of the matrix, initialises cache capability
  setinverse <- function(inverse) {
    i.cache <<- inverse
  }
  
  # return the inverse of the matrix, return the cached value
  getinverse <- function() i.cache
  
  ## list of available methods
  list(
       set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
   )

}


cacheSolve <- function(x, ...) {
  # Return a cacheable matrix that is the inverse of 'x'
  # Auth: aburow
  # Date: 2015.6.21

  # "i" will be NULL on a first pass
  # "i" will be !NULL if value is already calculated
  i <- x$getinverse()
  
  # check state of "i" - if "i" !null then exit the function returning the cached inverse matrix
  if( !is.null(i) ) {
    message("using cached solution")
    return(i)
  }

  # if the solution isn't pre-cached then the rest of the function
  # calculates the new matrix and sets the cache values using the
  # methods in the object.
  
  # retrieve the cached matrix into "mx"
  mx <- x$get()
  
  # solve the problem.. in this case the inverse of a matrix
  i <- solve(mx)
  
  # store the solution back into the cache object
  x$setinverse(i)
  
  # exit the function with the resultant solution
  return(i)
}

