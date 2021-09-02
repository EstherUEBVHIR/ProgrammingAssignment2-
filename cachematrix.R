##1##	makeCacheMatrix:	This function creates a special "matrix" object that can cache its inverse. 	

makeCacheMatrix <- function(x = matrix()) { # I define the function makeCacheMatrix.
    i <- NULL	
  set <- function(y) {	# I set the value of the matrix.
          x <<- y
          i <<- NULL
  }
  get <- function() x	# I get the value of the matrix. 
  setinv <- function(inv) i <<- inv	# I set the value of the inverse of the matrix.	
  getinv <- function() i	# I get the value of the inverse of the matrix.		
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##############################################################################################################

##2##	cacheSolve:	This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { # I define the function cacheSolve.
  i <- x$getinv()	
  if (!is.null(i)) {	# I check that the inverse has been calculated.
          return(i)
  }
  data <- x$get()	# I get the inverse from the cache.
  i <- solve(data, ...)
  x$setinv(i)
  i
}