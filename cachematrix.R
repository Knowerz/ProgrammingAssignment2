## The two functions contained in this file will cache the inverse of a matrix
## and then, when attempting to solve a matrix, will check the cache to see if th
## information is already cached. If so it will print out the cache, if not it will
## solve accordingly.

## makeCacheMatrix takes a matrix as its input and caches its inverse
## within a special vector that contains the functions to:
## A) Set the value of the matrix
## B) Get the value of the matrix
## C) Set the value of the inverse
## D) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
 	cacheinv <- NULL
      set <- function(y) 
	{
            x <<- y
            cacheinv  <<- NULL
      }
      get <- function() x
      setinv <- function(solve) cacheinv <<- solve
      getinv <- function() cacheinv
      list(set = set, 
	     get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve takes a matrix as its input and returns the inverse of that matrix.
## It first checks to see if the information has already been cached by makeCacheMatrix
## In order to save time computing the inverse. If it has not been cached, it will cache and
## return the inverse.
## NOTE ** This function assumes that any matrix given as an input is invertible **

cacheSolve <- function(x, ...) 
{
      inv <- x$getinv()
      if(!is.null(inv)) 
	## Checking to see if the inverse has already been calculated and cached
	{
      	message("Returning cached data...")
      	return(inv)
      }
      ## Otherwise it now calculates, caches, and returns
	data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
