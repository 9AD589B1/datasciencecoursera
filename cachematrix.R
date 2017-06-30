## The below functions will allow us to create a matrix, compute its inverse,
## cache its inverse, and check for a cached result to avoid re-computing the
## same matrix inverse

## This function will create a list vector of functions that:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the matrix inverse (using the solve function)
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
	inv <- NULL
    set <- function(y) {
  
          x <<- y
          inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
	   
}

## This function will compute a matrix inversion unless the same computation
## has been previously performed

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  ##if inv is not null, then retrive the cache, else compute solve()
  if (!is.null(inv)) {
  
          message("Getting cached data")
          return(inv)
		  
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
