## These functions will work with matrix and caching and creating inverses as needed

# cache the matrix
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set.f <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get.f <- function() x
  set.inverse <- function(inverse) invrs <<- inverse
  get.inverse <- function() invrs
  out <- list(set=set.f, get=get.f, setinverse=set.inverse, getinverse=get.inverse)
  return(out)
}


# # use to test function
# x = rbind(c(1, -1/4), c(-1/4, 1))
# m = makeCacheMatrix(x)
# m$get()



## Solve for the inverse using cached data
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}



## test the cache
cacheSolve(m)

## check that it gets the cache
cacheSolve(m)
