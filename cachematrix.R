## Matrix inversion can be computational heavy so there may be some
## benefit to caching the inverse of a matrix rather than computing
## it repeatedly

## The two functions below is used to cache the inverse of a matrix

## The first function, makeCacheMatrix, creates a list with a function
## to set and get the value and inverse value of a matrix


makeCacheMatrix <- function(x = matrix()) { 
  inverse <- NULL 
  set <- function(y) { 
    x <<- y; 
    inverse <<- NULL; 
  } 
  get <- function() return(x); 
  setinv <- function(inv) inverse <<- inv; 
  getinv <- function() return(inverse); 
  return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
} 


## The second function, cacheSolve, caches the value of the
## inverse matrix 

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data.")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  return(inverse)
}

## test
## > x = rbind(c(1, 2), c(2, 1))
## > x
## [,1] [,2]
## [1,]    1    2
## [2,]    2    1
## > m = makeCacheMatrix(x)
## > cacheSolve(m)
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## > cacheSolve(m)
## Getting cached data.
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333


