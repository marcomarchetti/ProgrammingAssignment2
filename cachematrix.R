##---------------------------------------------------------
## Marco Marchetti
## 30 Apr 2017
## Pair of functions that cache the inverse of a matrix.
##---------------------------------------------------------


#---------------------------------------------------------
# makeMx_Cache 
# cache matrix from a matrix
#---------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  
  Mx_Cache <- NULL
  
  Mx_Set <- function(y) {
    x <<- y
    Mx_Cache <<- NULL
  }
  
  Mx_Get <- function() x
  setcache <- function(inverse) Mx_Cache <<- inverse
  getcache <- function() Mx_Cache
  
  list(Mx_Set = Mx_Set,
       Mx_Get = Mx_Get,
       setcache = setcache,
       getcache = getcache)
}

#---------------------------------------------------------
# cacheSolve
# return inverse matrix using cache
#---------------------------------------------------------
cacheSolve <- function(x, ...) {
  
  Mx_Cache <- x$getcache()
  
  if (!is.null(Mx_Cache)) {
    message("getting cached matrix...")
    return(Mx_Cache)
  }
  else {
    Data_Matrix <- x$Mx_Get()
    Mx_Cache <- solve(Data_Matrix, ...)
    x$setcache(Mx_Cache)
    return(Mx_Cache)
  }
}

## Sample
x = rbind(c(1, 1/3, 1/5), c(1/3, 1, 1/5), c(1/3, 1/5, 1))
m = makeCacheMatrix(x)
m$Mx_Get()

## No cache
cacheSolve(m)

## Retrieving from the cache
cacheSolve(m)
