## Caches matrix inversion to save CPU time

## makeCacheMatrix - object to hold the matrix inverted results
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve - take makeCacheMatrix object, caches results
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

testCacheMatrix <- function(){
  mat<-matrix(c(1,1,1,3,4,3,3,3,4),3,3)
  message("created test matrix")
  print(mat)
  message("creating cache matrix")
  cacheMat <- makeCacheMatrix(mat)
  message("inverting cache matrix")
  print(cacheSolve(cacheMat))
  message("inverting cache matrix again")
  print(cacheSolve(cacheMat))
}