## These functions can be used to find and cache the inverse of a matrix.
## 
## The function makeCacheMatrix takes a matrix as input and returns an object...
## that both caches the original matrix and creates space to cache its inverse.
## e.g. matCache <- makeCacheMatrix(mat)
## 
## The function cacheSolve takes an object created by makeCacheMatrix and...
## finds the inverse of the original matrix using the solve function.
## The inverse is cached inside the object and subsequent calls of cacheSolve
## will return the cached inverse rather than using the solve function again.
## e.g. matInv <- cacheSolve(matCache)

##  makeCacheMatrix takes a matrix as input and returns a CacheMatrix
##  object (list of functions) that can store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  sx <- NULL
  set <- function(y) {
    x <<- y
    sx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) sx <<- inv
  getinv <- function() sx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##  cacheSolve takes a CacheMatrix object as input and returns its inverse

cacheSolve <- function(x, ...) {
  sx <- x$getinv()
  if(!is.null(sx)) {
    message("getting cached inverse matrix")
    return(sx)
  }
  matrix <- x$get()
  sx <- solve(matrix, ...)
  x$setinv(sx)
  sx
}
