## makeCacheMatrix(x) contructs a matrix "object" (from the
## matrix primitive x) which can cache its own inverse,
## after the first time the inverse is computed. 
## Whenever the matrix inverse is requested, the
## cached inverse is returned (if available) in order to avoid
## unnecessary repeated computation of the inverse.
##
## cacheSolve(x) returns the inverse of the matrix "object" x,
## which has been constructed by makeCacheMatrix().
## The inverse of matrix object x is only actually computed once,
## and then cached in the matrix object. Subsequent calls to
## cacheSolve() only return the cached inverse, rather than
## recomputing it.


## Function: makeCacheMatrix(x)
##
## Description:
## 
## makeCacheMatrix(x) contructs a matrix "object" (from the matrix primitive x) which can cache its own inverse,
## after the first time the inverse is computed. Whenever the matrix inverse is requested, the cached inverse is 
## returned (if available) in order to avoid unnecessary repeated computation of the inverse.
##
## Arguments:
##
## x        the input matrix primitive
##
## Returns:
## 
## A list of access functions (set, get, setinverse, getinverse)
##
## Assumptions:
##
## Assumes matrix x is invertible

makeCacheMatrix <- function(x = matrix()) {
  
    ## Initialize cached matrix inverse in local scope
    inv <- NULL
    
    ## Set internal matrix primitive to passed-in y matrix
    ## Set cached matrix inverse to NULL since internal matrix primitive has changed
    ## These assignments are done in the enclosing environment (the calling environment)
    ## using the "superassigment" operator
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Return internal matrix primitive
    get <- function() x
    
    ## Set internal matrix inverse to passed-in inverse, again using the superassignment operator
    setinverse <- function(inverse) inv <<- inverse
    
    ## Return internal matrix inverse
    getinverse <- function() inv
    
    ## Construct and return a list of the cacheMatrix access functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}

## Function: cacheSolve(x)
##
## Description:
## 
## cacheSolve(x) returns the inverse of the matrix "object" x, which has been constructed by makeCacheMatrix().
## The inverse of matrix object x is only actually computed once, and then cached in the matrix object. Subsequent calls to
## cacheSolve() only return the cached inverse, rather than recomputing it.##

## Arguments:
##
## x        the input matrix object created by makeCacheMatrix()
##
## Returns:
## 
## The inverse of the input matrix object x
##
## Assumptions:
##
## Assumes matrix x is invertible

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x', by returning the non-NULL cached inverse matrix
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Cached inverse matrix is NULL, so compute inverse for the first time using solve()
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
