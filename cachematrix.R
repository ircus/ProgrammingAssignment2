## The first function enriches a matrix x with 4 functions that can cache the matrix and its inverse
## in a different environment and return them to the console.
## The second function checks if the inverse of a matrix x has been cached and retuns it if so,
## calculates and returns the inverse otherwise.



## The function makeCacheMatrix below returns 4 other functions:
## get() returns x - the the formal argument of the main function makeCacheMatrix,
## set(y) assigns y to x in a different environment and assigns NULL to the local variable inv,
## setinv(inverse) assigns inverse to inv in a different environment
## getinv() returns inv from the other environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## The function cacheSolve below uses the functions from makeCacheMatrix to
## assign the cached inverse matrix to a local variable inv.
## Then it checks that there was something cached and returns it if so,
## if not calculates the inverse, chaches it, and returns to the console.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x
    inv
}
