## Cache the inverse of a Matrix and return the cached value if the Matrix
## has not changed

## Object to cache the Matrix and the inverse value. It clears the cache if we
## set a new Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(xParam) {
        x <<- xParam
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(invParam) inv <<- invParam
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Returns the Inverse of a Matrix in a makeCacheMatrix object. If the
## inverse has been calculated before then it returns the chached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached inverse matrix")
        return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx)
    x$setInv(inv)
    inv
}
