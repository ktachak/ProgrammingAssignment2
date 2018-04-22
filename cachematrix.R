## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is a matrix
    ## Return a list containing the matrix x and the
    ## inverse of x if already computed and the functions
    ## to set and get the inverse
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }

    get <- function() x
    setInv <- function(inv) invM <<- inv
    getInv <- function() invM
    list(set = set, get = get,
         setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, flush=FALSE) {
    ## 'x' is a special list containing a matrix
    ## and the accessor functions.
    
    ## 'flush' defaults to FALSE, if true discard
    ## the old cache value and recompute.
    
    ## Return a matrix that is the inverse of 'x'
    ## check if inverse if already computed in cache.
    invM <- x$getInv()
    if (!is.null(invM) && !flush) {
        message("getting cached data")
        return(invM)
    }
    
    mat <- x$get()
    invM <- solve(mat)
    x$setInv(invM)
    invM
}
