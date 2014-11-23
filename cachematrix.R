## This pair of function implements the inversion of a matrix
## Once computed the inverted matrix is cached, so the second time cacheSolve
## is called the result is not computed again but retrieved from the cache
##
## example call:
## mcm <- makeCacheMatrix(matrix(c(2,0,0,2), 2, 2))
## cacheSolve(mcm)
## cacheSolve(mcm)


## Returns a list of helper functions (set/get matrix/inverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setinv <- function(inv) {m <<- inv}
    getinv <- function() {m}
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## computes the inverse of a matrix (assuming the inverse exists)
## returns cached inverse if it has already been calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
