## Put comments here that give an overall description of what your
## functions do
# The makeCacheMatrix function allows for a reversible matrix to be 1, Created (set), Returned (get), 
#     Inverted (setInverse), Inverted Matrix to be returned (getInverse)


makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) Inv <<- solve
    getInverse <- function() Inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# The cacheSolve function allows speeds up the process to calculate an inverted Matrix. If the Matrix has already been 
#     run it will return the value of the Inverted matrix, otherwise it will calculate the new inverted matrix and store it.


cacheSolve <- function(x, ...) {
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    message("New matrix and its inverse needs to be calculated")
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInverse(Inv)
    Inv
}
