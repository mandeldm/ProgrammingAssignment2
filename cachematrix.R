## This code uses 2 functions to invert a matrix and to cache its
## inverse so as to avoid repeating time-consuming calculations.

## This function, makeCacheMatrix, returns a list containing 
## 4 functions, which:
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the value of the inverse matrix
##	4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function, cacheSolve, checks whether the inverse of the input
## matrix has already been computed. If yes, it returns the inverse
## from cache. If not, it inverts the matrix, caches the result &
## returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
