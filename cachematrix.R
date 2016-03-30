## Put comments here that give an overall description of what your
## functions do

## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(inverse) m <<- inverse
        getinvmatrix <- function() m
        list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}

## This function computes the inverse of the special "matrix" created by "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {
       m <- x$getinvmatrix()
       
       if(!is.null(m)) {
                message("getting cached data")
                return(m)
       }
       
       data <- x$get()
       m <- solve(data, ...)
       
       x$setinvmatrix(m)
       m
}
