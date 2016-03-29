## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(solve) inv <<- solve
        getinvmatrix <- function() inv
        list(set = set, get = get, setinvmatrix = setinvmatrix, getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       inv <- x$getinvmatrix()
       
       if(!is.null(inv)) {
               message("fetching cache")
               return(inv)
               
       }
       data <- x$get()
       inv <- solve(data, ...)
       
       x$setinvmatrix(inv)
       inv
}
