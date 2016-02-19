## Put comments here that give an overall description of what your
## functions do:
## used together, these functions implement a cache for matrix inverses. The first function implements a list
## of functions to set and retrieve a matrix and its inverse; the second function returns the inverse of the 
## matrix through its computation (should it has not been computed yet) or by accessing the cache (implemented 
## by mean of accessing the getinverse method defined within the former function). 

## Write a short comment describing this function
## Implementation of method set-sets new vaues for matrix-, get-to get current value of matrix-,
## setinverse -sets values of inversed matrix- and getinverse -to retrieve inversed matrix-.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
## set x and m with y and NULL. x an m are defined in the parent environement with regard to y, therefore <<- operator is used.
                x <<- y
                m <<- NULL
        }
        get <- function() x
## set m with passed object inv. m is defined in the parent environement with regard to inv, therefore <<- operator is used.
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## It checks wether inverse has already been cached, by mean of getinverse function. 
## If getinverse function returns a no null matrix, this is returned. Otherwise, it is computed, through solve function,
## cached, through the setinverse function, and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
