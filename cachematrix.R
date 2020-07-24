## Put comments here that give an overall description of what your
## functions do
## creates a cached matrix Object: a colletion of functions from which
## the matrix and inverse values can be acces or set up

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get<-function()x
        setInverse<-function(Inverse) m <<- Inverse
        getInverse<-function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## takes as input a cached matrix Object,
## gets the value of the cached inverse, if the value has been calculated
## it gets returned, if it hasn't been calculated the inverse is solved
## and it's value stored inside the cached matrix Object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}