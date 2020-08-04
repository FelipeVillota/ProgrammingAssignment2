## With this two functions we will show how the state inside of an object is 
## preserved.
## by FELIPE VILLOTA


## The following function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set1 <- function(inverse) m <<- inverse
        get1 <- function() m
        list(set = set, get = get,
             set1 = set1,
             get1 = get1)
}


## This function will retrieve the inverse of makeCacheMatrix 
## (if the object hasn't changed).

cacheSolve <- function(x, ...) {
        
        m<- x$get1()
        if(!is.null(m)) {
                message("cached computation")
                return(m)
        }
        
        z<- x$get()
        m<- solve(z,...)
        x$set1(m)
        m
        
        
        
}
