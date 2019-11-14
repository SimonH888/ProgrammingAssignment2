## Put comments here that give an overall description of what your
## functions do

## The 'makeCacheMatrix' function creates a list that contains
## a function to store and access a matrix and a companion object,
## using the following mutator and accessor methods:
## 1.  set the value of the source matrix
## 2.  get the value of the source matrix
## 3.  set the value of the companion object
## 4.  get the value of the companion object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcompanion <- function(companion) m <<- companion
        getcompanion <- function() m
        list(set = set, get = get,
             setcompanion = setcompanion,
             getcompanion = getcompanion)
}


## The 'cacheSolve' function calculates the inverse of the
## matrix created using the 'makeCaheMatrix' function above.
## It assumes the matrix supplied is invertible.
## 'cacheSolve' checks whether a companion (in this case the inverted matrix)
## object has been cached and if so, returns the cached companion object.
## If no cache is found, the inverse of the matrix is calculated
## using solve() and cached for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getcompanion()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setcompanion(m)
        m
        
}
