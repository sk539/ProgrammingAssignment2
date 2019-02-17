## This function takes a square matrix as an input and then outputs the inverse.
## If a cached value is present, it will output the cached value, otherwise it wil compute a new inverse.


#This function takes an input matrix and stores the values into a list to be used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<-y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(mean) m <<- mean
    getinv <- function() m
    list (set = set, get = get, setinv = setinv, getinv=getinv)
    
    
}


## This function checks whether a cached value is present, if so, it will output the cached value
## Otherwise, it computes the a new inverse matrix and outputs the resut.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if (!is.null(m)) {
        message ("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
