## Task: by using lexical scoping logic, write down the code for two functions 
## which together calculate, cache and retrieve from cache the inverse matrix  
## of a given matrix.

## makeCacheMatrix function creates a list containing 4 functions
## get(), set(), setinv(), getinv() and stores the matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
        kk <- NULL
        set <- function(y) {
                x <<- y
                kk <<- NULL
        }
        get <- function() x
        setinv <- function(inv) kk <<- inv
        getinv <- function() kk
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function calculates inverse or retrieves the inverse if already
## calculated and stored in makeCacheMatrix environment. In this case,
## R message "getting cached data" appears.

cacheSolve <- function(x, ...) {
        kk <- x$getinv()
        if (!is.null(kk)) {
                message("getting cached data")
                return(kk)
        }
        data <- x$get()
        kk <- solve(data, ...)
        x$setinv(kk)
        kk
        ## Return a matrix that is the inverse of 'x'
}
