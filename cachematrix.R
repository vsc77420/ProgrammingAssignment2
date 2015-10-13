## The cachematrix is divided in 2 functions: 
## - makeCacheMatrix that will store the matrix and the inverted matrix
## - cacheSolve that will returned the cached inverted matrix or that will
##   calculate  the inverted matrix , cache it and returns it to the user.

## makeCacheMatrix gives a list of 4 functions:
## - set that stores the matrix
## - get that gets back the matrix
## - setsolve that stores the inverted matrix
## - getsolve that gets back the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolved <- function(solve) s <<- solve
        getsolved <- function() s
        list(set = set, get = get, 
             setsolved = setsolved,
             getsolved = getsolved)
}


## cacheSolve will returns an inverted matrix with the help of solve function
## either returning the cached inverted matrix
## or calculate the inverted matrix, cache it and returning it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolved()
        if (!is.null(s)) {
                message("getting solved matrix")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolved(s)
        s
}
