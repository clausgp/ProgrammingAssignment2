
## makeCacheMatrix defines a list of 4 functions to be
## used on a matrix to help create its inverted matrix
## and cache both the matrix and its inverted matrix
## To be used in conjunction with cacheSolve
## set        : change the cached matrix
## get        : returns the cached matrix
## setimatrix : stores the 'inverse' matrix
## getimatrix : returns the stored 'inverse' matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setimatrix <- function(imatrix) m <<- imatrix
        getimatrix <- function() m
        list(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
}

## cacheSolve returns an inverse matrix
## Has to be used in conjunction with makeCacheMatrix
## to create the helping functions first
## example use : imatrlist <- makeCacheMatrix(some matrix)
##               cacheSolve(imatrlist)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getimatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setimatrix(m)
        m
}
