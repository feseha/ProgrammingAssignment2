##Programming Assignment 2: Lexical Scoping
##Feseha's attempt

## this function takes a matrix cache's its inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(w) m <<- w
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
             
}


## this function takes a matrix and caches its inverse or if it has already been
## cached it will grab the cached inverse bypassing the processof caching it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                        m <- x$getmatrix()
		        if(!is.null(m)) {
		                message("getting cached inverse matrix")
		                return(m)
		        }
		        data <- x$get()
		        m <- solve(data, ...)
		        x$setmatrix(m)
        m
}
