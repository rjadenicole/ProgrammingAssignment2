## Cache the inverse of a matrix as the inverse calculation can be costly.

## Create a special matrix object that can cache its inverse. It allows 
## one to set and get the actual matrix in addition to set and get the 
## inverse of the actual matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setData <- function(y) {
                x <<- y
                i <<- NULL
        }
        getData <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i
        list(setData = setData, getData=getData, setInverse=setInverse, getInverse=getInverse)
}


## Get the inverse of a special matrix created by makeCacheMatrix()
## If the inverse is not calculated, calculate it and save it in cache.
## Otherwise, return the cached inverse directly
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$inverse
        if (is.null(i)) {
                data <- x$getData()
                i <- solve(data)
                x$setInverse(i)
        }
        i
}
