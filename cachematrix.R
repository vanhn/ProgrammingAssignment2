## Put comments here that give an overall description of what your
## functions do

## This function is used to make matrix and inverse matrix in the cache
## set and get function for matrix
## setinverse and getinverse for inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is used to calculate inverse matrix using solve function
## If the inverse matrix has been calculated previously, the function return calculated inverse matrix
## If the inverse matrix hasn't been calculated previously, the function will calculate the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
