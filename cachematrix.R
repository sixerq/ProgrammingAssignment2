## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. 
##The following functions cache the inverse of a matrix.

## This makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.
## This contains a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matric
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        V <- NULL
        set <- function(y) {
                x <<- y
                V <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) V <<- inverse
        getinverse <- function() V
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        V <- x$getinverse()
        if(!is.null(V)) {
                message("getting cached data.")
                return(V)
        }
        data <- x$get()
        V <- solve(data, ...)
        x$setinverse(V)
        V
}
