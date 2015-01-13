## This pair of functions cache the inverse of a matrix to save 
## potentially time-consuming computations rather than compute 
## it repeatedly. 

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse. It is a list containing a functions 
## to set the value of the matrix, get the value of the matrix, set 
## the value of inverse of the matrix, get the value of inverse of 
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        inv_mtrx <- NULL
        set <- function(y) {
                x <<- y
                inv_mtrx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_mtrx <<- inverse
        getinverse <- function() inv_mtrx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv_mtrx <- x$getinverse()
        if(!is.null(inv_mtrx)) {
                message("getting cached data")
                return(inv_mtrx)
        }
        data <- x$get()
        inv_mtrx <- solve(data, ...)
        x$setinverse(inv_mtrx)
        inv_mtrx
}

## Output:
## > mtrx <- matrix(c(1,2,7,8), nrow=2, ncol=2, byrow = TRUE)
## > mtrx_test <- makeCacheMatrix(mtrx)
## > mtrx_test$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    7    8
## > cacheSolve(mtrx_test)
##      [,1]       [,2]
## [1,] -1.333333  0.3333333
## [2,]  1.166667 -0.1666667
