## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "Matrix".
makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        
        ## 1. The value of the matrix is set
        set <- function(y) {
                x <<- y
                r <<- NULL
        }
        
        ## 2. Get the value of the matrix
        get <- function() x
        
        ##3. Set the inverse of the matrix
        setinverse <- function(solve) r <<- solve
        
        ##4. Set the inverse of the matrix
        getinverse <- function() r
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function calculates the inverse of the matrix.
## If the inverse of the matrix already exists in the cache memory,this value is used

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## 1. Search for the value
        r <- x$getinverse()
        
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        
        #2. If value does not exist yet, calculate it
        r <- solve(data, ...)
        x$setinverse(r)
        r
        
}
