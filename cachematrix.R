## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It is really a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    
    ## Caches the inverse matrix.
    inverse_matrix <- NULL
    
    ## Sets the matrix.
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    
    ## Returns the matrix.
    get <- function() {
        x
    }
    
    ## Sets the inverse matrix.
    setinverse <- function(inverse){
        inverse_matrix <<- inverse
    }
    
    ## Returns the cached inverse matrix.
    getinverse <- function() {
        inverse_matrix
    }
    
    ## Returns our special "matrix".
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## This function calculates the inverse of the special "matrix". 
## First it checks if the inverse matrix has already been calculated.
## If it has, it retrieves the inverse matrix from the cache, it displays a message, and skips calculation.
## Otherwise, it calculates the inverse matrix and stores it in the cache.
cacheSolve <- function(x, ...) {
    inverse_matrix <- x$getinverse()
    
    if (!is.null(inverse_matrix)) {
        
        ## The inverse matrix is in the cache, so it retrieves it and displays a message.
        message("getting cached data")
        return(inverse_matrix)
    }
    
    ## The inverse matrix is not yet calculated, so it calculates and stores it in the cache.
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$setinverse(inverse_matrix)
    
    ## Returns a matrix that is the inverse of 'x'
    inverse_matrix
}
