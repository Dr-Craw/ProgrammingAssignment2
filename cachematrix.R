
## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # A1 will hold inverse matrix
        A1 <- NULL
        
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                A1 <<- NULL
        }
        
        #get the value of the matrix
        get <- function() x
        
        #set the value of the inverse
        setinverse <- function(inverse) A1 <<- inverse
        
        #get the value of the inverse
        getinverse <- function() A1
        
        #list all new functiuons
        list(set = set, get = get,  setinverse = setinverse, getinverse = getinverse)
           
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        A1 <- x$getinverse()
        #get inverse if it has been calculated
        if(!is.null(A1)) {
                message("getting cached data")
                return(A1)
        }
        # calculate the inverse
        data <- x$get()
        A1 <- solve(data, ...)
        
        #store the inverse
        x$setinverse(A1)
        
        #display the inverse
        A1
}


