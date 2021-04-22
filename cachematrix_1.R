## Test function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## inverse function: inv() or inverse()

makeCacheMatrix <- function(x = matrix()) {
        m=NULL
        set = function(y){
        x <<- y 
        m <<- NULL        
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set=set,
             get=get,
             setinv = setinv,
             getinv = getinv)
             
        
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

## not yet finished.... 04/22/21
