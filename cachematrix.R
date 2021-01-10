## This is the assignment to write function for 
## fCaching the Inverse of a Matrix

## makeCacheMatrix try to  create
## a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    
    cached_Inverse_matrix <- NULL
    
    set <- function(y) {
        x <<- y
        cached_Inverse_matrix <<- NULL
    }
    
    
    
    get <- function() x
    setsolve <- function(solve) cached_Inverse_matrix <<- solve
    getsolve <- function() cached_Inverse_matrix
    list(set = set, get = get,
         setsolve  = setsolve ,
         getsolve = getsolve)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    cached_Inverse_matrix <- x$getsolve()
    
    
    if(!is.null(cached_Inverse_matrix)) {
        message("getting cached data / 已找到缓存数据")
        return(cached_Inverse_matrix)
    }
    data <- x$get()
    cached_Inverse_matrix <- solve(data, ...)
    x$setsolve(cached_Inverse_matrix)
    cached_Inverse_matrix
}