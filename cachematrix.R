#Create a special "matrix" object that can cache its inverse. 
#param it takes matrix as an Argument
#return A list containing four functions
        # 1 set the value of the matrix
        # 2 get the value of the matrix
        # 3 set the value of the inverse of the matrix using the solve() function
        # 4 get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Define function to set the value of the matrix. It also clears the old
    # inverse from the cache
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Define function to get the value of the matrix
    get <- function() x
    # Define function to set the inverse, It only used by getinv() if inv is not cached
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    # Return a list of four functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve retrieves the 
# inverse from the cache.
# 
#param x a special matrix created with makeCacheMatrix
# 
#return The inverse of the matrix x


cacheSolve <- function(x, ...) {
    # This fetches the cached value for the inverse
    inv <- x$getinv()
    # if inv is not null, returing from cache, by checking not null.
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    
    # Fetching matrix data
    data <- x$get()
    # Calculating inverse
    inv <- solve(data, ...)
    # Setting inverse in cache
    x$setinv(inv)
    # Return inverse
    inv
}
