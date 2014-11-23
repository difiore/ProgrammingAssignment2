## makeCacheMatrix creates a special "vector", which is really a list containing a function to
# set the value of a matrix
# get the value of a matrix
# set the value of the inverse of a matrix
# get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvmat <- function(solve) m <<- solve
    getinvmat <- function() m
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
    
}

## cacheSolve returns the inverse of a matrix from a cache (if that has been completed) or get the matrix itself from the cache and takes the inverse (if the inverse has not already been cached)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinvmat()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmat(m)
    m
}

mat <- matrix(c(2, 4, 3, 1), nrow=2, ncol=2, byrow = TRUE)
cachemat<-makeCacheMatrix(mat)
solution<-cacheSolve(cachemat)
solution
