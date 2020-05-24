## Two functions named "makeCacheMatrix" and "cacheSolve" are being 
## created to cache the inverse of a matrix

## The makeCacheMatrix function creates an invertible square matrix
## that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skip the computation 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculate the inverse 
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        
        # set the value of the inverse in the cache via the setinv function
        x$setinv(inv)
        
        return(inv)
}
