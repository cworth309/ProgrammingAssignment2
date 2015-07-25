## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## Created by Carey W. Worth

makeCacheMatrix <- function(x = matrix()) {
    
    ## Create a null object
    matrixInvObj = NULL
    
    ## Create defaul setters and getters procedures
    set = function(y) {
        # use `<<-` to assign a value to an object in an environment
        # different from the current environment.
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) matrixInvObj <<- inverse
    getinv = function() matrixInvObj
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    
    ## Get the cached inverse object
    matrixInvObj = x$getinv()
    
    ## check if the inverse object has already been calculated and cached
    if (!is.null(matrixInvObj)){
        # get it from the cache and skips the computation.
        message("getting cached data")
        return(matrixInvObj)
    }
    
    # otherwise, calculates the inverse
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(matrixInvObj)
    
    ## Return inverse object
    return(matrixInvObj)
}
