## Descrition: 
##  Two functions 'makeCacheMatrix' and 'cacheSolve' to calculate 
##  and store the inverse of a matrix (using the 'solve' function).
##  For optimizing repeated calls with identical input data, the  
##  computed results are stored in a cache.


## Function 'makeCacheMatrix' returns a new cache-matrix object.
##  If a matrix is given as a parameter it will be stored in the object.
##  Functions that can be called on this object:
##    $set(m) ... sets the matrix of the object
##    $get() ... returns the matrix of the object (NULL if not set)
##    $setInv(im) ... sets the inverse matrix of the object (=the cache)
##    $getInv() ... returns the inverse matrix of the object (NULL if not set)

makeCacheMatrix <- function(x = matrix()) 
{
    #initialize the cache
    cachedInv <- NULL
    #define local functions for setting and
    # getting the matrix and its inverse
    set <- function(m) 
    {
        x <<- m
        cachedInv <<- NULL
    }
    get <- function() 
    {
        x
    }
    setInv <- function(im)
    {
        cachedInv <<- im
    }
    getInv <- function() 
    {
        cachedInv
    }
    #return a list containing the local functions
    list(set=set, get=get, 
         setInv=setInv, getInv=getInv)
}


## Function 'cacheSolve' calculates the inverse of a matrix 
##  given as parameter x, which is a cache-matrix object (see 
##  function 'makeCacheMatrix' above)
##  If the inverse was calculated before, the cached result is returned.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    cachedInv <- x$getInv()
    if ( is.null(cachedInv) ) 
    {
        #cache is empty, so we need to calculate 
        # the result (and store it in the cache)
        data <- x$get()
        cachedInv <- solve(data, ...)
        x$setInv(cachedInv)
    }
    else #found in cache
    {
        #optinal message (for debugging)
        message("getting cached data")
    }
    #return value
    cachedInv
}



