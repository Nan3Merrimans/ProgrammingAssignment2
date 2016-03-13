## These two functions allow you to cach the inverse of a square and
## invertible matrix rather than and then solve for the inverse 
## multiple times.


## makeCacheMatrix creates a matrix which is really a list containing functions to 
   ## 1.  set the matrix 
   ## 2.  get the matrix 
   ## 3.  solve and cache the value of the inverse of the matrix 
   ## 4.  get the cached value of the inverse of the matrix 


makeCacheMatrix <- function ( x = matrix()) {
    invX <- NULL
    set <- function(y){
        x<<- y
        invX <<- NULL
    }
    get<- function() x
    setInverse<- function(solve) invX <<- solve
    getInverse<-function() invX
    list(
        set=set,
        get=get,
        setInverse=setInverse,
        getInverse=getInverse )

}


## cacheSolve is to check if the inverse of the matrix 'x' is available in the 
## cache and if it is, return it.  Otherwise compute inverse of 'x' and 
## save it in the cache and return the solution 


cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

