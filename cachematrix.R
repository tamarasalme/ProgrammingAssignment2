## 2 functions that cache the inverse of a matrix, which can be a costly computation

## Function 1: makeCacheMatrix
## This function creates a special matrix object, which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() x
        
        ## Set the value of the inverse matrix
        setInv <- function(solved) inv <<- solved
        
        ## Get the value of the inverse matrix
        getInv <- function() inv
        
        ## List containing the four previous functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## Function 2: cacheSolve
## This function computes the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed,the inverse which was stored in the cache is returned.

cacheSolve <- function(x, ...) {
        
        ## Check if the inverse is stored in the cache
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("Getting cached inverse matrix")
                return(inv)
        }
        
        ## If the cache is empty or the matrix has changed, the inverse is computed
        data <- x$get()
        inv <- solve(data, ...)
        
        ## The inverse matrix is stored
        x$setInv(inv)
        inv
}
