## William Adriano Alves
## University of Sao Carlos @ Sorocaba, Brazil

## R Programming, Programming Assignment 2:
## Lexical Scoping: caching the inverse of a matrix

## Below are two function that are used to (1) create
## and store a matrix and store its inverse and
## (2) to verify if there was already been calculated
## the inverse of the matrix and: return this value if
## so, or calculate and store if no.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv){
        inverse <<- inv
    }
    
    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) 
}

cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    
    if(!is.null(inverse)){
        message("getting cached data") 
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
