## I have prepared two functions to calculate the inverse of a matrix
## if it hasn't already been calculated yet. 

## The makeCacheMatrix function takes a matrix element as parameter
## and returns a list of sub-functions to alter the matrix.

## If a new matrix is sent as a parameter to makeCacheMatrix function, 
## automatically set sub-function is run and the new matrix is then
## stored as the queried matrix. Since it is a new matrix, it won't have
## its inverse calculated before. So inverse of the matrix is set to NULL.

## The get() sub-function returns the original matrix, 
## whose inverse will be calculated 

## The setInverse(parameter) method caches the inverse matrix given 
## as parameter, by setting up the value of "inverse" variable as
## the parameter sent.

## The getInverse() parameter returns the value of "inverse" variable

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL    
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(newInverse) inverse <<- newInverse
    getInverse <- function() inverse
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## In cacheSolve(parameters) function, the first parameter should be the
## returning value of the call of function makeCacheMatrix(matrix), where
## parameter "matrix" is the variable, whose inverse is going to be calculated.
## It first checks if the inverse of matrix already exists (not null) and
## prints the cached inverse, if so. If it doesn't already exist (null), 
## then it uses get() function to reach up to the original matrix and 
## uses the built-in solve() function on it. Since the inverse is freshly
## calculated, it caches the value by using setInverse(parameter) 
## sub-function and finally returns the inverse value.

cacheSolve <- function(x, ...) {
    
    inverse <- x$getInverse()
    if( ! is.null(inverse) )
    {
        message("Required data is found in cache!")
        return(inverse)
    }
    
    inverse <- solve(x$get())
    
    x$setInverse(inverse)
    
    inverse
}