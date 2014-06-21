
## This R file is used to speed up matrix inversion by caching the
## inverse of a matrixonce it has been calculated becuase matrix 
## inversioncan be a costly computation.
##-----------------------------------------------------------------------------------
## Example usage
## 1. Read the R code from the R file cachematrix.R
## 2. Create a square matrix
## 3. Call makeCacheMatrix which will create the "special matrix", actually a list
## 4. Call cacheSolve to get the inverse of B. This is the first time the
##    inverse has been asked for so it will calculate it and show a message
##    indicating the inverse has been calculated
## 5. Call cacheSolve again. This will get the inverse from the cache and
##    show a message indicating the cache value has been used
##-------------------------------------------------------------------------
## > source("cachematrix.R")
## > B = matrix(c(2, 4, 3, 1, 5, 7, 10, 20, 4), nrow=3, ncol=3)
## > K <- makeCacheMatrix(B)
## > kinv <- cacheSolve(K)
## > kinv <- cacheSolve(K)
##-----------------------------------------------------------------------------------


## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## This function creates a "matrix" with additional attributes. It is actually a list containing a 
## function that will do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
##
## The final capability, getting the inverse of the matrix is done with the cacheSolve function below
makeCacheMatrix <- function(x = matrix()) {
   
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is a function that calculate the inverse of the makeCacheMatrix object. If first checks
## to see if the inverse has been calculated already. If it has not, it calculates the inverse and  
## caches it. If the inverse has already been calculated, the function returs the value in cache 
## using the setinverse function
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    message("calculating inverse")
    inv
}


