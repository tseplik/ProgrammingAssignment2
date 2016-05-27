## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##  makeCacheMatrix: this function creates a list that cache matrix and its inverse.
##  the <<- operator used to assign a value to an object in an environment that is different
##  from the current environment

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


## Write a short comment describing this function

##  for this assignment, we assume that the matrix supplied is always invertible.
##  i.e. the matrix is always a square matrix
##  computing the inverse of a square matrix done with solve function in R

##  computing the inverse of a non-square matrix can be done with ginv function in R
##  ginv: Calculates the Moore-Penrose generalized inverse of a matrix X.

##  cacheSolve: this function computes the inverse of the matrix or take it from "cache" 
##  if the inverse already calculated. 


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    message("no cached data. compute the inverse")
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

##  testing

##      X <- matrix(rnorm(16), nrow = 4)    ## create matrix
##      cacheX <- makeCacheMatrix(X)        ## cache it
##      cacheSolve(cacheX)                  ## catculate the inverse
##      cacheSolve(cacheX)                  ## matrix didn't change
##      invX <- cacheSolve(cacheX)          ## 
##      zapsmall(invX %*% X)                ## check the inverse

