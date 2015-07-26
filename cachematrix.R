## Put comments here that give an overall description of what your
## functions do

##Exercise for Programming Assignment 2 in Programming in R for JHU Data Specialization Course July 2015

##The assignment is to to write a pair of functions that avoid unnecessarily repeating inversions of matrices by caching the inversions and retrieving the caches rather than re-computing the inverse
## IF the matrix has changed since the last inversion, the inversion needs to be re-done. (but i'm not sure how to do this part...?)

##The code below is based on the two examples for calculating and caching the mean provided in the assignment


makeCacheMatrix <- function(myx = matrix()) {
 myInverse <- NULL
 set <- function(x) {
        myx <<- x;
        myInverse <<- NULL;
    }
    get <- function() (myx);
    setinv <- function(solve) myInverse <<- solve;
    getinv <- function() myInverse;
    list(set = set, get = get, 
          setinv = setinv, 
          getinv = getinv)    
}



##cacheSolve <- function(x, ...) {
        ## Return a matrix that is (hopefullly?)the inverse of 'x'
##}


## Compute the inverse of the
## "matrix" returned by `makeCacheMatrix` above, provided the matrix has not changed, otherwise re-calc inverse
## return the inverse...

cacheSolve <- function(myx, ...) {
    myInverse <- myx$getinv()
    if(!is.null(myInverse)) {
        message("Cache exists... now retrieving cached data...")
        myInverse
    }	
    myData <- myx$get()
    myInverse <- solve(myData, ...)
    myx$setinv(myInverse)
   myInverse
}
