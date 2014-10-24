# R Programming | Coursera | Fall 2014
# Programming Assignment 2
# https://github.com/lcpalm/ProgrammingAssignment2

# The functions makeCacheMatrix() and cacheSolve() are a pair of functions used together 
# to use a special 'inverse caching matrix' which can keep a cached copy of its inverse.
# A new 'inverse-caching matrix' object is created by giving the desired matrix as input
# to makeCacheMatrix; for example:
# > ICmatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))   
# Then the inverse of the stored matrix can be cached in ICmatrix by running cacheSolve:
# > cacheSolve(ICmatrix)
# The 'inverse-caching matrix' object has associated functions to access its components:
# > ICmatrix$get()          # returns the original matrix
# > ICmatrix$getinverse()   # returns the inverse if it has been cached and NULL otherwise
# > ICmatrix$set(x)         # given a matrix x, changes the matrix stored in ICmatrix to x


# Given a matrix, this function returns a list of four functions. 
# The functions $get, $set, $getinverse are used to return the matrix, to reset the 
# existing matrix to a different one, and to return the matrix inverse (if previously 
# cached using cacheSolve(), otherwise it returns NULL) respectively. A fourth function
# $setinverse() is provided for internal use by the companion function cacheSolve() 
# and should not be called directly by the user.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getmatrix <- function() {x}
    setinverse <- function(theInverse) {inverse <<- theInverse}
    getinverse <- function() {inverse}
    list(set = setmatrix,  
         get = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Given an 'inverse-caching matrix', x, previously created using the function makeCacheMatrix(),
# this function returns the inverse of the matrix stored in x, and also caches it within x.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseFromCache <- x$getinverse()
    if(!is.null(inverseFromCache)) {
        message("getting cached data")
        return(inverseFromCache)
    }
    data <- x$get()
    inverseCalculated <- solve(data, ...) 
    x$setinverse(inverseCalculated)
    inverseCalculated
}
