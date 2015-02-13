## These are a pair of functions that cache the inverse of a matrix
## 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    if (!exists("v")) v <<- NULL # create an empty vector to cache the inverse if needed
    setMatrix <- function(y) {
        oldMatrix <<- y
        v <<- NULL
    } # a function to cache the matrix for comparison and Null to the inverse
    getMatrix <- function() x # a function to get the new matrix
    setSolve <- function(solve) v <<- solve # a function to cache the inverse
    getSolve <- function() v # a function to retrieve the cached inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix, setSolve = setSolve,
         getSolve = getSolve) # return the list of functions
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    v <- x$getSolve() #retrieve the cached inverse
    if(!is.null(v)) { # if there was a value cached as inverse
        if(sum(oldMatrix != x$getMatrix())==0){ # and the matrix didn't change
            message("Getting cached matrix inverse:") # than give the message
            return(v) # and return the cached inverse without calculating it
        }
    } # otherwise:
    y <- x$getMatrix() # use the matrix given
    x$setMatrix(y) # cache the matrix
    v <- solve(y, ...) # inverse the matrix
    x$setSolve(v) # cache the inverse
    v # return the inverse
    
        ## Return a matrix that is the inverse of 'x'
}
