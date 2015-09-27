## Using the following two functions, inversion of a matrix is done once 
## and cached for subsequent call for inversion thereby computation
## for same inversion can be avoided, where applicable
## USE PATTERN: 
##             1. call makeCacheMatrix for the matrix whose inv to be cached
##             2. call cacheSolve for inverse of the matrix


## makeCacheMatrix is a special "matrix" object with functions 
##           to set/get the value of a matrix, and
##           to set/get the "inverse" of a matrix
##  INPUT: x - a square invertible matrix
##  OUTPUT: a list of the above functions (to get/set the matrix and it's inverse)
##  PURPOSE: the list is used by the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        invrse = NULL
        set = function(y) { #sets the matrix
                x <<- y  # use `<<-` to assign a value to an object in an environment 
                         # different from the current environment. 
                invrse <<- NULL
        }
        get = function() x #get the matrix
        setinv = function(inverse) invrse <<- inverse #set the inverse of the matrix
        getinv = function() invrse #get the inverse of the matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv) # list of the functions

}


## cacheSolve is a special "solve" (that does matrix inversion) function that
## invert a matrix only when it is not done yet, otherwise it uses the cached 
## inverse
## INPUT: object created by makeCacheMatrix
## OUTPUT: inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        # if the inverse has already been calculated, return that value
        # skip calculation
        if (!is.null(inv)){
                
                # message("getting cached data") #use for testing
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv) # and return it, voila!
}
