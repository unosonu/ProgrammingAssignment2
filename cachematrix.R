## The two functions below are used to create special object that stores matrix vector
## and cache it's inverse

## The function makeCacheMatrix creates a special vector, which is a list containing functions to
## get the value of the vector
## set the value of the vector
## get the value of the inverse
## set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                                      ## The inverse of the matrix initialized with NULL
    
    set <- function(y) {
      x <<- y                                      
      i <<- NULL                                   
    }                                              ## Set the matrix 
    
    get <- function() x                            ## Retrieve the matrix
    setinverse <- function(inverse) i <<- inverse  ## Set the inverse to i
    getinverse <- function() i              ## Retrieve the inverse as i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The function cacheSolve calculates the inverse of the special vector created with the function makeCacheMatrix
## It first checks to see if the inverse has already been calculated. If so it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()     ## Retrieve the value of inverse to check if it has already been calculated.
        if(!is.null(i)){
            message("getting cached data")
            return(i)
        }
        data <- x$get()         ## Since the inverse is not calculated, retrieve the matrix to calculate the inverse.
        i <- solve(data, ...)      ## Calculation of inverse
        x$setinverse(i)         ## Setting the value of the inverse in the cache
        i                       
}
