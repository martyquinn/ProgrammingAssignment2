## makeCacheMatrix allows one to store a matrix and retrieve its inversion which is calculated only once 
## the function accepts a matrix and delays calculation of the inversion until the user calls cacheSolve using 
## the returned makeCacheMatrix list. 
## The returned object list contains a series of functions and holds the original matrix and the 
## cached inverted matrix of it.
##    set (newMatrix) - use this to set a new matrix into this cached matrix object. 
##                     if the matrix is the same in terms of values 
##                     and dimensions then the inversion is not recalculated. 
##    get - gets the stored matrix. 
##    setinverse - accepts the solve(matrix) matrix and stores it as a cached inverted matrix. 
##    getinverse - retrieves the cached inversion matrix
## 

makeCacheMatrix <- function(storedMatrix = matrix()) {
  
    inversion <- NULL
    set <- function(newMatrix) {  
        
        if (!matequal(storedMatrix, newMatrix))
        {
            # we only want a new inversion if needed and it is necessary when the new matrix is not equal  (in values
            # and in dimensions) to the stored matrix. 
            inversion <<- NULL
        }
        storedMatrix <<- newMatrix  # store the new matrix no matter what in case it has other named qualities.
                                    # also the user expects his matrix will be saved into this structure. 
    }
    
    get <- function() storedMatrix
    setinverse <- function(inverseofmatrix) inversion <<- inverseofmatrix
    getinverse <- function() inversion
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the cached inverse of the matrix stored 
## inside the makeCacheMatrix object 'x'

cacheSolve <- function(x, ...) {
   
    inversion <- x$getinverse()
    
    if(!is.null(inversion)) {
        message("getting cached inverted matrix")
        return(inversion)
    }
    storedMatrix <- x$get()
    inversion <- solve(storedMatrix)
    x$setinverse(inversion)
    inversion
}

## test for equality of values between two matrices. 
## from Rui Barradas  https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y) 
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

