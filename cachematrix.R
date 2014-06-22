## This R script contains two functions:
##      1) makeCacheMatrix
##      2) cacheSolve
##
## The first function provides functionality to cache a matrix and its
## inverse.
##
## The second function uses the first function to provide the matrix and
## its inverse to the first function so that the first function can cache
## them. The second function calculates the inverse only if it doesn't 
## exist in the cache matix object.
##

## Object name: makeCacheMatrix
## Functions: set( matrix ), matrix := get(), setInverse( inverse( matrix )),
##            matirx_inverse := getInverse()
##
makeCacheMatrix <- function( cDataMatrix = matrix() ) {
        # Setting cMatrixInverse to NULL
        cMatrixInverse <- NULL
        
        # Setter function to set the data matrix for caching
        set <- function( matrixData ) {
            cDataMatrix <<- matrixData
            cMatixInverse <<- NULL
        }
        
        # Getter function return the data matrix
        get <- function() {
            cDataMatrix
        }
        
        # Setting the inverse of data matrix to cache
        setInverse <- function( inverse ) {
            cMatrixInverse <<- inverse
        }
        
        # Getter that returns the cached matrix inverse
        getInverse <- function() {
            cMatrixInverse
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    }    
}


##
## This function would use a cahced matrix object to cache
## the result of the inverse of a matrix. The inverse
## calculation is done by this function. It can be assumed
## that a matrix would be created, a cached matrix object
## would be created and the newly created matrix would be
## to the cached matrix object by a test program.
##  

cacheSolve <- function( mCachedMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInverse <- mCachedMatrix$getInverse()
    
    # If inverse already exist in the cached matrix object
    # then return the cahched inverse
    if ( ! is.null( mInverse ) ) {
        message( "Reading the inverse from cache....")
        return ( mInverse ) 
    }
    
    # If the inverse does not exist in the cahce,
    # calculate the inverse
    message( "Calculating inverse....")
    mInverse <- solve( mCachedMatrix$get() )
    
    # Set the inverse in the cached matrix
    mCachedMatrix$setInverse( mInverse )
    
    # Returns the calculated inverse
    mInverse
}
