## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
