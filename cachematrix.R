## This R script contains two functions:
##      1) makeCacheMatrix
##      2) cacheSolve
##
## The first function represents an object that allows caching of a 
## matrix and its inverse.
##
## The second function is a utility function that creates a makeCacheMatrix
## object. The function accepts a makeCacheMatrix, calls its method to see
## whether an inverse exists and if not, it performs the following tasks:
##      1. Read the matrix using the getter
##      2. Creates an inverse of the matrix
##      3. Set the inverse to the makeCaheMatrix object using the setter
##


## Object name: makeCacheMatrix
## Functions: set( matrix ), matrix := get(), setInverse( inverse( matrix )),
##            matirx_inverse := getInverse()
## 
## @params: cDataMatrix:matrix()
## @returns: list
makeCacheMatrix <- function( cDataMatrix = matrix() ) {
        # Initializer : Setting cMatrixInverse to NULL
        cMatrixInverse <- NULL
        
        # Setter function to set the data matrix for caching
        #
        # @params: matrixData:matrix()
        # @returns: void
        set <- function( matrixData ) {
            cDataMatrix <<- matrixData
            cMatixInverse <<- NULL
        }
        
        # Getter function return the data matrix
        #
        # @params: void
        # @returns: cDataMatrix:matrix()
        get <- function() {
            cDataMatrix
        }
        
        # Setting the inverse of data matrix to cache
        #
        # @params: inverse:matirx()
        # returns: void
        setInverse <- function( inverse ) {
            cMatrixInverse <<- inverse
        }
        
        # Getter that returns the cached matrix inverse
        #
        # @params: void
        # returns: cMatrixInverse:matrix()
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
