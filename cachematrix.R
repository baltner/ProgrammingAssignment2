## This script contains a pair of functions that are useful for calculating the inverse of a matrix
## and storing it in cache to avoid recalculating it each time it is needed.


## makeCacheMatrix: This function creates a set of functions (list) that will be used by the
## cachesolver. The input is a matrix (assumed to be invertible) and the functions are:
## set: stores the matrix in memory (in the environment namespace)
## get: retrieves the matrix
## setInverse: stores the inverse of the matrixs in memory (in the environment namespace)
## getInverse: retrieves the inverse from memory


makeCacheMatrix <- function(x = matrix()) {
  
        inverse <- NULL
        
        #constructor for the imput matrix in this environment
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        #get the matrix
        get <- function() x
        
        
        #set the inverse
        setInverse <- function(inv) inverse <<- inv
        
        #get the inverse
        getInverse <- function() inverse
        
        ## return the list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the matrix created by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), cacheSolve retrieves
## the inverse from the cache. 

## The required input to this function is x, the list of functions returned by makeCacheMatrix
## However, it also may take a new matrix for those cases where it is desired to recalculate the inverse
## because the matrix has changed.  The code checks to see if that second argument has been passed and if so,
## compares it to the original matrix. The following scenarios apply:
##      The inverse is returned from cache if it exists there and the input matrix has not changed
##      The inverse is calculated if it does not already exist in the cache
##      The inverse is recalculated if the input matrix has changed (in this case the new matrix is also
##        stored in memory using the x$set function).


cacheSolve <- function(x, ...) {
       
        ## Check to see if a new matrix has been entered in the argument list.
        ## If so, compare it to the original. Do not recompute the inverse if it is unchanged.
        
        unchanged <- TRUE
        matrix <- x$get()
        newmatrix <- list(...)
        msg <- "The input matrix was not changed."
        
        len <- as.integer(length(newmatrix))
        if(len > 0L) {
                newmatrix <- newmatrix[[1]]
                unchanged <- identical(matrix,newmatrix)
                if(!unchanged) msg <- "The input matrix was changed."
        }
        
        ## Retrieve the inverse from cache if available and input matrix not changed, then return
        inverse <- x$getInverse()
        if(!is.null(inverse) && unchanged) {
                message(paste(msg,"Inverse retrieved from cache..."))
                return(inverse)
        }

        ## Calculate the inverse here because it is not cached OR the matrix has changed
        message(paste(msg,"Calculating the inverse."))
        if(!unchanged) {
                matrix <- newmatrix  # overwrite the previous matrix with the new one
                x$set(matrix)        # store it for later recall
        }
        inverse <- solve(matrix)
        x$setInverse(inverse)
        inverse
}
