# The two functions below together calculate he inverse of a matrix.  If the 
#   inverse has already been calculated, the inverse will be returned without
#   recalculating.

# This function creates other functions that are used when calculating/returning
#   the inverse of a matrix.  These formulas are used in the cacheSolve function 
#   below.

makeCacheMatrix <- function(x = numeric()) {
      # Set original value of the inverse matrix to NULL
      inv <- NULL  
      
      #     The set function from the original seems to have no purpose so I commented it out
      #      set <- function(y) {  # 
      #            z <<- y
      #            inv <<- NULL
      #      }
      
      # Store input argument so it can be retrieved by cacheSolve
      get <- function() x
      
      # Create a function that stores inverted matrix in the global environment
      setinv <- function(invrs) inv <<- invrs
      
      # Create a function that retrieves the value of the inverted matrix.
      #  Used by cacheSolve to assign current value of inverse (can be NULL)
      getinv <- function() inv
      
      # Create list of formulas that is returned to the global environment
      list(get=get,
           setinv = setinv,
           getinv = getinv)
}



# This function determines whether a inverse has already been calculated
# If calculated, it returns the value without doing the calculation again
# If not calculated, it calculates the inverse and returns the value

cacheSolve <- function(x, ...) {
      # Retrieve the value of the inverse matrix (NULL or already calculated)
      # Will look outside of function to global environment to find
      inv <- x$getinv()
      
      # Return value of inverse if it has already been calculated
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # If the inverse has not been calculated, get matrix to invert.  will find
      #  it in the global environment
      data <- x$get()
      
      # Invert the matrix
      inv <- solve(data, ...)
      
      # Execute function in global environment that sets value of inverse matrix
      #   to the calculated value
      x$setinv(inv)
      
      # Returns the value of the inverted matrix
      inv
      
}

