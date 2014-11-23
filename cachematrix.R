## Functions for calculating and caching the inverse of a matrix
#
#   makeCacheMatrix: Create a list object for caching the inverse of a square matrix
#   cacheSolve: Return the inverse of a matrix, using cached value if available
#
# Examples:
#
#   m <- matrix(1:4, 2, 2)
#   cm <- makeCacheMatrix(m)
#   cmInv1 <- cacheSolve(cm)
#   cmInv2 <- cacheSolve(cm)
#   cmInv1
#   cmInv2
#   cm$set(matrix(rnorm(9), 3, 3))
#   cm$get()
#   cmInv1 <- cacheSolve(cm)
#   cmInv2 <- cacheSolve(cm)
#   cmInv1
#   cmInv2


## makeCacheMatrix
#
# Description:
#
#   Create a list object for caching the inverse of a square matrix
#
# Arguments:
#
#   x: A numeric matrix
#
# Returns:
#
#   A list object for use with cacheSolve() containing the following values:
#
#     get():       Function returning the original matrix
#     set(y):      Function to update the original matrix
#     getinv():    Function returning the inverse matrix
#     setinv(inv): Function used by cacheSolve() to set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL           # Cached inverse matrix; computed and set by cacheSolve()
  get <- function() x       # Returns the value of the original matrix
  set <- function(y) {
    x <<- y                 # Update original matrix
    inverse <<- NULL        # Invalidate cached inverse matrix
  }

  setinv <- function(inv) {
    inverse <<- inv         # Cache inverse matrix; set by cacheSolve()
  }

  getinv <- function() inverse # Return cached inverse

  # Create and return list object of access functions
  list(
    get = get,
    set = set,
    getinv = getinv,
    setinv = setinv
  )
}


## cacheSolve
#
# Description:
#
#   Return the inverse of a matrix, using cached value if available
#
# Arguments:
#
#   x: Cached matrix object created by makeCacheMatrix()
#   ...: Additional arguments passed to solve()
#
# Returns:
#
#   The inverse of the cached matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()        # Return cached inverse from cacheMatrix object
  if(!is.null(inv)) {
    message("Using cached inverse")
    return(inv)            # Return cached inverse if previously calculated
  }
                           # No cached inverse available
  data <- x$get()          # Retrieve matrix data
  inv <- solve(data, ...)  # Solve matrix, passing ... arguments
  x$setinv(inv)            # Cache inverse in cacheMatrix object
  inv                      # Return inverse
}
