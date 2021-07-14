## ProgrammingAssignment2: Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialization  
    inverse <- NULL
  
  #Setting the matrix 
    set <- function(matrix){
        m <-- matrix
        inverse <<- NULL
  }
  #Getting the matrix
    get <- function(){
      m
    }
  # Setting the inverse of the matrix
    setInverse <- function(inv){
      inverse <<- inv
    }
    
  # Getting the inverse of the matrix
    getInverse <- function(){
      inverse
    }
  # List of methods
    list(set=set,get=get,
      setInverse = setInverse,
      getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    # If it has already been calculated:
    if(!is.null(m)){
      message("getting cached data")
      return (m)
    }
  
    # Getting the matrix from the object
    datMatrix <- x$get()
    
    # If it has not already been calculated: 
    # Calculate the inverse matrix by matrix multiplication
    m <- solve(datMatrix)%*%datMatrix
    
    # Setting the inverse to the object
    x$setInverse(m)
    
    # Return of the function: the matrix
    m
}
