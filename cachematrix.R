## cachematrix.R can create a special object that stores a matrix and caches its inverse

## This function creates a special "matrix" object that can cachs its inverse
## There are two variants in the function: x, m
## x is the data to deal with
## m saves the results

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(t) m <<- t
  get_inverse <- function() m
	
  ##return the results in a list
  list(
    set = set,
    get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  
  if (!is.null(m)){
    ## Transpose has been calculated
    message("Getting cached data")
    return(m)
  }
  
  ## Transpose has not been calculated
  data <- x$get()
  ## Then calculate the transpose of x
  m <- t(data)
  x$set_inverse(m)
  ## Return th results
  m
}
