## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{
  ## Initialize the inverse property
  m <- NULL
  ## Method used to set the matrix
  set <- function(y) 
  {
    x <<- y   
    m <<- NULL 
  }
  ## Method used to get the matrix
  get <- function() 
  {
    x
  }
  ## Method used to set the inverse of the matrix
  setInverse <- function(inverse)
  {
    m <<- inverse
  }
  ## Method used to get the inverse of the matrix
  getInverse <- function()
  {
    m
  }   
  ## Return list of the former 2 methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
## If inverse has already been calculated, then it should retrieve the inverse from the cache.
cacheSolve <- function(x)
{
  ## return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Return the inverse if it is already set
  if(!is.null(m)) 
  { 
    message("Attaining cached data")
    return(m)
  }
  ## Get the matrix from our object
  data <- x$get()  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) 
  ## Set the inverse to the object
  x$setInverse(m) 
  ## Return the matrix
  m                
}
