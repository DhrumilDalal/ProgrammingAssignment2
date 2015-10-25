####################################################################################################################################################
## Caching is very effective and activelyused design pattern in programming. Operation that are considered expensive in terms
## of computation or network access are cached for better performance. 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  #initially the inverse is null.
  cached_inv <- NULL
  
  #This functin will store the matrix whoes inverse will be cached
  set <- function(y) 
  {
    #Store the new value
    x <<- y
    
    #Recycle the cacche because the new value is set
    cached_inv <<- NULL
  }
  
  
  #This function will return the stored matrix
  get <- function() 
  {
      x
  }
  
  
  #Cache the inverse
  setInverse <- function(computed_inv) 
  {
    cached_inv <<- computed_inv
  }
  
  
  #Returns the Cached inverse
  getInverse <- function() 
  {
    cached_inv
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}#End of makeCacheMatrix function
####################################################################################################################################################

####################################################################################################################################################
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Call GetInverse operation on  'x' to obtain the inverse. Here we simply call the getInverse operation to obtain the inverse. At this point in time
  ## we do not know if the inverse already exist. 
  inv <- x$getInverse()
  
  #If the inverse is not null then we can return the inverse object. This object is now returned using cache.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #If the inverse doesnot exist then we need to get the original matrix
  mat <- x$get()
  
  #Here we compute the inverse, which is usually very expensive 
  inv <- solve(mat, ...)
  
  #Now we set the inverse
  x$setInverse(inv)
  inv
}# End of cacheSolve function.
####################################################################################################################################################