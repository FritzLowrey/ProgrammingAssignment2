## CacheMatrix.R - Programming assignment 2 for R programming.

## Cache a matrix or update the cached matrix if the new one differs from the
## cached version
makeCacheMatrix <- function(x = matrix()) {  
  
  set <- function(y = matrix())
  {
    cacheMatrix <<- y
    cacheInverse <<- NULL
  }  

  setInverse <- function(z = matrix())
  {
    cacheInverse <<- z
  }
  
  get <- function() 
  {
    if(exists("cacheMatrix"))
    {
      return(cacheMatrix)
    }
    else
    {
      return(NULL)
    }
  }
  
  getInverse <- function() 
  {
    if(exists("cacheInverse"))
    {
      return(cacheInverse)
    }
    else
    {
      return(NULL)
    }
  }
    
  list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}


##This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached <- makeCacheMatrix(NULL)
  
  thisMatrix <- cached$get()
  
  if(is.null(thisMatrix) | all(thisMatrix == x))
    cached$set(x)
  
  if(is.null(cached$getInverse()))
    cached$setInverse(solve(x))
  
  cached$getInverse()
}
