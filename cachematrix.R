## CacheMatrix.R - Programming assignment 2 for R programming.

## Cache a matrix or update the cached matrix if the new one differs from the
## cached version
makeCacheMatrix <- function(x = matrix()) {  
  
  ## set the value of the matrix and NULL out the inverse
  set <- function(y = matrix())
  {
    cacheMatrix <<- y
    cacheInverse <<- NULL
  }  
  
  ## assign the inverse value from the calling function
  setInverse <- function(z = matrix())
  {
    cacheInverse <<- z
  }
  
  ## get the cached matrix or NULL if not assigned
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
  
  ## get the cached inverse matrix or NULL if not assigned
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
