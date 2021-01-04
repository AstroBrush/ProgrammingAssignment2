## There are two functions here. The first one creates a matrix
## and its performs a cache of its inverse. The second one computes
## an inversion of said matrix by using the cache.

## creating the matrix on the basis of the example in the assignment

makeCacheMatrix <- function(x = matrix()) {
  
       mInv <- NULL
  
       set <- function(y) {
    
             x <<- y
    
            mInv <<- NULL
    
        }
  
       get <- function() x
  
       setinverse <- function(inv) mInv <<- inv
  
       getinverse <- function() mInv
  
       list(set = set, get = get,
             
                       setinverse = setinverse,
             
                       getinverse = getinverse)
   }


## Computing the inverse of the matrix that was just created

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  mInv <- x$getinverse()

      if(!is.null(mInv)) {
    
             message("getting cached data")
    
            return(mInv)
    
         }
  
       data <- x$get()
  
       mInv <- solve(data, ...)
  
       x$setinverse(mInv)
  
       mInv
   }
