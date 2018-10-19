    ## R Programming Assignment 2 Week 3: Caching the Inverse of a Matrix.
    
    ## Assignment consists in creating 2 functions that will benefit the costly computation of the inverse
    ## of a matrix by (1) creating a "special" matrix to cache the inverse of a matrix (instead of computing
    ## it repeatedly) and (2) computing the inverse of the special matrix created.
    
    ## 1. makeCacheMatrix: 
    ## This function creates a special "matrix" object that can cache its inverse.
    
        makeCacheMatrix <- function(x = matrix()) {
          #set the value of the matrix
          inv <- NULL
          set <- function(y) {
            x <<- y
            inv <<- NULL
          }
          
          #get the value of the matrix
          get <- function() x
          
          #set the value of the inverse
          setInverse <- function(inverse) {
              inv <<- inverse
          }
          
          #get the value of the inverse
          getInverse <- function() {
            inv
          }
          
          ## Return list of created "special" matrix function
          list(set = set,
               get = get,
               setInverse = setInverse,
               getInverse = getInverse)
        }
        
    
    ## 2. cacheSolve: computes the inverse of the special "matrix" created by makeCacheMatrix above. If the 
    ## inverse has already been calculated (and the matrix has not changed), then it should retrieve the 
    ## inverse from the cache.
    
        cacheSolve <- function(x, ...) {
          inv <- x$getInverse()
          
          #check if the inverse has already been created
          if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
          }
          
          #get the matrix
          mat <- x$get()
          #generate the inverse
          inv <- solve(mat, ...)
          #set the matrix as its inverse
          x$setInverse(inv)
          #return the inverse matrix
          inv
        }
