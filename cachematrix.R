## The makeCacheMatrix function outputs a list of functions to get,set data and 
## set and get inverse matrix and the cacheSolve set the inverse matrix and uses it from
## cache when run again

## creates a list to output with elements as functions to get/set data and
## get/set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setData <- function(y) {
     x <<- y
     invMatrix <<- NULL
  }
  getData <- function() x
  setInverse <- function(inverseM) 
    invMatrix <<- inverseM
  getInverse <- function() invMatrix
  list(setData=setData,getData=getData,setInverse=setInverse,getInverse=getInverse)
}


## calculates the inverse and stores in the cache and pulls the cached
## inverse from the second call onwards unless the underlying data is changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  dataM <- x$getData()
  Inv <- solve(dataM)
  x$setInverse(Inv)
  message("calculating inverse")
  Inv
  
}
