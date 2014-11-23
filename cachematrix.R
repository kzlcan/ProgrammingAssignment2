##	This function creates a special "matrix" object
##  that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      ## m will holdthe cahced unverse matrix
      
      m<-NULL
      
      ## matrix being set
      
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      
      ## matrix retrieved
      
      get<-function() x
      
      ## inverse matrix being set
      
      setmatrix<-function(solve) m<<- solve
     
	  ## inverse retrieved 	
     
      getmatrix<-function() m
      
      ## returned matrix with defined functions 
      
      list(set=set, get=get,
           setmatrix=setmatrix,
           getmatrix=getmatrix)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
      m<-x$getmatrix()
      
      ## if inverse has been already calculated , it returns it
      
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      
      ## The inverse  being calculated
    
      matrix<-x$get()
      m<-solve(matrix, ...)
      
      ## The inverse cached
      x$setmatrix(m)
      
      ## the invderse returned
      
      m
}
