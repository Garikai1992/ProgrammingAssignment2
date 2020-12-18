## This function is sort to save time when computing the inverse of matrix
## So what these two functions (makeCacheMatrix and cacheSolve ) do is to computes 
## the inverse of the special "matrix" 
## And checks if the inverse has already been calculated (and the matrix has not changed), 
# then return the inverse


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  my_inv<-NULL
  set<-function(t){
    x<<-t
    my_inv<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){my_inv<<-inverse}
  getInverse<-function(){my_inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  my_inv<-x$getInverse()
  if(!is.null(my_inv)){
    message("get cached data")
    return(my_inv)
  }
  my_matrix<-x$get()
  my_inv<-solve(my_matrix,....)
  x$setInverse(my_inv)
  my_inv
}
