## The calculation of the inverse of a matrix could be a computational
## expensive task. In this file we present two functions to improve the 
## computations of matrix inversions by using a cache approach.
 

makeCacheMatrix <- function(x = matrix()) {
 
  # This function creates a special "matrix" object that can cache its inverse. 
  # It is a list containing a function that
  #     
  # 1. Set the value of the matrix.
  # 2. Get the value of the matrix.
  # 3. Set the value of the inverse.
  # 4. Get the value of the inverse. 
  
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(inverse){
    inv <<- inverse
  }
 
  getinverse <- function(){
    inv
  }
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  
cacheSolve <- function(x, ...) {

  # Return a matrix that is the inverse of "x"
  
  inv <- x$getinverse()
  
  if (!is.null(inv)){
      message("Getting cached inverse matrix...")
      return(inv)
  }
  
  else{
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
}

  
  

